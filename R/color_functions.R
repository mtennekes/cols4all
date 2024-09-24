
colors_sort = function(x, by = c("H", "C", "L", "CRW", "CRB")) {
	by = match.arg(by)
	crw = colorspace::contrast_ratio("white", x)
	hcl = get_hcl_matrix(x)
	df = cbind(as.data.frame(hcl), CRW = crw, CRB = 21/crw)
	x[order(df[[by]])]
}

colors_filter = function(x, Hmin = 0, Hmax = 360, Cmin = 0, Cmax = Inf, Lmin = 0, Lmax = 100, CRWmin = 1, CRWmax = 21, CRBmin = 1, CRBmax = 21) {
	crw = colorspace::contrast_ratio("white", x)
	hcl = get_hcl_matrix(x)
	df = cbind(as.data.frame(hcl), CRW = crw, CRB = 21/crw)
	ids = which(df$H >= Hmin & df$H <= Hmax & df$C >= Cmin & df$C <= Cmax & df$L >= Lmin & df$L <= Lmax & df$CRW >= CRWmin & df$CRW <= CRWmax & df$CRB >= CRBmin & df$CRB <= CRBmax)
	x[ids]
}

colors_name = function(x, label = c("i", "H", "C", "L", "CRW", "CRB")) {
	label = match.arg(label)
	crw = colorspace::contrast_ratio("white", x)
	hcl = get_hcl_matrix(x)
	df = cbind(as.data.frame(hcl), CRW = crw, CRB = 21/crw, i = 1L:length(x))
	structure(x, names = round(df[[label]], 2))
}

get_dist_matrices = function(p) {
	#norm = cols4all:::get_dist_matrix(p, cvd = "none")

	res = lapply(c("protan", "deutan", "tritan", "none"), function(cvd) {
		m = cols4all:::get_dist_matrix(p, cvd = cvd)
		m
	})
	names(res) = c("protan", "deutan", "tritan", "normal")
	res
}

colors_cbf_set_plot = function(df, pal, columns = 2, cex = 1) {
	k = ncol(df) - 2
	n = nrow(df)

	nrows = ceiling(n / columns)

	grid::grid.newpage()
	grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow = nrows * 2, ncol = columns * k + (columns - 1))))

	col = 1
	row = 1
	for (i in 1:n) {
		for (j in 1:k) {
			grid::pushViewport(grid::viewport(layout.pos.col = col + (j-1), layout.pos.row = row))
			grid::grid.rect(width = 0.9, height = 0.9, gp=grid::gpar(fill = pal[df[[j]][i]]))
			grid::upViewport()
		}
		grid::pushViewport(grid::viewport(layout.pos.col = col:(col+k-1), layout.pos.row = row+1))
		grid::grid.text(y= .7, just = "bottom", paste0("Delta E: ", sprintf("%0.2f", df[[k+1]][i]), " (", df[[k+2]][i], ")"), gp = grid::gpar(cex = cex))
		grid::upViewport()
		if (row == (nrows * 2 - 1))  {
			col = col + (k + 1)
			row = 1
		} else {
			row = row + 2
		}
	}
}

colors_order = function(x, head = 1, weight_normal = 0) {
	n = length(x)

	m = get_dist_matrices(x)
	mcvd = do.call(pmin, m[1:3])
	mnorm = m$normal

	m2 = mcvd * (1-weight_normal) + mnorm * weight_normal

	m2[lower.tri(m2)] = t(m2)[lower.tri(t(m2))]
	upper.tri(m2)

	ids = head
	todo = setdiff(1L:n, head)

	while(length(todo) > 0) {
		j = todo[which.max(apply(m2[ids, todo, drop = FALSE], MARGIN = 2, FUN = min))]
		ids = c(ids, j)
		todo = setdiff(todo, j)
	}
	x[ids]
}




colors_cbf_set = function(x, k = NA, option_list = NULL, plot = TRUE, dE_min = 10, columns = 2, cex = 1, required = NULL, top = 20, parallelize = NA, ncores = 4, batch.size = 2e7, max.size = 100e7, step = -1, dir = "temp", init = NULL) {
	# step -1: initialize only (return init)
	# step 0: all
	# step 1, ... batch process
	# step Inf, only combining results

	if (step < 1 || is.null(init)) {
		if (!is.list(required) && !is.null(required)) required = list(required)

		stopifnot(!(is.na(k) && is.null(option_list)))


		message("Step 0a: Initialize: calculating combinations")
		n = length(x)

		if (!is.null(option_list)) {
			option_list = lapply(option_list, as.integer)
			y = unname(t(do.call(expand.grid, option_list)))
			ncomb = ncol(y)
		} else {
			ncomb = choose(n,k)
		}

		if (ncomb >= max.size) stop("Number of combinations is ", formatC(ncomb, format = "fg", big.mark = ","))

		message("Number of combinations: ", formatC(ncomb, format = "fg", big.mark = ","))

		if (is.null(option_list)) {
			y = combn(1:n, k)

			message("Starting now...")

			if (!is.null(required)) {
				sel = apply(y, MARGIN = 2, function(x) {
					all(vapply(required, function(req) {
						any(req %in% x)
					}, FUN.VALUE = logical(1)))
				})
				message("After filtering by required colors ", ncol(y), " combinations are left")
				y = y[, sel]

			}
		}

		ny = ncol(y)

		##
		message("Step 0b: Create batch files")

		start = seq(1, ny + batch.size, by = batch.size)
		end = start[-1] - 1
		start = head(start, -1)
		if (end[length(end)] != ny) end[length(end)] = ny

		bn = length(start)

		if (!dir.exists(dir)) dir.create(dir)

		fls = paste0(dir, "/k", k, "_batch_", 1L:bn, ".rds")

		if (!all(file.exists(fls))) {
			for (i in 1L:bn) {
				ysel = y[, start[i]:end[i]]
				saveRDS(ysel, file = fls[i])
			}
		}

		rm(y)
		gc()

		if (step == -1) {
			return(list(k = k,
						x = x,
						ny = ny,
						start = start,
						end = end,
						dE_min = dE_min,
						bn = bn,
						fls = fls))
		}
	} else {
		k = init$k
		x = init$x
		ny = init$ny
		start = init$start
		end = init$end
		bn = init$bn
		dE_min = init$dE_min
		fls = init$fls
	}

	if (is.na(parallelize)) {
		parallelize = ny >= 1e7
	}


	step = max(step, 1)

	if (step != Inf) {
		ms = get_dist_matrices(x)

		if (parallelize) {
			message("In parallel")
			ncores = if (is.na(ncores)) parallel::detectCores() else ncores
			cl = parallel::makeCluster(ncores)
			on.exit(parallel::stopCluster(cl))
		}

		dfs = lapply(step:bn, function(s) {
			message("Step ", s)
			y = readRDS(fls[s])
			if (parallelize) {
				res = parallel::parApply(cl = cl, X = y, MARGIN = 2, function(yi) {
					mins = vapply(ms, function(m) {
						min(m[yi,yi], na.rm = TRUE)
					}, FUN.VALUE = numeric(1))
					which.min(mins) * 1000 + min(mins)
				})
			} else {
				res = pbapply::pbapply(y, MARGIN = 2, function(yi) {
					mins = vapply(ms, function(m) {
						min(m[yi,yi], na.rm = TRUE)
					}, FUN.VALUE = numeric(1))
					which.min(mins) * 1000 + min(mins)
				})

			}
			whichType = res %/% 1000
			dE = res %% 1000

			ids = which(dE > dE_min)

			if (!length(ids)) {
				message("maximum Delta E value is ", max(dE))
				ids = which(dE >= floor(max(dE)))
			}
			df = cbind(as.data.frame(t(y[,ids])), dist = dE[ids], type = names(ms)[whichType[ids]])
			saveRDS(df, paste0(dir, "/k", k, "_df_", s, ".rds"))
			df
		})
		if (step > 1) {
			dfs0 = lapply(1:(step-1), function(s) {
				readRDS(paste0(dir, "/k", k, "_df_", s, ".rds"))
			})
			dfs = rbind(dfs0, dfs)
		}
	} else {
		dfs = lapply(step:bn, function(s) {
			readRDS(paste0(dir, "/k", k, "_df_", s, ".rds"))
		})
	}


	df = do.call(rbind, dfs)

	df2 = df[order(df$dist, decreasing = TRUE), ]

	message("Complete")

	if (plot) {
		if (nrow(df2) > top) {
			message(nrow(df2), " palettes found. Plotting only the top ", top)
			df3 = df2[1:top,]
		} else {
			df3 = df2
		}
		colors_cbf_set_plot(df3, x, columns = columns, cex = cex)
	}
	pals = get_palettes(df2, x)

	z = list(pool = x,
			 palettes = pals,
			 dE = df2$dist,
			 dE_type = df2$type)
	invisible(z)
}

colors_remove_twins = function(x, th = 2, include.cvd = FALSE) {
	n = length(x)

	if (include.cvd) {
		m = cols4all:::get_dist_matrices(x)
		d = do.call(pmin, m)
	} else {
		d = cols4all:::get_dist_matrix(x)

	}

	d[lower.tri(d)] = NA
	ids = which(d < th)
	rids = ((ids - 1) %/% n) + 1
	cids = ((ids - 1) %% n) + 1
	keeps = setdiff(cids, rids)
	rem = setdiff(c(rids,cids), keeps)
	remain = setdiff(1L:n, rem)
	x[remain]
}


get_ids = function(df, k) {
	lapply(1:nrow(df), function(i) {
		unlist(df[i, 1:k])
	})
}

get_palettes = function(df, p) {
	p = unname(p)
	k = length(which(substr(names(df), 1, 1) == "V"))
	ids = get_ids(df, k = k)
	pals = lapply(ids, function(x) p[x])
	# pals = lapply(pals, function(pal) {
	# 	H = get_hcl_matrix(pal)
	# 	pal[order(H[,1])]
	# })
	#names(pals) = paste0("pal", 1:length(pals))
	pals
}
