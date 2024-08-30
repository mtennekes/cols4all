
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

get_dist_matrices = function(p, th = 15) {
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

colors_cbf_set = function(x, k, plot = TRUE, dE_min = 10, columns = 2, cex = 1, required = NULL, top = 20, parallelize = NA) {
	if (!is.list(required) && !is.null(required)) required = list(required)

	ms = get_dist_matrices(x, th = dE_min)
	n = length(x)

	ncomb = choose(n,k)

	message("There are ", ncomb, " combinations to check")

	if (is.na(parallelize)) {
		parallelize = ncomb > 1e7
	}

	y = combn(1:n, k)

	message("Starting now...")

	if (parallelize) {
		message("In parallel")
		ncores = parallel::detectCores()
		cl = parallel::makeCluster(ncores)
		on.exit(parallel::stopCluster(cl))
	}

	if (!is.null(required)) {
		sel = apply(y, MARGIN = 2, function(x) {
			all(vapply(required, function(req) {
				any(req %in% x)
			}, FUN.VALUE = logical(1)))
		})
		message("After filtering by required colors ", ncol(y2), " combinations are left")
		y2 = y[, sel]
	} else {
		y2 = y
	}


	if (parallelize) {
		res = parallel::parApply(cl = cl, X = y2, MARGIN = 2, function(yi) {
			mins = vapply(ms, function(m) {
				min(m[yi,yi], na.rm = TRUE)
			}, FUN.VALUE = numeric(1))
			which.min(mins) * 1000 + min(mins)
		})
	} else {
		res = pbapply::pbapply(y2, MARGIN = 2, function(yi) {
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

	df = cbind(as.data.frame(t(y2[,ids])), dist = dE[ids], type = names(ms)[whichType[ids]])
	df2 = df[order(df$dist, decreasing = TRUE), ]

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
