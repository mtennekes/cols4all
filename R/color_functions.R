
colors_sort = function(x, by = c("H", "C", "L", "CR")) {
	by = match.arg(by)
	cr = colorspace::contrast_ratio("white", x)
	hcl = get_hcl_matrix(x)
	df = cbind(as.data.frame(hcl), CR = cr)
	x[order(df[[by]])]
}

colors_filter = function(x, Hmin = 0, Hmax = 360, Cmin = 0, Cmax = Inf, Lmin = 0, Lmax = 100, CRmin = 1, CRmax = 21) {
	cr = colorspace::contrast_ratio("white", x)
	hcl = get_hcl_matrix(x)
	df = cbind(as.data.frame(hcl), CR = cr)
	ids = which(df$H >= Hmin & df$H <= Hmax & df$C >= Cmin & df$C <= Cmax & df$L >= Lmin & df$L <= Lmax & df$CR >= CRmin & df$CR <= CRmax)
	x[ids]
}

colors_name = function(x, label = c("H", "C", "L", "CR")) {
	label = match.arg(label)
	cr = colorspace::contrast_ratio("white", x)
	hcl = get_hcl_matrix(x)
	df = cbind(as.data.frame(hcl), CR = cr)
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

colors_cbf_set = function(x, k, plot = TRUE, dE_min = 10, columns = 2, cex = 1) {
	ms = get_dist_matrices(x, th = dE_min)
	n = length(x)
	y = combn(1:n, k)

	res = apply(y, MARGIN = 2, function(yi) {
		mins = sapply(ms, function(m) {
			min(m[yi,yi], na.rm = TRUE)
		})
		which.min(mins) * 1000 + min(mins)
	})
	whichType = res %/% 1000
	dE = res %% 1000

	ids = which(dE > dE_min)

	if (!length(ids)) {
		message("maximum Delta E value is ", max(dE))
		return(NULL)
	}

	df = cbind(as.data.frame(t(y[,ids])), dist = dE[ids], type = names(ms)[whichType[ids]])
	df2 = df[order(df$dist, decreasing = TRUE), ]

	if (plot) {
		if (nrow(df2) > 20) {
			message(nrow(df2), " palettes found. Plotting only the top 20")
			df3 = df2[1:20,]
		} else {
			df3 = df2
		}
		colors_cbf_set_plot(df3, x, columns = columns, cex = cex)
	}
	pals = get_palettes(df2, x)

	y = list(pool = x,
			 palettes = pals,
			 dE = df2$dist,
			 dE_type = df2$type)
	invisible(y)
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
	pals = lapply(pals, function(pal) {
		H = get_hcl_matrix(pal)
		pal[order(H[,1])]
	})
	#names(pals) = paste0("pal", 1:length(pals))
	pals
}
