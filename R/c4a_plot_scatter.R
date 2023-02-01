c4a_plot_scatter = function(cols = NULL, col1 = "blue", col2 = "red", borders = "black", lwd = 0, dark = FALSE, dist = c("random", "concentric")) {
	dist = match.arg(dist)

	if (is.null(cols)) {
		cols = c(col1, col2)
	}

	n = length(cols)
	k = 100

	x = .C4A$rdata.scatter.x
	y = .C4A$rdata.scatter.y

	if (dist == "concentric") {
		d = order(x^2 + y^2)
		fill = cols[round(seq(1 - 0.5/n, n + 0.5/n, length.out = 100))][order(d)]
	} else {
		fill = rep(cols, length.out = 100)
	}



	if (lwd == 0) borders = NA

	if (is.na(borders)) {
	 	col = shp$gp$fill
	 	lwd = 0
	} else {
		col = borders
		lwd = lwd
	}

	grid::grid.newpage()

	bc = ifelse(dark, "#000000", "#FFFFFF")

	grid::grid.rect(gp=grid::gpar(fill = bc, col = NA))


	# if (dasp > 1) {
	# 	pushViewport(viewport(width = unit(1, "snpc"), height = unit(1/dasp, "snpc")))
	# } else {
	# 	pushViewport(viewport(width = unit(dasp, "snpc"), height = unit(1, "snpc")))
	# }

	grid::pushViewport(grid::viewport(xscale = c(-2, 2), yscale = c(-1, 1)))

	grid::grid.points(x = x, y = y, pch = 21, gp = grid::gpar(col = col, fill = fill, lwd = lwd))

}
