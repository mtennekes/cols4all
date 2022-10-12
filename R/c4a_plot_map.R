c4a_plot_map = function(cols = NULL, col1 = "blue", col2 = "red", borders = "black", lwd = 0, crop = FALSE, dark = FALSE, dist = c("random", "gradient")) {
	dist = match.arg(dist)




	if (is.null(cols)) {
		cols = c(col1, col2)
	}

	n = length(cols)
	k = length(shp$gp$fill)


	shp$gp$fill = if (dist == "random") {
		rep(cols, length.out = k)
	} else {
		cols[round(seq(1 - 0.5/n, n + 0.5/n, length.out = k))]#[order(shp_c[, 1])]
	}


	if (crop) {
		xrange = bbx[3] - bbx[1]
		bbx[1] = bbx[1] + xrange * 0.6
		bbx[3] = bbx[3] - xrange * 0.2
	}

	if (lwd == 0) borders = NA
	if (is.na(borders)) {
		shp$gp$col = shp$gp$fill
	} else {
		shp$gp$col = borders
		shp$gp$lwd = lwd
	}

	sasp = (bbx[3] - bbx[1]) / (bbx[4] - bbx[2])
	dasp = dev.size()[1] / dev.size()[2]

	cy = (bbx[4] + bbx[2]) / 2
	cx = (bbx[3] + bbx[1]) / 2

	if (sasp > dasp) {
		h = (bbx[4] - bbx[2]) * (sasp / dasp)
		bbx[2] = cy - h/2
		bbx[4] = cy + h/2
	} else {
		w = (bbx[3] - bbx[1]) * (dasp / sasp)
		bbx[1] = cx - w/2
		bbx[3] = cx + w/2
	}

	grid::grid.newpage()

	bc = ifelse(dark, "#000000", "#FFFFFF")

	grid::grid.rect(gp=grid::gpar(fill = bc, col = NA))


	# if (dasp > 1) {
	# 	pushViewport(viewport(width = unit(1, "snpc"), height = unit(1/dasp, "snpc")))
	# } else {
	# 	pushViewport(viewport(width = unit(dasp, "snpc"), height = unit(1, "snpc")))
	# }

	grid::pushViewport(grid::viewport(xscale = bbx[c(1, 3)], yscale = bbx[c(2, 4)]))


	grid::grid.draw(shp)
}
