xyY2XYZ = function(x, y, Y) {
	X = x * (Y / y)
	Z = (1-x-y) * (Y / y)
	XYZ(X, Y, Z)
}

XYZ2xyY = function(X, Y, Z) {
	x = X / (X+Y+Z)
	y = Y / (X+Y+Z)
	c(x=x, y=y, Y=Y)
}

hex2xyY = function(cols) {
	co = coords(as(hex2RGB(cols), "XYZ"))
	x = co[,1] / rowSums(co)
	y = co[,2] / rowSums(co)
	Y = co[,3]
	cbind(x, y, Y)
}

rescale = function(x, from = c(0, 1), to = c(0, 1)) {
	(x - from[1])/diff(from) * diff(to) + to[1]
}

cellplot = function (r, c, e, ...) {
	grid::pushViewport(grid::viewport(layout.pos.row = r, layout.pos.col = c, clip = TRUE, ...))
	e
	grid::upViewport()
}


plot_rgb = function(cvd = c("none", "deutan", "protan", "tritan"), confusion_lines = TRUE, colors = NULL, white = TRUE, dark = FALSE, L = "L100") {
	grid::grid.newpage()

	grid::pushViewport(grid::viewport(width = grid::unit(1, "snpc"), height = grid::unit(1, "snpc"), clip = TRUE))

	fc = ifelse(dark, "#FFFFFF", "#000000")
	bc = ifelse(dark, "#000000", "#FFFFFF")

	grid::grid.rect(gp=grid::gpar(fill = bc, col = NA))


	cvd = match.arg(cvd)

	fc = ifelse(dark, "#FFFFFF", "#000000")
	bc = ifelse(dark, "#000000", "#FFFFFF")

	toM = function(x, nr) {
		m = matrix(x, nrow = nr, byrow = TRUE)
		m[nr:1,]
	}

	#rgb_data$cols[rgb_data$cols == "#FFFFFF"] = NA
	cols = rgb_data$cols_list[[L]]
	colsM = toM(sim_cvd(cols, cvd), rgb_data$res[1])

	grid::grid.raster(colsM)

	w = hex2xyY("#FFFFFF")
	x0 = rescale(w[,1], from = rgb_data$xrange, to = c(0, 1))
	y0 = rescale(w[,2], from = rgb_data$yrange, to = c(0, 1))

	if (confusion_lines) {
		a0 = head(seq(0, 2 * pi, length.out = 30), -1)
		a1 = head(seq(0, 2 * pi, length.out = 100), -1)
		a2 = head(seq(0, 2 * pi, length.out = 200), -1)

		#ind = (rbind(rep(1, 3), diag(3)) == TRUE)[match(cvd, c("none", "deutan", "protan", "tritan")), ]
		ind = match(cvd, c("none", "deutan", "protan", "tritan"))

		co = list(w[,1:2], c(0.747, 0.253), c(1.4, -0.4), c(0.171, 0))[[ind]]

		a = list(a0, a1, a1, a2, a1)[[ind]]

		co2 = c(rescale(co[1], from = rgb_data$xrange, to = c(0, 1)),
					 rescale(co[2], from = rgb_data$yrange, to = c(0, 1)))

		for (ai in a) {
			if (cvd == "none") {
				grid::grid.lines(c(co2[1] + .025 * cos(ai), co2[1] + 5 * cos(ai)),
								 c(co2[2] + .025 * sin(ai), co2[2] + 5 * sin(ai)), gp=grid::gpar(col="#555555"))
			} else {
				grid::grid.lines(c(co2[1], co2[1] + 5 * cos(ai)),
								 c(co2[2], co2[2] + 5 * sin(ai)), gp=grid::gpar(col="#000000"))
			}
		}
	}

	if (!is.null(colors)) {
		co = hex2xyY(colors)
		x = rescale(co[,1], from = rgb_data$xrange, to = c(0, 1))
		y = rescale(co[,2], from = rgb_data$yrange, to = c(0, 1))
		grid::grid.points(x, y, size = grid::unit(.5, "lines"))
		grid::grid.text(label = 1:length(colors), x = grid::unit(x, "npc") - grid::unit(0.6, "lines"), y = grid::unit(y, "npc") + grid::unit(0.6, "lines"))
	}

	if (white) {
		grid::grid.points(x0, y0, size = grid::unit(.5, "lines"), pch = 3)

	}

}

c4a_plot_rgb_space = function(cols = NULL, cvd = "none", dark = FALSE, L = "L100") {
	plot_rgb(cvd = cvd, colors = cols, confusion_lines = FALSE, dark = dark, L = L)
}

c4a_plot_confusion_lines = function(cols = NULL, cvd = "none", dark = FALSE, L = "L100") {
	plot_rgb(cvd = cvd, colors = cols, confusion_lines = TRUE, dark = dark, L = L)
}
