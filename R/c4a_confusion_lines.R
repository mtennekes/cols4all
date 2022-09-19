xyY2XYZ = function(x, y, Y) {
	X = x * (Y / y)
	Z = (1-x-y) * (Y / y)
	XYZ(X, Y, Z)
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

cellplot = function (x, y, e) {
	grid::pushViewport(grid::viewport(layout.pos.row = x, layout.pos.col = y, clip = TRUE))
	e
	grid::upViewport()
}


plot_rgb = function(cvd = c("none", "deutan", "protan", "tritan"), confusion_lines = TRUE, colors = NULL) {
	cvd = match.arg(cvd)

	toM = function(x, nr) {
		m = matrix(x, nrow = nr, byrow = TRUE)
		m[nr:1,]
	}

	cols = if (cvd == "none") {
		toM(rgb_data$cols, rgb_data$res[1])
	} else if (cvd == "deutan") {
		toM(colorspace::deutan(rgb_data$cols), rgb_data$res[1])
	} else if (cvd == "protan")  {
		toM(colorspace::protan(rgb_data$cols), rgb_data$res[1])
	} else {
		toM(colorspace::tritan(rgb_data$cols), rgb_data$res[1])
	}

	grid::grid.rect()
	grid::grid.raster(cols)

	if (confusion_lines) {
		a1 = head(seq(0, 2 * pi, length.out = 100), -1)
		a2 = head(seq(0, 2 * pi, length.out = 200), -1)

		ind = (rbind(rep(1, 3), diag(3)) == TRUE)[match(cvd, c("none", "deutan", "protan", "tritan")), ]

		coords = list(c(0.747, 0.253), c(1.4, -0.4), c(0.171, 0))[ind]

		alist = list(a1, a2, a1)[ind]

		coords_scale = lapply(coords, function(co) {
			c(rescale(co[1], from = rgb_data$xrange, to = c(0, 1)),
			  rescale(co[2], from = rgb_data$yrange, to = c(0, 1)))
		})

		mapply(function(co, a) {
			for (ai in a) {
				grid.lines(c(co[1], co[1] + 5 * cos(ai)),
						   c(co[2], co[2] + 5 * sin(ai)), gp=gpar(col="#000000"))
			}
		}, coords_scale, alist)
	}

	if (!is.null(colors)) {
		co = hex2xyY(colors)
		x = rescale(co[,1], from = rgb_data$xrange, to = c(0, 1))
		y = rescale(co[,2], from = rgb_data$yrange, to = c(0, 1))
		grid.points(x, y)
	}


}

c4a_confusion_lines = function(cols = NULL) {
	grid.newpage()
	grid::pushViewport(viewport(width = unit(1, "snpc"), height = unit(1, "snpc"), clip = TRUE))
	grid::pushViewport(viewport(layout = grid::grid.layout(2, 2)))

	cellplot(1, 1, {
		plot_rgb(cvd = "none", colors = cols, confusion_lines = FALSE)
	})
	cellplot(1, 2, {
		plot_rgb(cvd = "deutan", colors = cols)
		grid.rect(x = 0.85, y = 0.95, width = 0.25, height = unit(1, "lines"), gp = gpar(col = NA, fill = "#FFFFFF"))
		grid.text("Deutan", x = 0.95, y = 0.95, just = c("right", "center"))
	})
	cellplot(2, 1, {
		plot_rgb(cvd = "protan", colors = cols)
		grid.rect(x = 0.85, y = 0.95, width = 0.25, height = unit(1, "lines"), gp = gpar(col = NA, fill = "#FFFFFF"))
		grid.text("Protan", x = 0.95, y = 0.95, just = c("right", "center"))
	})
	cellplot(2, 2, {
		plot_rgb(cvd = "tritan", colors = cols)
		grid.rect(x = 0.85, y = 0.95, width = 0.25, height = unit(1, "lines"), gp = gpar(col = NA, fill = "#FFFFFF"))
		grid.text("Tritan", x = 0.95, y = 0.95, just = c("right", "center"))
	})
}


