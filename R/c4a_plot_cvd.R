c4a_plot_cvd = function(cols, dark = FALSE, annotation = FALSE) {
	grid::grid.newpage()

	fc = ifelse(dark, "#FFFFFF", "#000000")
	bc = ifelse(dark, "#000000", "#FFFFFF")

	grid::grid.rect(gp=grid::gpar(fill = bc, col = NA))
	n = length(cols)

	cols_lst = c(list(cols), lapply(c("deutan", "protan", "tritan"), FUN = function(cvd) {
		do.call(eval(parse(text=paste0('colorspace::', cvd))), list(cols))
	}))

	grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow = 5, ncol = n + 1,
																 widths = grid::unit(c(3, rep(1/n, n)), c("lines", rep("null", n))),
																 heights = grid::unit(c(1, rep(4, 1/4)), c("lines", rep("null", 4))))))

	for (i in 2:(n+1)) {
		cellplot(1, i, {
			grid::grid.text(i-1, gp = grid::gpar(col = fc))
		})
		for (j in 2:5) cellplot(j, i, {
			grid::grid.rect(height = 0.8, gp=grid::gpar(fill = cols_lst[[j-1]][i-1], col = bc, lwd = 2))
		})
		if (i == 2) {
			for (j in 2:5) cellplot(j, 1, {
				grid::grid.text(x = 0.95, c("Normal", "Deutan", "Protan", "Tritan")[j-1], just = "right",
								gp = grid::gpar(col = fc))
			})
		}
	}
	grid::upViewport()

	if (annotation) {
		p = png::readPNG(system.file("img/cvd.png", package = "cols4all"))
		if (dark) {
			p[,,1:3] = 1 - p[,,1:3]
		}
		r = grDevices::as.raster(p)

		grid::grid.raster(r)
	}

}
