c4a_CL_plot = function(cols) {
	grid::grid.newpage()

	grid::pushViewport(grid::viewport(width = grid::unit(1, "snpc"), height = grid::unit(1, "snpc"), clip = TRUE))
	grid::pushViewport(grid::viewport(layout = grid::grid.layout(3, 3,
																 widths = grid::unit(c(2,1,2), c("lines", "null", "lines")),
																 heights = grid::unit(c(2,1,2), c("lines", "null", "lines")))))

	sq = 2
	marg = 1.5

	cellplot(2,2, {
		grid::grid.rect(gp = grid::gpar(fill = "#EEEEEE"))

		m = get_hcl_matrix(cols)

		cr = range(m[,2]) + c(-marg, marg) * 1.8
		lr = range(m[,3]) + c(-marg, marg)



		grid::grid.rect(x = grid::unit(mean(cr), "native"), y = grid::unit(mean(lr), "native"),
						width = grid::unit(diff(cr), "native"), height = grid::unit(diff(lr), "native"),
						gp = grid::gpar(fill = "#FFFFFF", col = NA))


		grid::grid.lines(x = grid::unit(rep(.C4A$Cpastel, 2), "native"), gp = grid::gpar(lty = 2))
		grid::grid.lines(x = grid::unit(rep(.C4A$Cintense, 2), "native"), gp = grid::gpar(lty = 2))

		#grid::grid.points(m[,2], m[,3], pch = 15, gp = grid::gpar(col = cols))
		grid::grid.rect(grid::unit(m[,2], "native"), grid::unit(m[,3], "native"), width = grid::unit(sq * 1.8, "native"), height = grid::unit(sq, "native"), gp = grid::gpar(col = NA, fill = cols))
	}, yscale = c(0,100), xscale = c(0,180))

	cellplot(2, 1, {
		s = seq(10, 90, by = 10)
		k = length(s)
		grid::grid.polyline(rep(c(0.85, 1), k), grid::unit(rep(s, each  = 2), "native"), id = rep(1:k, each = 2))
		grid::grid.text(s, rep(0.75, k), grid::unit(s, "native"), just = "right", gp = grid::gpar(cex = 0.8))
		grid::grid.text("Luminance", rot = 90, x = 0.2)
	}, yscale = c(0,100))

	cellplot(3, 2, {
		s = seq(20, 160, by = 20)
		k = length(s)
		grid::grid.polyline(grid::unit(rep(s, each  = 2), "native"), rep(c(0.85, 1), k), id = rep(1:k, each = 2))
		grid::grid.text(s, grid::unit(s, "native"), rep(0.65, k), just = "center", gp = grid::gpar(cex = 0.8))
		grid::grid.text("Chroma", y = .2)
	}, xscale = c(0,180))


	cellplot(1, 2, {
		grid::grid.text(c("*", "■-■"), x = grid::unit(c(.C4A$Cpastel / 2, c(.C4A$Cintense + 180) / 2), "native"), y = c(.3, .5), gp = grid::gpar(cex = c(2, 1.5)))
	}, xscale = c(0, 180))

}

