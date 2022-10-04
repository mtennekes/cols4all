c4a_CL_plot = function(cols, Lrange = FALSE) {
	grid::grid.newpage()

	grid::pushViewport(grid::viewport(width = grid::unit(1, "snpc"), height = grid::unit(1, "snpc"), clip = TRUE))
	grid::pushViewport(grid::viewport(layout = grid::grid.layout(3, 3,
																 widths = grid::unit(c(2,1,2), c("lines", "null", "lines")),
																 heights = grid::unit(c(2,1,2), c("lines", "null", "lines")))))

	sq = 2
	marg = 1.5

	m = get_hcl_matrix(cols)
	cr = range(m[, 2])
	lr = range(m[, 3])

	cellplot(2,2, {
		grid::grid.rect(gp = grid::gpar(fill = "#EEEEEE"))



		cr2 = cr + c(-marg, marg) * 1.8
		lr2 = lr + c(-marg, marg)



		grid::grid.rect(x = grid::unit(mean(cr2), "native"), y = grid::unit(mean(lr2), "native"),
						width = grid::unit(diff(cr2), "native"), height = grid::unit(diff(lr2), "native"),
						gp = grid::gpar(fill = "#FFFFFF", col = NA))

		grid::grid.lines(x = grid::unit(rep(.C4A$Cpastel, 2), "native"), gp = grid::gpar(lty = 2))
		grid::grid.lines(x = grid::unit(rep(.C4A$Cintense, 2), "native"), gp = grid::gpar(lty = 2))


		grid::grid.polyline(x = grid::unit(c(cr[1], cr[1], cr[1], cr[2], cr[2], cr[2]), "native"),
							y = grid::unit(c(lr2[1] - marg * 0.5, lr2[1] - marg * 1.5, lr2[1] - marg, lr2[1] - marg, lr2[1] - marg * 0.5, lr2[1] - marg * 1.5), "native"),
							id = c(1, 1, 2, 2, 3, 3), gp = grid::gpar(col = "#555555"))

		grid::grid.rect(x = grid::unit(mean(cr2), "native"), y = grid::unit(lr2[1] - marg * 2.75, "native"),
						width = grid::unit(diff(cr2), "native"), height = grid::unit(marg * 2, "native"),
						gp = grid::gpar(fill = "#EEEEEE", col = NA))

		ctext = if (diff(cr) <= .C4A$CrangeHarm) {
			"Low chroma range"
		} else if (diff(cr) >= .C4A$CrangeDisH) {
			"High chroma range"
		} else {
			"Medium chroma range"
		}
		grid::grid.text(ctext, x = grid::unit(mean(cr2), "native"), y = grid::unit(lr2[1] - marg * 2.5, "native"), gp = grid::gpar(cex = 0.8))


		if (Lrange) {
			grid::grid.polyline(x = grid::unit(cr2[2] + c(marg * 0.5, marg * 1.5, marg, marg, marg * 0.5, marg * 1.5)  * 1.8, "native"),
								y = grid::unit(c(lr[1], lr[1], lr[1], lr[2], lr[2], lr[2]), "native"),
								id = c(1, 1, 2, 2, 3, 3), gp = grid::gpar(col = "#555555"))

			ltext = if (diff(lr) <= .C4A$LrangeHarm) {
				"Low luminance range"
			} else if (diff(lr) >= .C4A$LrangeDisH) {
				"High luminance range"
			} else {
				"Medium luminance range"
			}
			grid::grid.text(ltext, x = grid::unit(cr2[2] + marg * 2.5 * 1.8, "native"), y = grid::unit(mean(lr2), "native"), rot = 90, gp = grid::gpar(cex = 0.8))
		}






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
		grid::grid.text(c("Low chroma", "High chroma"), x = grid::unit(c(.C4A$Cpastel / 2, c(.C4A$Cintense + 180) / 2), "native"), y = c(.3, .3), gp = grid::gpar(cex = c(1, 1)))
	}, xscale = c(0, 180))

}

