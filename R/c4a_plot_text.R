c4a_plot_text2 = function(cols = c("#0000FF", "#FF0000"), dark = FALSE) {
	grid::grid.newpage()
	bg = if (dark) "#000000" else "#FFFFFF"

	grid::grid.rect(gp=grid::gpar(fill = bg, col = NA))

	grid::pushViewport(grid::viewport(layout = grid::grid.layout(4, 4, heights = grid::unit(rep(1, 4), units = c("null", "lines", "null", "lines")), widths = grid::unit(rep(1, 4), units = c("null", "lines", "null", "lines")))))

	cellplot(1, 1, {
		grid::grid.rect(gp = grid::gpar(fill = cols[1], col = NA))
		grid::grid.text("Font size 14pt", gp = grid::gpar(col = cols[2], fontsize = 14))
	})
	cellplot(3, 1, {
		grid::grid.rect(gp = grid::gpar(fill = cols[2], col = NA))
		grid::grid.text("Font size 14pt", gp = grid::gpar(col = cols[1], fontsize = 14))
	})
	cellplot(1, 3, {
		grid::grid.rect(gp = grid::gpar(fill = cols[1], col = NA))
		grid::grid.text("Font size 18pt", gp = grid::gpar(col = cols[2], fontsize = 18))
	})
	cellplot(3, 3, {
		grid::grid.rect(gp = grid::gpar(fill = cols[2], col = NA))
		grid::grid.text("Font size 18pt", gp = grid::gpar(col = cols[1], fontsize = 18))
	})
}

c4a_plot_text = function(cols = c("#0000FF", "#FF0000"), words = NULL, size = 12, dark = TRUE, frame = FALSE, face = "plain") {
	grid::grid.newpage()

	bg = if (dark) "#000000" else "#FFFFFF"

	grid::grid.rect(gp=grid::gpar(fill = bg, col = NA))

	k = length(cols)

	if (is.null(words)) {
		words =  rep(paste0(LETTERS,letters), length.out = k)
	} else {
		words = rep(words, lenth.out = k)
	}

	dev = par("din")
	dasp = dev[1] / dev[2]

	m = which.min(sapply(1:5, function(cnr) {
		cnc = ceiling(k / cnr)
		casp = cnc / cnr
		b = ifelse(casp<dasp, dasp/casp, casp/dasp)
	}))

	n = ceiling(k/m)

	#m = (k - 1) %/% 8 + 1

	#n = min(k, 8)




	pasp = n / m

	if (dasp > pasp) {
		u = grid::convertHeight(grid::unit(1, "npc"), "lines") / m
		grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow = m, ncol = n + 1,
																	 widths = grid::unit(c(rep(u, n), 1), units = c(rep("lines", n), "null")),
																	 heights = grid::unit(rep(u, m), units = rep("lines", m)))))
	} else {
		u = grid::convertWidth(grid::unit(1, "npc"), "lines") / n
		grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow = m + 1, ncol = n,
																	 widths = grid::unit(rep(u, n), units = rep("lines")),
																	 heights = grid::unit(c(rep(u, m), 1), units = c(rep("lines", m), "null")))))
	}

	z = 0
	for (j in 1:m) {
		for (i in 1:n) {
			z = z + 1
			if (z <= k) {
				cellplot(j, i, {
					if (frame) {
						grid::grid.rect(width = 0.95, height = 0.95, gp = grid::gpar(col = NA, fill = cols[z]))
						grid::grid.text(words[z], gp = grid::gpar(col = bg, fontface = face, fontsize = size))#cex = u * size))
					} else {
						grid::grid.text(words[z], gp = grid::gpar(col = cols[z], fontface = face, fontsize = size))#cex = u * size))
					}
				})
			}
		}
	}
	grid::upViewport(1)

}


