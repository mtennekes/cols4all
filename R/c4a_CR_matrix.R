get_CR_matrix = function(p) {
	n = length(p)

	m = do.call(rbind, lapply(1:n, function(i) {
		sapply(p, function(pj) colorspace::contrast_ratio(p[i], pj))
	}))
	diag(m) = NA

	m
}

get_dist_matrix = function(p, cvd = c("none", "deutan", "protan", "tritan")) {
	cvd = match.arg(cvd)
	if (cvd == "none") {
		colorblindcheck::palette_dist(p)
	} else {
		colorblindcheck::palette_dist(p, cvd = substr(cvd, 1, 3))
	}
}



sim_cvd = function(pal, cvd = c("none", "deutan", "protan", "tritan")) {
	cvd = match.arg(cvd)
	switch(cvd,
	       none = function(x) x,
		   deutan = colorspace::deutan,
		   protan = colorspace::protan,
		   tritan = colorspace::tritan)(pal)
}


c4a_CR_matrix = function(p, id1 = NULL, id2 = NULL, type = c("CR", "dist"), cvd = "none") {
	n = length(p)
	type = match.arg(type)

	m = if (type == "CR") {
		get_CR_matrix(p)
	} else {
		get_dist_matrix(p, cvd = cvd)
	}

	p = sim_cvd(p, cvd)


	#m[lower.tri(m)] = NA


	cex = min(1, 12 / n)

	cellplot = function(rw, cl, e) {
		grid::pushViewport(grid::viewport(layout.pos.row = rw, layout.pos.col = cl))
		e
		grid::upViewport()
	}

	grid::grid.newpage()
	#grid::grid.rect(gp=grid::gpar(fill="grey80"))
	din = par("din")
	cr = din[1] / din[2]
	if (cr > 1.25) {
		grid::pushViewport(grid::viewport(width = grid::unit(1.25, "snpc"), height = grid::unit(1, "snpc"), gp = grid::gpar(cex = cex)))
	} else {
		grid::pushViewport(grid::viewport(width = grid::unit(1, "snpc"), height = grid::unit(0.8, "snpc"), gp = grid::gpar(cex = cex)))
	}
	#grid::grid.rect(gp=grid::gpar(fill="blue"))

	grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow = 1, ncol = 2, widths = c(0.8, 0.2))))

	cellplot(1, 1, {
		#grid::grid.rect(gp=grid::gpar(fill="grey70"))
		grid::pushViewport(grid::viewport(width = 0.9, height = 0.9))
		#grid::grid.rect(gp=grid::gpar(fill="grey60"))
		grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow = n + 1, ncol = n + 1)))

		cellplot(2:(n+1), 2:(n+1), {
			grid::grid.lines(x = c(0,1), y = c(1, 0), gp = grid::gpar(lwd = 2))
		})


		for (i in 1:n) {
			cellplot(i+1, 1, {
				grid::grid.rect(width = 0.9, height = 0.9, gp=grid::gpar(fill = p[i]))
				if (!is.null(id1) && i == id1) {
					grid::grid.circle(r = 0.17, gp = grid::gpar(fill = "#FFFFFF", col = NA))
					grid::grid.circle(r = 0.1, gp = grid::gpar(fill = NA, col = "#000000", lwd = 2))
				}
			})
			cellplot(1, i+1, {
				grid::grid.rect(width = 0.9, height = 0.9, gp=grid::gpar(fill = p[i]))
				if (!is.null(id2) && i == id2) {
					grid::grid.circle(r = 0.17, gp = grid::gpar(fill = "#FFFFFF", col = NA))
					grid::grid.circle(r = 0.1, gp = grid::gpar(fill = NA, col = "#000000", lwd = 2))
				}
			})
			for (j in 1:n) {
				v = m[i,j]
				if (is.na(v)) next

				s = symbol_size(v, type)

				cellplot(i+1,j+1, {
					grid::grid.points(x = 0.5, y = 0.5, pch = c(15, 17, 16, 16)[s], size = grid::unit(c(1, 0.6, 0.3, 0)[s], units = "lines"))
					if (!is.null(id1) && !is.null(id2) && id1 == i && id2 == j) {
						grid::grid.circle(r = 0.4, gp = grid::gpar(fill = NA, col = "#000000", lwd = 1.5, lty = "dotted"))
					}
				})

			}
		}
		grid::upViewport(2)
	})
	cellplot(1, 2, {
		#grid::grid.rect(gp=grid::gpar(fill="gold", lwd =4))
		grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow = 6, ncol = 5,
																	 widths = grid::unit(c(0.25, 1, 0.25, 1, 1), units = c("lines", "lines", "lines", "null", "lines")),
																	 heights = grid::unit(c(1, 1, 1, 1, 1, 1), units = c(rep("lines", 5), "null")))))

		pchs = c(15, 17, 16)
		sizes = c(1, 0.6, 0.3)
		texts = symbol_text(type)

		# cellplot(2, 2, {
		# 	grid::grid.text("Contrast Ratio", x = 0, just = "left")
		# })
		for (i in 1:3) {
			cellplot(2 + i, 2, {
				grid::grid.points(x = 0.5, y = 0.5, pch = pchs[i], size = grid::unit(sizes[i], units = "lines"))
			})
			cellplot(2 + i, 4, {
				grid::grid.text(texts[i], x = 0, just = "left")
			})
		}
		grid::upViewport()
		# #grid::grid.rect(gp=grid::gpar(fill="yellow"))

	})

}

symbol_size = function(cr, type) {
	brks = .C4A$breaks[[type]]
	brks_digits = .C4A$breaks_digits[type]
	cr = round(cr, brks_digits)
	ifelse(cr <= brks[2], 1, ifelse(cr < brks[3], 2, ifelse(cr < brks[4], 3, 4)))
}


symbol_text = function(type) {
	.C4A$interval_labels[[type]]

	# brks = .C4A$breaks[[type]]
	# brks_digits = .C4A$breaks_digits[type]
	# f = formatC(brks, digits = brks_digits, format = "f")
	# mapply(function(a, b) {
	# 	paste(a, "to", b)
	# }, head(f, -1), f[-1])
}
