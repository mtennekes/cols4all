get_CR_matrix = function(p) {
	n = length(p)

	m = do.call(rbind, lapply(1:n, function(i) {
		sapply(p, function(pj) colorspace::contrast_ratio(p[i], pj))
	}))
	diag(m) = NA

	m
}

get_dist_matrix = function(p, cvd = c("none", "deutan", "protan", "tritan"), whole_matrix = FALSE) {
	cvd = match.arg(cvd)
	m = if (cvd == "none") {
		colorblindcheck::palette_dist(p)
	} else {
		colorblindcheck::palette_dist(p, cvd = substr(cvd, 1, 3))
	}
	if (whole_matrix) {
		m[lower.tri(m)] = t(m)[lower.tri(m)]
	}
	m
}



sim_cvd = function(pal, cvd = c("none", "deutan", "protan", "tritan")) {
	cvd = match.arg(cvd)
	switch(cvd,
	       none = function(x) x,
		   deutan = colorspace::deutan,
		   protan = colorspace::protan,
		   tritan = colorspace::tritan)(pal)
}

c4a_plot_dist_matrix = function(p, id1 = NULL, id2 = NULL, cvd = "none", dark = FALSE, title = "Similarity") {
	plot_matrix(p = p, id1 = id1, id2 = id2, type = "dist", cvd = cvd, dark = dark, title = title)
}

c4a_plot_CR_matrix = function(p, id1 = NULL, id2 = NULL, cvd = "none", dark = FALSE, title = "Contrast ratio") {
	plot_matrix(p = p, id1 = id1, id2 = id2, type = "CR", cvd = cvd, dark = dark, title = title)
}

plot_matrix = function(p, id1 = NULL, id2 = NULL, type = c("CR", "dist"), cvd = "none", dark = FALSE, annotation = FALSE, title = "Contrast ratio") {
	n = length(p)
	type = match.arg(type)

	pchs = .C4A$matrix_pchs[[type]]
	sizes = .C4A$matrix_sizes[[type]]
	texts = .C4A$matrix_interval_labels[[type]]


	m = if (type == "CR") {
		get_CR_matrix(p)
	} else {
		get_dist_matrix(p, cvd = cvd, whole_matrix = TRUE)
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

	fc = ifelse(dark, "#FFFFFF", "#000000")
	bc = ifelse(dark, "#000000", "#FFFFFF")

	grid::grid.rect(gp=grid::gpar(fill = bc, col = NA))


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
			grid::grid.lines(x = c(0,1), y = c(1, 0), gp = grid::gpar(lwd = 2, col = fc))
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
#if (v>2 && v < 4.5) browser()
				s = symbol_size(v, type)

				cellplot(i+1,j+1, {
					grid::grid.points(x = 0.5, y = 0.5, pch = pchs[s], size = grid::unit(sizes[s], units = "lines"), gp = grid::gpar(col = fc))
					if (!is.null(id1) && !is.null(id2) && id1[1] == i && id2[1] == j) {
						grid::grid.circle(r = 0.4, gp = grid::gpar(fill = NA, col = fc, lwd = 1.5, lty = "dotted"))
					}
				})

			}
		}
		grid::upViewport(2)
	})
	cellplot(1, 2, {
		#grid::grid.rect(gp=grid::gpar(fill="gold", lwd =4))
		grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow = length(texts) + 3L,
																	 ncol = 5,
																	 widths = grid::unit(c(0.25, 1, 0.25, 1, 1), units = c("lines", "lines", "lines", "null", "lines")),
																	 heights = grid::unit(rep(1, length(texts)+2L), units = c(rep("lines", length(texts)+2L), "null")))))


		cellplot(2, 2, {
			grid::grid.text(title, x = 0, just = "left")
		})
		for (i in 1:length(texts)) {
			cellplot(2 + i, 2, {
				grid::grid.points(x = 0.5, y = 0.5, pch = pchs[i], size = grid::unit(sizes[i], units = "lines"), gp = grid::gpar(col = fc))
			})
			cellplot(2 + i, 4, {
				grid::grid.text(texts[i], x = 0, just = "left", gp = grid::gpar(col = fc, cex = 0.9))
			})
		}
		grid::upViewport()
		# #grid::grid.rect(gp=grid::gpar(fill="yellow"))

	})
	grid::upViewport(2)

}

symbol_size = function(cr, type) {
	brks = .C4A$matrix_breaks[[type]]
	#brks_digits = .C4A$matrix_breaks_digits[type]
	#cr = round(cr, brks_digits)
	id = which(cr <= brks[-1])[1]
	if (is.na(id)) id = length(brks)
	id
}


