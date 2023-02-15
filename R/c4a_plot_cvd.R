gridCell = function(rows, cols, e, ...) {
	vp = grid::viewport(layout.pos.row = rows, layout.pos.col = cols, ...)
	grid::grobTree(e, vp = vp)
}

c4a_plot_cvd = function(cols, dark = FALSE, include.na = FALSE) {
	grid::grid.newpage()

	fc = ifelse(dark, "#FFFFFF", "#000000")
	bc = ifelse(dark, "#000000", "#FFFFFF")

	bg = grid::rectGrob(gp=grid::gpar(fill = bc, col = NA))
	n = length(cols)

	cols_lst = c(list(cols), lapply(c("deutan", "protan", "tritan"), FUN = function(cvd) {
		do.call(eval(parse(text=paste0('colorspace::', cvd))), list(cols))
	}))

	vp = grid::viewport(layout = grid::grid.layout(nrow = 5, ncol = n + 1,
												   widths = grid::unit(c(3, rep(1/n, n)), c("lines", rep("null", n))),
												   heights = grid::unit(c(1, rep(4, 1/4)), c("lines", rep("null", 4)))))

	cps = do.call(c, lapply(2:(n+1), function(i) {
		cp1 = gridCell(1, i, {
			txt = if (i == (n+1) && include.na) "Missing" else (i - 1)
			grid::textGrob(txt, gp = grid::gpar(col = fc))
		})
		cp2s = lapply(2:5, function(j) {
			gridCell(j, i, {
				grid::rectGrob(height = 0.8, gp=grid::gpar(fill = cols_lst[[j-1]][i-1], col = bc, lwd = 2))
			})
		})
		if (i == 2) {
			cp3s = lapply(2:5, function(j) {
				gridCell(j, 1, {
					grid::textGrob(x = 0.95, c("Normal", "Deutan", "Protan", "Tritan")[j-1], just = "right",
								   gp = grid::gpar(col = fc))
				})
			})
		} else {
			cp3s = NULL
		}
		c(list(cp1), cp2s, cp3s)
	}))
	grb = grid::grobTree(bg, do.call(grid::grobTree, c(cps, list(vp = vp))))
	grid::grid.draw(grb)
	invisible(grb)
}
