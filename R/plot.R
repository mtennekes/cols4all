gridCell = function(rows, cols, e, ...) {
	vp = grid::viewport(layout.pos.row = rows, layout.pos.col = cols, ...)
	grid::grobTree(e, vp = vp)
}

plot_palette = function(cols, dark = FALSE, include.na = FALSE, nrows = NA, ncols = NA) {
	n = length(cols)
	grid::grid.newpage()

	if (is.na(nrows) && is.na(ncols)) {
		devsize = dev.size()
		dasp = devsize[1] / devsize[2]

		casp = n / (1:n)^2

		nrows = tail(which(casp-dasp > 0), 1)

		ncols = ceiling(n / nrows)
	} else if (!is.na(nrows) && is.na(ncols)) {
		ncols = ceiling(n / nrows)
	} else if (is.na(nrows) && !is.na(ncols)) {
		nrows = ceiling(n / ncols)
	} else {
		if (ncols * nrows < n) warning("nrows * ncols too low.", call. = FALSE)
		ncols = ceiling(n / nrows)
	}


	fc = ifelse(dark, "#FFFFFF", "#000000")
	bc = ifelse(dark, "#000000", "#FFFFFF")

	bg = grid::rectGrob(gp=grid::gpar(fill = bc, col = NA))

	nms = if (is.null(names(cols))) 1L:n else names(cols)

	vp = grid::viewport(layout = grid::grid.layout(nrow = 4 * nrows, ncol = ncols,
												   widths = grid::unit(rep(1/ncols, ncols), rep("null", ncols)),
												   heights = grid::unit(rep(c(.1,1, 1, .1), nrows), rep(c("lines", "lines", "null", "lines"), nrows))))

	cps = do.call(c, lapply(1:n, function(i) {
		rw = ((i-1) %/% ncols) + 1
		cl = i - (rw - 1) * ncols
		cp1 = gridCell(2 + (rw-1)*4, cl, {
			txt = if (i == n && include.na) "Missing" else nms[i]
			grid::textGrob(txt, gp = grid::gpar(col = fc))
		})
		cp2s = gridCell(3 + (rw-1)*4, cl, {
			grid::rectGrob(height = 1, gp=grid::gpar(fill = cols[i], col = bc, lwd = 2))
		})

		c(list(cp1), list(cp2s))
	}))
	grb = grid::grobTree(bg, do.call(grid::grobTree, c(cps, list(vp = vp))))
	grid::grid.draw(grb)
	invisible(grb)
}


plot_cvd = function(cols, dark = FALSE, include.na = FALSE) {
	grid::grid.newpage()

	fc = ifelse(dark, "#FFFFFF", "#000000")
	bc = ifelse(dark, "#000000", "#FFFFFF")

	bg = grid::rectGrob(gp=grid::gpar(fill = bc, col = NA))
	n = length(cols)

	nms = if (is.null(names(cols))) 1L:n else names(cols)

	cols_lst = c(list(cols), lapply(c("deutan", "protan", "tritan"), FUN = function(cvd) {
		do.call(eval(parse(text=paste0('colorspace::', cvd))), list(cols))
	}))

	vp = grid::viewport(layout = grid::grid.layout(nrow = 5, ncol = n + 1,
												   widths = grid::unit(c(3, rep(1/n, n)), c("lines", rep("null", n))),
												   heights = grid::unit(c(1, rep(4, 1/4)), c("lines", rep("null", 4)))))

	cps = do.call(c, lapply(2:(n+1), function(i) {
		cp1 = gridCell(1, i, {
			txt = if (i == (n+1) && include.na) "Missing" else nms[(i - 1)]
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
