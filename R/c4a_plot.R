#' Plot a color palette
#'
#' Plot a color palette, either a cols4all palette, or a color vector. `c4a_plot_cvd` is a shortcut to include color-blind simulated colors, `c4a_plot_hex is a shortcut to print hex codes instead of labels.
#'
#' @param palette Palette name (see \code{\link{c4a}}) or a color vector
#' @param ... arguments passed on to \code{\link{c4a}}
#' @param dark dark (black) background?
#' @param include.na should a color for missing values be included?
#' @param hex should hex codes be printed instead of color labels (or numbers)?
#' @param include.cvd should color deficiency simulated colors be included?
#' @param nrows,ncols Number of rows and columns. Ignored if `include.cvd = TRUE` (in that case, rows are used for the simulated colors). By default automatically calculated based on aspect ratio of the device.
#' @return Besides the plot, a \code{\link[grid:gTree]{gTree}} is returned silently
#' @export
#' @name c4a_plot
#' @rdname c4a_plot
#' @example ./examples/c4a_plot.R
c4a_plot = function(palette, ..., dark = FALSE, include.na = FALSE, hex = FALSE, include.cvd = FALSE, nrows = NA, ncols = NA) {
	args = list(...)

	nms = names(palette)

	if (length(palette) == 1L) {
		pal = do.call(c4a, c(list(palette = palette, verbose = FALSE), args))
		if (is.null(pal)) {
			pal = validate_colors(palette, name = "palette")
		} else {
			nms = NULL
			if (is.matrix(pal)) {
				ncols = ncol(pal)
				pal = t(pal)
			}
			if (include.na) pal = c(pal, c4a_na(palette))
		}
	} else {
		pal = validate_colors(palette, name = "palette")
	}

	if (hex) {
		nms = unname(pal)
		names(pal) = nms
	}



	if (is.null(pal)) return(invisible(NULL))

	pal = as.vector(pal)
	if (!is.null(nms)) {
		names(pal) = nms
	}

	if (include.cvd) {
		invisible(plot_cvd(pal, dark = dark, include.na = include.na))
	} else {
		invisible(plot_palette(pal, dark = dark, include.na = include.na, nrows = nrows, ncols = ncols))
	}
}

#' @export
#' @name c4a_plot_cvd
#' @rdname c4a_plot
c4a_plot_cvd = function(...) {
	args = list(...)
	args$include.cvd = TRUE
	do.call(c4a_plot, args)
}

#' @export
#' @name c4a_plot_hex
#' @rdname c4a_plot
c4a_plot_hex = function(...) {
	args = list(...)
	args$hex = TRUE
	do.call(c4a_plot, args)
}
