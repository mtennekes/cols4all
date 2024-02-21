#' Plot a color palette
#'
#' Plot a color palette, either a cols4all palette, or a color vector.
#'
#' @param palette Palette name (see \code{\link{c4a}}) or a color vector
#' @param ... arguments passed on to \code{\link{c4a}}
#' @param include.na should a color for missing values be included?
#' @param include.cvd should color deviciency simulated colors be included?
#' @return Besides the plot, a \code{\link[grid:gTree]{gTree}} is returned silently
#' @export
c4a_plot = function(palette, ..., include.na = FALSE, include.cvd = TRUE) {
	args = list(...)

	nms = names(palette)

	if (length(palette) == 1L) {
		pal = do.call(c4a, c(list(palette = palette), args))
		if (include.na) pal = c(pal, c4a_na(palette))
	} else {
		pal = validate_colors(palette, name = "palette")
	}

	if (is.null(pal)) return(invisible(NULL))

	pal = as.vector(pal)
	if (!is.null(nms)) {
		names(pal) = nms
	}

	if (include.cvd) {
		invisible(c4a_plot_cvd(pal, include.na = include.na))
	} else {
		invisible(c4a_plot_palette(pal, include.na = include.na))
	}
}
