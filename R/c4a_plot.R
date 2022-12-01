#' Plot a color palette
#'
#' Plot a color palette, either a cols4all palette, or a color vector.
#'
#' @param palette Palette name (see \code{\link{c4a}}) or a color vector
#' @param ... arguments passed on to \code{\link{c4a}}
#' @export
c4a_plot = function(palette, ...) {
	args = list(...)

	if (length(palette) == 1L) {
		pal = do.call(c4a, c(list(palette = palette), args))
	} else {
		pal = palette
	}

	c4a_plot_cvd(pal)
}
