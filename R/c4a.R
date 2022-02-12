#' List all available cols4all color palettes
c4a_palettes = function(type = c("all", "cat", "seq", "div", "biv", "cyc")) {
	type = match.arg(type)
	names(z_cat) # todo: make it work for other types
}

c4a_defaults = c(cat = "tol.muted", seq = "hcl.blues", div = "hcl.purple-green")

#' Select a cols4all color palette
#'
#' Select a cols4all color palette
#'
#' @param palette name of the palette. See \code{\link{c4a_palettes}} for options
#' @param n
#'
c4a = function(palette = NULL, n = NULL, type = c("cat", "seq", "div", "biv", "cyc"), reverse = FALSE, order = NULL) {
	type = match.arg(type)

	if (is.null(palette)) palette = c4a_defaults[type]

	if (!(palette %in% names(z_cat))) stop("Unknown palette. See c4a_names() for options, and c4a_show / c4a_gui to see them.")
	zi = z_cat[[palette]]
	index = attr(zi, "index")

	pal = if (is.null(index)) {
		if (!is.null(n)) {
			if (length(zi) < n) stop("Palette ", palette, " only supports ", length(zi), " colors.")
			zi[1L:n]
		} else {
			n = length(zi)
			zi
		}
	} else {
		ns = as.character(names(index))
		if (!is.null(n)) {
			if (!n %in% ns) {
				stop("Palette ", palette, " only supports ", min(ns), " to ", max(ns), " colors.")
			}
			zi[index[[as.character(n)]]]
		} else {
			zi[index[[length(index)]]]
		}
	}
	pal = if (!is.null(order)) {
		if (!all(order %in% 1L:n)) stop("order should consist of numbers 1 to ", n)
		pal[order]
	} else pal
	if (reverse) rev(pal) else pal
}
