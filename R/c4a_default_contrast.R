#' Get the default range
#'
#' Get the default range for gradient palettes, so sequential `"seq"` and diverging `"div"`. The range is a vector of two numbers between 0 and 1 that determine the range that is used for sequential and diverging palettes. The first number determines where the palette begins, and the second number where it ends. For sequential `"seq"` palettes, 0 means the leftmost (normally lightest) color, and 1 the rightmost (often darkest) color. For diverging `"seq"` palettes, 0 means the middle color, and 1 both extremes. If only one number is provided, this number is interpreted as the endpoint (with 0 taken as the start). This function sets the default based on the number of colors `n`: for small values of `n` the default range is smaller than `c(0, 1)` for aesthetically more pleasing palettes. See `c4a_gui` how the range effects the palettes. See also examples below. The range can be set manually with the argument `range` of \code{\link{c4a}}.
#'
#' @param n number of colors
#' @param type type of color palette: one of `"seq"` (sequential palettes), `"div"` (diverging palettes), and `"cat"` (categorical/qualitative palettes).
#' @example ./examples/c4a_default_range.R
#' @export
c4a_default_range = function(n, type = c("seq", "div", "bivs", "bivc", "bivu", "cat")) {
	type = match.arg(type)
	fun = paste0("default_range_", type)
	do.call(fun, list(k = n))
}

default_range_cat <- function(k) {
	c(0, 1)
}

default_range_seq <- function(k) {
	if (k == 1) {
		c(0.5, 0.5)
	} else {
		if (k == 2) k = 3
		c(max(0, ((9 - k) / 6) * 0.2),
		  min(1, 1 - (((9 - k) / 6) * 0.2)))
	}
}

default_range_div <- function(k) {
	if (k == 1) {
		c(0, 0)
	} else {
		if (k == 2) k = 3
		c(0, min(1, 1 - (((11 - k) / 8) * 0.4)))
	}
}

default_range_bivs = function(k) {
	c(0, 1)
}

default_range_bivc = function(k) {
	c(0, 1)
}

default_range_bivu = function(k) {
	c(0, 1)
}
