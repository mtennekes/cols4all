#' Get the default contrast range
#'
#' Get the default contrast range for gradient palettes, so sequential `"seq"` and diverging `"div"`. The contrast range is a vector of two numbers between 0 and 1 that determine the range that is used for sequential and diverging palettes. The first number determines where the palette begins, and the second number where it ends. For sequential `"seq"` palettes, 0 means the leftmost (normally lightest) color, and 1 the rightmost (often darkest) color. For diverging `"seq"` palettes, 0 means the middle color, and 1 both extremes. If only one number is provided, this number is interpreted as the endpoint (with 0 taken as the start). This function sets the default based on the number of colors `n`: for small values of `n` the default contrast range is smaller than `c(0, 1)` for aesthetically more pleasing palettes. See `c4a_gui` how the contrast range effects the palettes. See also examples below. The contrast range can be set manually with the argument `contrast` of \code{\link{c4a}}.
#'
#' @param n number of colors
#' @param type type of color palette: one of `"seq"` (sequential palettes), `"div"` (diverging palettes), and `"cat"` (categorical/qualitative palettes).
#' @example ./examples/c4a_default_contrast.R
#' @export
c4a_default_contrast = function(n, type = c("seq", "div", "cat")) {
	type = match.arg(type)
	if (type == "cat") return(c(0,1))
	fun = paste0("default_contrast_", type)
	do.call(fun, list(k = n))
}


default_contrast_seq <- function(k) {
	c1 <- max((9-k) * (.15/6), 0)
	c2 <- min(.7 + (k-3) * (.3/6), 1)
	c(c1,c2)
}

default_contrast_div <- function(k) {
	c(0, min(.6 + (k-3) * (.4/8), 1))
}
