#' List all available cols4all color palettes and series
#'
#' `c4a_palettes` lists all available cols4all color palettes. The used naming convention of the palettes is: series.palette_name. Palettes are also accessible (with \code{\link{c4a}})by just palette_name. `c4a_series` lists all series from which palettes are available. `c4a_default_palette` returns the one default (recommended) palette per type and `c4a_default_contrast` returns the default contrast for sequential and diverging palette (see argument `contrast` of \code{\link{c4a}}).
#'
#' @section Source / Credits:
#'
#' \strong{misc}
#'
#' * `r3` and `r4` base R colors, from the `grDevices` package
#' * `ggplot2` from the `ggplot2` package (also included in `grDevices`)
#' * `okabe` by Okabe-Ito (2002, revised 2008), Color Universal Design (CUD) - How to make figures and presentations that are friendly to Colorblind people -, \url{https://jfly.uni-koeln.de/color/} (also included in `grDevices`)
#' * `alphabet` from the `Polychroma` package (also included in `grDevices`)
#' * `alphabet2` by P.Green-Armytage (2010): A Colour Alphabet and the Limits of Colour Coding. Colour: Design & Creativity (5) (2010): 10, 1-23. www.aic-color.org/journal/v5/jaic_v5_06.pdf
#' * `polychrome36` from the `Polychrome` package (also included in `grDevices`)
#' * `kelly` by K. Kelly (1965): Twenty-two colors of maximum contrast. Color Eng., 3(6), 1965. http://www.iscc.org/pdf/PC54_1724_001.pdf (from the `pals` package)
#' * `watlington` John Watlington. An Optimum 16 Color Palette. \url{http://alumni.media.mit.edu/~wad/color/palette.html} (from the `pals` package)
#' * `cols25`by K. Wright \url{https://stackoverflow.com/a/9568659/1393348} (from the `pals` package)
#' * `glasbey`by Chris Glasbey, Gerie van der Heijden, Vivian F. K. Toh, Alision Gray (2007). Colour Displays for Categorical Images. Color Research and Application, 32, 304-309. (from the `pals` package)
#'
#' \strong{hcl}
#'
#' From the `colorspace` package, citation:
#'
#' Zeileis A, Fisher JC, Hornik K, Ihaka R, McWhite CD, Murrell P, Stauffer R, Wilke CO (2020). colorspace: A Toolbox for Manipulating and Assessing Colors and Palettes. Journal of Statistical Software, 96 (1), 1-49.
#'
#' \strong{brewer}
#'
#' From the `RColorBrewer` package, by Cynthia Brewer:
#'
#' Harrower, Mark, and Cynthia A. Brewer. "ColorBrewer.org: an online tool for selecting colour schemes for maps." The Cartographic Journal 40.1 (2003): 27-37.
#'
#' \strong{tol}
#'
#' Color schemes by Paul Tol. \url{https://personal.sron.nl/~pault/}
#'
#' \strong{viridis}
#'
#' \strong{kovesi}
#'
#' \strong{wes}
#'
#' \strong{carto}
#'
#' \strong{scico}
#'
#' \strong{tableau}
#'
#' @param type type of color palette: one of `"all"` (all palettes), `"cat"` (categorical/qualitative palettes), `"seq"` (sequential palettes) and `"div"` (diverging palettes).
#' @param series series to list the palettes from. Run `c4a_series` to see the options.
#' @param full.names should full names, i.e. with the prefix "series."? By default `TRUE`.
#' @param n number of colors
#' @return names of the loaded color palettes
#' @example ./examples/c4a_palettes.R
#' @rdname c4a_palettes
#' @name c4a_palettes
#' @export
c4a_palettes = function(type = c("all", "cat", "seq", "div"), series = NULL, full.names = TRUE) {
	type = match.arg(type)
	z = .C4A$z
	fnames = z$fullname
	sel_type = if (type != "all") z$type == type else TRUE
	sel_series = if (is.null(series)) TRUE else (z$series %in% series)
	fnames[sel_type & sel_series]
}

#' @rdname c4a_palettes
#' @name c4a_series
#' @export
c4a_series = function(type = c("all", "cat", "seq", "div")) {
	type = match.arg(type)
	z = .C4A$z
	series = z$series
	unique({if (type != "all") series[z$type == type] else series})
}

#' @rdname c4a_palettes
#' @name c4a_default_palette
#' @export
c4a_default_palette = function(type = c("cat", "seq", "div")) {
	type = match.arg(type)
	.C4A$defaults[type]
}


#' @rdname c4a_palettes
#' @name c4a_default_contrast
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

