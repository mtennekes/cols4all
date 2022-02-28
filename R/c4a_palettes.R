#' List all available cols4all color palettes and series
#'
#' `c4a_palettes` lists all available cols4all color palettes. Palettes are organized by series. The available series are listed with `c4a_series`. The references are provided below. Palettes are also organized per functional type, where we currently support: categorical `"cat"`, sequential `"seq"`, and diverging `"div"`" palette types. The function `c4a_default_palette` returns the default (recommended) palette per type.
#'
#' @section References:
#'
#' \strong{misc}
#'
#' * `r3` and `r4` base R colors, from the `grDevices` package
#' * `ggplot2` from the `ggplot2` package (also included in `grDevices`)
#' * `okabe` by Okabe-Ito (2002, revised 2008), Color Universal Design (CUD) - How to make figures and presentations that are friendly to Colorblind people -, \url{https://jfly.uni-koeln.de/color/} (also included in `grDevices`)
#' * `alphabet` from the `Polychroma` package (also included in `grDevices`)
#' * `alphabet2` by P.Green-Armytage (2010): A Colour Alphabet and the Limits of Colour Coding. Colour: Design & Creativity (5) (2010): 10, 1-23. www.aic-color.org/journal/v5/jaic_v5_06.pdf
#' * `polychrome36` from the `Polychrome` package (also included in `grDevices`)
#' * `kelly` by K. Kelly (1965): Twenty-two colors of maximum contrast. Color Eng., 3(6), 1965. http://www.iscc.org/pdf/PC54_1724_001.pdf (obtained from the `pals` package)
#' * `watlington` by John Watlington. \url{http://alumni.media.mit.edu/~wad/color/palette.html} (obtained from the `pals` package)
#' * `cols25` by K. Wright \url{https://stackoverflow.com/a/9568659/1393348} (obtained from the `pals` package)
#' * `glasbey` by Chris Glasbey, Gerie van der Heijden, Vivian F. K. Toh, Alision Gray (2007). Colour Displays for Categorical Images. Color Research and Application, 32, 304-309. (from the `pals` package)
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
#' Color schemes used in the Python library matplotlib:
#'
#' J. D. Hunter, "Matplotlib: A 2D Graphics Environment", Computing in Science & Engineering, vol. 9, no. 3, pp. 90-95, 2007.
#'
#' In R obtained via the package `viridisLite`
#'
#' \strong{kovesi}
#'
#' Color schemes by Peter Kovesi:
#'
#' Peter Kovesi (2016). CET Perceptually Uniform Colour Maps. \url{https://colorcet.com/}
#' Peter Kovesi (2015). Good Colour Maps: How to Design Them. Arxiv. \url{https://arxiv.org/abs/1509.03700}
#'
#' Obtained via the R package `pals` (by Kevin Wright)
#'
#' \strong{wes}
#'
#' Palettes from Wes Anderson movies
#'
#' Obtained via the package `wesanderson`
#'
#' \strong{carto}
#'
#' Palettes by CARTO.
#'
#' Obtained via the package `rcartocolors`
#'
#' \strong{scico}
#'
#' Palettes developed by Fabio Crameri
#'
#' \url{http://www.fabiocrameri.ch/colourmaps.php}
#'
#' Crameri, Fabio. (2018, May 8). Scientific colour maps (Version 3.0.1). Zenodo.
#' Crameri, Fabio. (2018). Geodynamic diagnostics, scientific visualisation and StagLab 3.0. Geosci. Model Dev. Discuss.
#'
#' Obtained via the R package `scico`
#'
#' \strong{tableau}
#'
#' Palettes by Tableau Software
#'
#' Obtained via the R package `ggthemes`
#'
#' @param type type of color palette: one of `"all"` (all palettes), `"cat"` (categorical/qualitative palettes), `"seq"` (sequential palettes) and `"div"` (diverging palettes).
#' @param series series to list the palettes from. Run `c4a_series` to see the options.
#' @param full.names should full names, i.e. with the prefix "series."? By default `TRUE`.
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
