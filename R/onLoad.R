#' cols4all overview
#'
#' Color palettes for all people, including those with color vision deficiency. Palettes, organized by source and type, are scored on several properties such as color-blind-friendliness and harmony. Furthermore each palette is assigned a distinct color for missing values. Own palettes can be loaded as well. ggplot2 scales are included.
#'
#' This page provides a brief overview of all package functions.
#'
#' @section Main functions:
#' \tabular{ll}{
#' \code{\link{c4a_gui}}\tab GUI (shiny app) to analyse the palettes \cr
#' \code{\link{c4a}}\tab Get the colors of a palette \cr
#' }
#'
#' @section Palette names and properties:
#' \tabular{ll}{
#' \code{\link{c4a_palettes}}\tab Get available palette names \cr
#' \code{\link{c4a_series}}\tab Get available series \cr
#' \code{\link{c4a_meta}}\tab Get meta information (such as type and maximum number of colors) \cr
#' \code{\link{.P}}\tab Environment via which palette names can be browsed with auto-completion (using `$`) \cr
#' }
#'
#' @section Importing and exporting palettes:
#' \tabular{ll}{
#' \code{\link{c4a_series_add}}\tab Add color palettes \cr
#' \code{\link{c4a_series_remove}}\tab Remove color palettes \cr
#' \code{\link{c4a_sysdata_import}}\tab Import system data \cr
#' \code{\link{c4a_sysdata_export}}\tab Export system data \cr
#' }
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
#' Smith, Nathaniel, and Stéfan Van der Walt. 2015. “A Better Default Colormap for Matplotlib.” In SciPy 2015 – Scientific Computing with Python. Austin.
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
#' CARTO. 2022. CARTOColors – Data-Driven Color Schemes.” https://carto.com/carto-colors/.
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
#' \strong{seaborn}
#'
#' Palettes contained in the Python library `seaborn` by Michael Waskom. \url{https://seaborn.pydata.org/tutorial/color_palettes.html}
#'
#' \strong{stevens}
#'
#' Bivariate palettes by Joshua Stevens
#'
#' \url{https://www.joshuastevens.net/cartography/make-a-bivariate-choropleth-map}. Obtained via the R package `pals`.
#'
#' @name cols4all-package
#' @aliases cols4all
#' @docType package
#' @author Martijn Tennekes \email{mtennekes@@gmail.com}
#' @seealso \url{https://github.com/mtennekes/cols4all}
#' @concept color
#' @concept visualization
NULL

.onLoad <- function(...) {
	assign("z", .z, envir = .C4A)
	assign("s", .s, envir = .C4A)

	with(.C4A,{
		defaults = c(cat = "tol.muted", seq = "hcl.blues2", div = "hcl.purple-green")

		#color-blind-friendly thresholds
		CBF_th = list(cat = c(min_dist = 10),
					  seq = c(min_step = 5),
					  div = c(inter_wing_dist = 10, min_step = 5),
					  bivs = c(inter_wing_dist = 7, min_step = 3),
					  bivc = c(inter_wing_dist = 7, min_step = 3),
					  bivu = c(inter_wing_dist = 7, min_step = 3))

		Cgray = 10 # maximum chroma value to be considered as gray (used for Hwidth and c4a_add_series)
		LrangeWeight = 2/3 # LCrange (which determines harmony) is calculated as max(Lrange * LrangeWeight, Crange * (1-LrangeWeight))
		LCrangeHarmonic = 80/3 # Maximum LCrange values for which the palette is labeled "harmonic"
		Cintense = 100 # chroma of colors that are considered intense
		HwidthDivRainbow = 90 # a diverging palette is labeled as 'rainbow hue' if HwidthL or HwidthR are at least HwidthDivRainbow
		HwidthDivSingle = 20 # a diverging palette is labeled as 'single hue' if HwidthL and HwidthR are at most HwidthDivSingle
		HwidthSeqRainbow = 180 # a sequential palette is labeled as 'rainbow hue' if Hwidth is at least HwidthSeqRainbow
		HwidthSeqSingle = 15 # a sequential palette is labeled as 'single hue' if Hwidth is at most HwidthSeqSingle

		sc = c("min_dist", "min_step", "max_step", "inter_wing_dist", "rank")

		types = c("Categorical" = "cat",
				  "Sequential" = "seq",
				  "Diverging" = "div",
				  "Bivariate (sequential to sequential)" = "bivs",
				  "Bivariate (sequential to categorical)" = "bivc",
				  "Bivariate (sequential to desaturated)" = "bivu")

		indicators = list(cat = c("min_dist"),
						  seq = c("min_step", "max_step"),
						  div = c("inter_wing_dist", "min_step"),
						  bivs = c("inter_wing_dist", "min_step"),
						  bivc = c("inter_wing_dist", "min_step"),
						  bivu = c("inter_wing_dist", "min_step"))
		hcl = c("Cmax", "Hwidth", "HwidthL", "HwidthR", "Lrange", "Crange")

		sortRev = c("Cmax", "min_dist", "Hwidth", "HwidthL", "HwidthR", "nmax")

		labels = c(min_dist = "Minimum distance",
					min_step = "Minimum step",
					max_step = "Maximum step",
					inter_wing_dist = "Inter-wing-distance",
					Crel = "Chroma (rel) max",
					Cmax = "Chroma max",
					Hwidth = "Hue width",
					HwidthL = "Hue width L",
					HwidthR = "Hue width R",
					Lrange = "Luminance range",
					Crange = "Chroma range",
					LCrange = "Lum/Chr range",
					rank = "Ranking",
					cbfriendly = "Colorblind-friendly",
					highC = "Intense colors",
					hueType = "Hues",
					harmonic = "Harmonic palette",
					nmax = "Max number")

		nmax = c(cat = 36, seq = 15, div = 15, bivs = 5, bivc = 5, bivu = 5)
	})
	fill_P()
}

.C4A <- new.env(FALSE, parent=globalenv())



fill_P = function() {
	rm(list = ls(envir = .P), envir = .P)
	z = .C4A$z
	if (is.null(z)) return(invisible(NULL))
	x = sort(unique(z$series))
	y = structure(lapply(x, function(xi) {
		zi = z[z$series == xi, ]
		structure(as.list(zi$fullname), names = zi$name)
	}), names = x)
	list2env(y, envir = .P)
}

