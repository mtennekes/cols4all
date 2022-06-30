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
#' \code{\link{c4a_series}}\tab Get available series names\cr
#' \code{\link{c4a_info}}\tab Get palette information (such as type and maximum number of colors) \cr
#' \code{\link{c4a_citation}}\tab Show how to cites palettes (with bibtex code) \cr
#' \code{\link{.P}}\tab Environment via which palette names can be browsed with auto-completion (using `$`) \cr
#' }
#'
#' @section Importing and exporting palettes:
#' \tabular{ll}{
#' \code{\link{c4a_palettes_add}}\tab Add color palettes \cr
#' \code{\link{c4a_palettes_remove}}\tab Remove color palettes \cr
#' \code{\link{c4a_sysdata_import}}\tab Import system data \cr
#' \code{\link{c4a_sysdata_export}}\tab Export system data \cr
#' }
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
	assign("zbib", .zbib, envir = .C4A)

	attach_bib()

	with(.C4A,{
		defaults = c(cat = "tol.muted", seq = "hcl.blues2", div = "hcl.purple-green")

		#color-blind-friendly thresholds
		CBF_th = list(cat = c(min_dist = 10),
					  seq = c(min_step = 5),
					  div = c(inter_wing_dist = 10, min_step = 5),
					  bivs = c(inter_wing_dist = 7, min_step = 3),
					  bivc = c(min_dist = 10),
					  bivd = c(inter_wing_dist = 7, min_step = 3),
					  bivg = c(inter_wing_dist = 7, min_step = 3))

		# unfriendly (rolling eyes)
		CBU_th = list(cat = c(min_dist = 2),
					  seq = c(min_step = 1),
					  div = c(inter_wing_dist = 4, min_step = 1),
					  bivs = c(inter_wing_dist = 3, min_step = 1),
					  bivc = c(min_dist = 2),
					  bivd = c(inter_wing_dist = 3, min_step = 1),
					  bivg = c(inter_wing_dist = 3, min_step = 1))

		Cgray = 10 # maximum chroma value to be considered as gray (used for Hwidth and c4a_add_series)
		# LrangeWeight = 2/3 # LCrange (which determines harmony) is calculated as max(Lrange * LrangeWeight, Crange * (1-LrangeWeight))
		# LCrangeHarmonic = 80/3 # Maximum LCrange values for which the palette is labeled "harmonic"

		CrangeHarmonic = 40 # replacement for LCrangeHarmonic (and LrangeWeight)

		Cintense = 100 # chroma of colors that are considered intense
		HwidthDivRainbow = 90 # a diverging palette is labeled as 'rainbow hue' if HwidthL or HwidthR are at least HwidthDivRainbow
		HwidthDivSingle = 20 # a diverging palette is labeled as 'single hue' if HwidthL and HwidthR are at most HwidthDivSingle
		HwidthSeqRainbow = 180 # a sequential palette is labeled as 'rainbow hue' if Hwidth is at least HwidthSeqRainbow
		HwidthSeqSingle = 15 # a sequential palette is labeled as 'single hue' if Hwidth is at most HwidthSeqSingle

		sc = c("min_dist",
			   "min_step",
			   "max_step",
			   "inter_wing_dist",
			   "rank")

		types = c("Categorical" = "cat",
				  "Sequential" = "seq",
				  "Diverging" = "div",
				  "Bivariate (sequential x sequential)" = "bivs",
				  "Bivariate (sequential x categorical)" = "bivc",
				  "Bivariate (sequential x diverging)" = "bivd",
				  "Bivariate (sequential x desaturated)" = "bivg")

		types1 = c("Categorical" = "cat",
				   "Sequential" = "seq",
				   "Diverging" = "div",
				   "Bivariate" = "biv")

		types2 = list(biv = c("Sequential x sequential" = "bivs",
				   "Sequential x categorical" = "bivc",
				   "Sequential x diverging" = "bivd",
				   "Sequential x desaturated" = "bivg"))

		type_info = data.frame(type = c("cat", "seq", "div", "bivs", "bivc", "bivd", "bivg"),
							   description = c("categorical",
							   				"sequential",
							   				"diverging",
							   				"bivariate (sequential x sequential)", "bivariate (sequential x categorical)", "bivariate (sequential x diverging)", "bivariate (sequential x desaturated)"))

		ndef = c(cat = Inf, seq = 7, div = 9, bivc = Inf, bivs = 3, bivd = 3, bivg  = 3) # Inf meaning maximum available colors
		mdef = c(cat = 1, seq = 1, div = 1, bivc = 3, bivs = NA, bivd = 3, bivg  = 3) # NA meaning same as ndef

		indicators = list(cat = c("min_dist"),
						  seq = c("min_step", "max_step"),
						  div = c("inter_wing_dist", "min_step"),
						  bivs = c("inter_wing_dist", "min_step"),
						  bivc = c("min_dist"),
						  bivd = c("inter_wing_dist", "min_step"),
						  bivg = c("inter_wing_dist", "min_step"))
		hcl = c("Cmax", "Hwidth", "HwidthL", "HwidthR", "Lrange", "Crange", "CRmin", "CRbg")

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
					CRmin = "contrast-ratio min",
					CRbg = "contrast-ratio white bg",
					rank = "Ranking",
					cbfriendly = "Colorblind-friendly",
					cbfuf = "Colorblind-friendly",
					highC = "Intense colors",
					hueType = "Hues",
					harmonic = "Harmonic palette",
					nmax = "Max number")

		nmax = c(cat = 36, seq = 15, div = 15, bivs = 7, bivc = 7, bivd = 7, bivg = 7)
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

