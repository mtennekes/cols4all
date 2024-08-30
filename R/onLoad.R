#' cols4all overview
#'
#' cols4all stands for: color palettes for all people, including those with color vision deficiency. Popular color palette series, such as ColorBrewer, have been organized by type and have been scored on several properties such as color-blind-friendliness and fairness (i.e. do colors stand out equally?). Own palettes can also be loaded and analysed. Besides the common palette types (categorical, sequential, and diverging) it also includes bivariate color palettes. ggplot2 scales are included.
#'
#' This page provides a brief overview of all package functions.
#'
#' @section Main functions:
#' \tabular{ll}{
#' \code{\link{c4a_gui}}\tab Dashboard for analyzing the palettes \cr
#' \code{\link{c4a}}\tab Get the colors from a palette (\code{\link{c4a_na}} for the associated color for missing values) \cr
#' \code{\link{c4a_plot}}\tab Plot a color palette \cr
#' }
#'
#' @section Palette names and properties:
#' \tabular{ll}{
#' \code{\link{c4a_palettes}}\tab Get available palette names \cr
#' \code{\link{c4a_series}}\tab Get available series names\cr
#' \code{\link{c4a_overview}}\tab Get an overview of palettes per series x type\cr
#' \code{\link{c4a_citation}}\tab Show how to cites palettes (with bibtex code) \cr
#' \code{\link{c4a_info}}\tab Get information from a palette, such as type and maximum number of colors) \cr
#' \code{\link{.P}}\tab Environment via which palette names can be browsed with auto-completion (using `$`) \cr

#' }
#'
#' @section Importing and exporting palettes:
#' \tabular{ll}{
#' \code{\link{c4a_data}}\tab Build color palette data \cr
#' \code{\link{c4a_load}}\tab Load color palette data \cr
#' \code{\link{c4a_sysdata_import}}\tab Import system data \cr
#' \code{\link{c4a_sysdata_export}}\tab Export system data \cr
#' }
#'
#' @concept color
#' @concept visualization
"_PACKAGE"


#' Set cols4all options
#'
#' Get or set global options for c4a. Works similar as the base function `options`
#'
#' @param ... Use character values to retrieve options. To set options, either use named arguments (where the names refer to the options), a list that consists of those options.
#'
#' @details
#'
#' | **Option**        | **Description**   |
#' | ------------- | ------------- |
#' | defaults		|  Default palettes per type |
#' | CBF_th		|  Parameters that label a palette as color blind friendly  |
#' | CBU_th		| Parameters that label a palette as color blind unfriendly |
#' | CrangeFair		| Maximum chroma range for which a palette is considered harmonic |
#' | CrangeUnfair		| Minimum chroma range for which a palette is considered disharmonic  |
#' | LrangeFair		| Maximum luminance range for which a palette is considered harmonic |
#' | LrangeUnfair		| Minimum luminance range for which a palette is considered disharmonic |
#' | Cintense		| Chroma of colors that are considered intense |
#' | Cpastel		| Chroma of colors that are considered 'pastel' |
#' | HwidthDivRainbow		| A diverging palette is labeled as 'rainbow hue' if HwidthL or HwidthR are at least `HwidthDivRainbow` |
#' | HwidthDivSingle		| A diverging palette is labeled as 'single hue' if HwidthL and HwidthR are at most `HwidthDivSingle` |
#' | HwidthSeqRainbow | A sequential palette is labeled as 'rainbow hue' if Hwidth is at least `HwidthSeqRainbow` |
#' | HwidthSeqSingle | A sequential palette is labeled as 'single hue' if Hwidth is at most `HwidthSeqSingle` |
#'
#' @md
#' @name c4a_options
#' @rdname c4a_options
#' @return A list of options
#' @export
c4a_options = function(...) {
	lst = list(...)
	e1 = parent.frame()
	nms = c("defaults", "CBF_th", "CBU_th", "CrangeFair", "CrangeUnfair", "LrangeFair", "LrangeUnfair", "Cintense", "Cpastel", "HwidthDivRainbow", "HwidthDivSingle", "HwidthSeqRainbow", "HwidthSeqSingle", "boynton_weights")

	o = as.list(.C4A)[nms]

	if (length(lst) >= 1 && is.null(names(lst))) {
		arg = lst[[1]]
		if (is.list(arg)) {
			## case 1: option list is given
			args = arg
			if (length(lst) > 1) warning("Only the first argument is used; the other arguments are ignored.")
		} else {
			## case 2: option name is given
			args = sapply(lst, "[", 1)
			return(o[args])
		}
	} else {
		## case 3: named options are set
		## case 4: c4a_options is called without arguments
		args = lapply(as.list(match.call()[-1]), eval, envir = e1)
	}


	if (!length(args)) {
		# case 4
		return(o)
	} else {
		# case 1 and 3
		backup = o[names(args)]
		o[names(args)] = args # check_named_items(args, backup)

		list2env(args, envir = .C4A)

		if ("boynton_weights" %in% names(args)) {
			.C4A$name_data = create_name_data()
		}

		invisible(backup)
	}
}



do_cellspec = function(lst) {
	do.call(kableExtra::cell_spec, lst)
}


.onLoad <- function(...) {
	assign("z", .z, envir = .C4A)
	assign("s", .s, envir = .C4A)
	assign("zbib", .zbib, envir = .C4A)
	assign("zdes", .zdes, envir = .C4A)
	name_data = rdata$name_data
	assign("name_data", name_data, envir = .C4A)


	attach_bib()

	with(.C4A,{
		defaults = c(cat = "tol.muted", seq = "hcl.blues2", div = "hcl.purple_green", bivs = "c4a.bu_br_bivs", bivc = "met_monet", bivd = "c4a.pu_gn_bivd", bivg = "c4a.br_bivg")

		score_x100 = c("min_dist", "min_step", "max_step", "inter_wing_dist", "CRmin", "CRwt", "CRbk", "Blues")

		#color-blind-friendly thresholds
		CBF_th = list(cat = c(min_dist = 10),
					  seq = c(min_step = 5),
					  div = c(inter_wing_dist = 10, min_step = 5),
					  bivs = c(inter_wing_dist = 7, min_step = 3),
					  bivc = c(min_dist = 10),
					  bivd = c(inter_wing_dist = 7, min_step = 3),
					  bivg = c(inter_wing_dist = 7, min_step = 3))

		#color-blind-very-friendly thresholds
		CBVF_th = list(cat = c(min_dist = 15))

		# unfriendly (rolling eyes)
		CBU_th = list(cat = c(min_dist = 2),
					  seq = c(min_step = 1),
					  div = c(inter_wing_dist = 4, min_step = 1),
					  bivs = c(inter_wing_dist = 3, min_step = 1),
					  bivc = c(min_dist = 2),
					  bivd = c(inter_wing_dist = 3, min_step = 1),
					  bivg = c(inter_wing_dist = 3, min_step = 1))

		Cgray = 10 # maximum chroma value to be considered as gray (used for Hwidth and c4a_add_series)

		CrangeFair = 50
		CrangeUnfair = 80
		LrangeFair = 30
		LrangeUnfair = 50

		Blues = 3
		contrastEL = 1.2 # Equiluminance
		contrastTxt = 3

		Cintense = 100 # chroma of colors that are considered intense
		Cpastel = 70 # chroma of 'pastel' colors
		HwidthDivRainbow = 90 # a diverging palette is labeled as 'rainbow hue' if HwidthL or HwidthR are at least HwidthDivRainbow
		HwidthDivSingle = 20 # a diverging palette is labeled as 'single hue' if HwidthL and HwidthR are at most HwidthDivSingle
		HwidthSeqRainbow = 180 # a sequential palette is labeled as 'rainbow hue' if Hwidth is at least HwidthSeqRainbow
		HwidthSeqSingle = 15 # a sequential palette is labeled as 'single hue' if Hwidth is at most HwidthSeqSingle

		sc = c("min_dist",
			   "nameability",
			   "min_step",
			   "max_step",
			   "inter_wing_dist")

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

		CB_ranges = list(cat = list(min_dist = c(0, 20)),
						 seq = list(min_step = c(0, 20), max_step = c(0, 20)),
						 div = list(inter_wing_dist = c(0, 20), min_step = c(0, 20)),
						 bivs = list(inter_wing_dist = c(0, 20), min_step = c(0, 20)),
						 bivc = list(min_dist = c(0, 20)),
						 bivd = list(inter_wing_dist = c(0, 20), min_step = c(0, 20)),
						 bivg = list(inter_wing_dist = c(0, 20), min_step = c(0, 20)))

		Ohter_ranges = list(C = c(0, 180, 5),
							L = c(0, 100, 5),
							H = c(0, 360, 5),
							Blues = c(1, 5, 0.1),
							contrastTH = c(1, 2, 0.1))


		rgb = c("Blues")
		hcl = c("Cmax", "H", "HL", "HR", "Lmid", "Hwidth", "HwidthL", "HwidthR", "Lrange", "Crange", "CRmin", "CRwt", "CRbk")

		sortRev = c("cbfriendly", "harmonyRank", "fairRank", "Cmax", "min_dist", "nameability", "Lmid", "Hwidth", "HwidthL", "HwidthR", "nmax", "CRmin", "CRwt", "CRbk", "Blues")

		boynton_weights = c(Green = 1, Blue = 1, Purple = 1, Pink = 1,
					Yellow = 1, Brown = 1, Orange = 1, Red = 1,
					White = 1, Gray = 1, Black = 1)


		labels = c(min_dist = "Minimum distance",
				   nameability = "Nameability",
				   min_step = "Minimum step",
				   max_step = "Maximum step",
				   inter_wing_dist = "Inter-wing-distance",
				   Crel = "Chroma (rel) max",
				   Cmax = "Chroma max",
				   H = "Hue middle",
				   HL = "Hue middle L",
				   HR = "Hue middle R",
				   Lmid = "Luminance mid",
				   Hwidth = "Hue width",
				   HwidthL = "Hue width L",
				   HwidthR = "Hue width R",
				   Lrange = "Luminance range",
				   Crange = "Chroma range",
				   LCrange = "Lum/Chr range",
				   CRmin = "Contrast-Ratio minimum",
				   CRwt = "Contrast-Ratio white",
				   CRbk = "Contrast-Ratio black",
				   cbfriendly = "Colorblind-friendly",
				   chroma = "Vivid",
				   fair = "Fair",
				   nameable = "Naming",
				   fairRank = "Fair",
				   hueType = "Hue type",
				   equiluminance = "Contrast (between)",
				   contrastWT = "Contrast (white)",
				   contrastBK = "Contrast (black)",
				   float = "3D Blues",
				   Blues = "Dominant blues",
				   nmax = "Max number")

		th = list(series = list("Series", tooltip = "Palette series. See last column for references"),
				  name = list("Name", tooltip = "Palette name"),
				  cbfriendly = list("Colorblind-friendly", tooltip = "Is the palette suitable for colorblind people?"),
				  chroma = list("Vivid", tooltip = "Are the colors vivid  or pastel?"),
				  nmax = list("Max number", tooltip = "Maximum number of colors"),
				  fair = list("Fair", tooltip = "Do colors stand out about equally?"),
				  contrastWT = list("Contrast", tooltip = "Contrast issues with white, black, and between the colors."),
				  contrastBK = list("", tooltip = ""),
				  equiluminance = list("", tooltip = ""),
				  nameable = list("Naming", tooltip = "Are the colors are easy to name? If so, they are also easy to remember"),
				  float = list("3D Blues", tooltip = "Is there a pure blue color that may cause a 3D illusion?"),
				  hueType = list("Hue type", tooltip = "How many different hues are used?"),
				  references = list("References", tooltip = "Click to copy the colors and references"))

		tc = list(cbfriendly = list('NA' = "",
									'0' = "",
									'2' = list("&#9786;&#9786;", extra_css="font-size: 80%;", tooltip = "Extra colorblind-friendly! Also for points and lines", escape = FALSE),
									'1' = list("&#9786;", extra_css="font-size: 80%;", tooltip = "Colorblind-friendly! Be careful with points and lines", escape = FALSE),
									'-1' = list("&#128064;", extra_css ="font-size: 60%;", tooltip = "Be careful! Some colors are hard to distinguish by color blind people (see tab 'Color Blind Friendliness'", escape = FALSE)),
				  chroma = list('NA' = "",
				  			  'H' = list("&#x1f576;", tooltip = "Vivid colors (high chroma): ideal for small important objects to stand out (e.g. markers on a map), but less suited for space filling visualizations (see tab 'HCL Analysis')", escape = FALSE),
				  			  'M' = "",
				  			  'L' = list("&#10057;", tooltip = "Pastel colors (low chroma): ideal for space filling visualizations, such as choropleths (see tab 'HCL Analysis')", escape = FALSE, extra_css = "font-size: 70%;")), #&#9729; &#10020;
				  hueType = list(seq = list('NA' = "",
				  						  'MH' = "",
				  						  'RH' = list("&#127752;",
				  						  			tooltip = "Spectral (&#34;rainbow&#34;) palette: easy to distinguish colors, but less suitable for quantitative analysis",
				  						  			escape = FALSE, extra_css = "font-size: 150%; vertical-align: -0.1em; line-height: 0px;"),
				  						  'SH' = list("&#128396;",
				  						  			tooltip = "Single hue palette: good for quantitative analysis, but harder to distinguish colors",
				  						  			escape = FALSE, extra_css = "font-size: 200%; vertical-align: -0.2em; line-height: 0px;")),
				  			   bivg = list('NA' = "",
				  			   			'MH' = "",
				  			   			'RH' = list("&#127752;",
				  			   						tooltip = "Spectral (&#34;rainbow&#34;) palette: easy to distinguish colors, but less suitable for quantitative analysis",
				  			   						escape = FALSE, extra_css = "font-size: 150%; vertical-align: -0.1em; line-height: 0px;"),
				  			   			'SH' = list("&#128396;",
				  			   						tooltip = "Single hue palette: good for quantitative analysis, but harder to distinguish colors",
				  			   						escape = FALSE, extra_css = "font-size: 200%; vertical-align: -0.2em; line-height: 0px;")),
				  			   div = list('NA' = "",
				  			   		   'MH' = "",
				  			   		   'RH' = list("&#127752;",
				  			   		   			tooltip = "Spectral (&#34;rainbow&#34;) palette: easy to distinguish colors, but less suitable for quantitative analysis",
				  			   		   			escape = FALSE, extra_css = "font-size: 150%; vertical-align: -0.1em; line-height: 0px;"),
				  			   		   'SH' = list("&#x262F;",
				  			   		   			tooltip = "Each side has its own distinct hue: recommended!",
				  			   		   			escape = FALSE, extra_css = "font-size: 200%; vertical-align: -0.2em; line-height: 0px;")),
				  			   bivd = list('NA' = "",
				  			   			'MH' = "",
				  			   			'RH' = list("&#127752;",
				  			   						tooltip = "Spectral (&#34;rainbow&#34;) palette: easy to distinguish colors, but less suitable for quantitative analysis",
				  			   						escape = FALSE, extra_css = "font-size: 150%; vertical-align: -0.1em; line-height: 0px;"),
				  			   			'SH' = list("&#x262F;",
				  			   						tooltip = "Each side has its own distinct hue: recommended!",
				  			   						escape = FALSE, extra_css = "font-size: 200%; vertical-align: -0.2em; line-height: 0px;")),
				  			   bivs = list('NA' = "",
				  			   			'MH' = "",
				  			   			'RH' = list("&#127752;",
				  			   						tooltip = "Spectral (&#34;rainbow&#34;) palette: easy to distinguish colors, but less suitable for quantitative analysis",
				  			   						escape = FALSE, extra_css = "font-size: 150%; vertical-align: -0.1em; line-height: 0px;"),
				  			   			'SH' = list("&#x262F;",
				  			   						tooltip = "Each dimension has its own distinct hue: recommended!",
				  			   						escape = FALSE, extra_css = "font-size: 200%; vertical-align: -0.2em; line-height: 0px;"))
				  ),
				  fair = list(cat = list('NA' = "",
				  					   'M' = "",
				  					   'L' =  list("&#10799;",
				  					   			tooltip = "Unfair: colors are not equally vivid and/or bright. See tab 'HCL Analysis'", escape = FALSE,
				  					   			extra_css = "font-size: 100%; vertical-align: 0.1em; line-height: 0px;"),
				  					   'H' = list("&#9825;",
				  					   		   tooltip = "Fair: colors are equally vivid and bright. See tab 'HCL Analysis'", escape = FALSE,
				  					   		   extra_css = "font-size: 60%; vertical-align: 0em; line-height: 0px;")),
				  			x = list('NA' = "",
				  					 'M' = "",
				  					 'L' =  list("&#10799;",
				  					 			tooltip = "Unfair: colors are not equally vivid. See tab 'HCL Analysis'", escape = FALSE,
				  					 			extra_css = "font-size: 100%; vertical-align: 0.1em; line-height: 0px;"),
				  					 'H' = list("&#9825;",
				  					 		   tooltip = "Fair: colors are equally vivid. See tab 'HCL Analysis'", escape = FALSE,
				  					 		   extra_css = "font-size: 60%; vertical-align: 0em; line-height: 0px;"))),
				  nameable = list('NA' = "",
				  				  'FALSE' =  "",
				  				  'TRUE' = list("&#10023;",
				  				  			  tooltip = "Colors are easy to name, and therefore, easy to remember",
				  				  			  escape = FALSE, extra_css = "font-size: 130%; vertical-align: -0.1em; line-height: 0px;")),
				  equiluminance = list('NA' = "",
				  				'FALSE' =  "",
				  				'TRUE' = list("&#43612;",
				  							  tooltip = "Very low contrast between some colors (equiluminance); borders needed (see tab 'Contrast')",
				  							  escape = FALSE, extra_css = "font-size: 130%; vertical-align: -0.1em; line-height: 0px;")),
				  contrastWT = list('NA' = "",
				  				  'FALSE' =  "",
				  				  'TRUE' = list("&#127987;",
				  				  			  tooltip = "Low contrast with white (see tab 'Contrast')",
				  				  			  escape = FALSE, extra_css = "font-size: 130%; vertical-align: -0.1em; line-height: 0px; margin-right: -10px;")),
				  contrastBK = list('NA' = "",
				  				  'FALSE' =  "",
				  				  'TRUE' = list("&#127988;",
				  				  			  tooltip = "Low contrast with black (see tab 'Contrast')",
				  				  			  escape = FALSE, extra_css = "font-size: 130%; vertical-align: -0.1em; line-height: 0px; margin-right: -10px;")),
				  float = list('NA' = "",
				  			 'FALSE' = "",
				  			 'TRUE' = list("&#128313;",
				  			 			  tooltip = "This palette has got the blues; it contains a pure blue color which may cause a floating (3D) effect next to red colors (see tab '3D Blues')",
				  			 			  escape = FALSE, extra_css = "font-size: 130%; vertical-align: -0.1em; line-height: 0px; color: '#000000'"))
		)


		nmax = c(cat = 36, seq = 15, div = 15, bivs = 7, bivc = 7, bivd = 7, bivg = 7)
		nmin = c(cat = 1, seq = 2, div = 3, bivs = 2, bivc = 2, bivd = 3, bivg = 2)
		mdef = c(bivc = 5, bivd = 5, bivg = 5)
		matrix_breaks = list(CR = c(1, 1.2, 1.5, 2, 3, 4.5, 7), dist = c(0, 2, 5, 10, 15))
		matrix_pchs = list(CR = c(15, 17, 16, 1, 1, 2, 0), dist = c(15, 17, 16, 16, 1))
		matrix_sizes = list(CR = c(1, 0.6, 0.3, 0, 0.3, 0.6, 1), dist = c(1, 0.6, 0.6, 0.3, 0))
		matrix_interval_labels = list(CR = c("1.0 - 1.2", "1.2 - 1.5", "1.5 - 2.0", "", "3.0 - 4.5", "4.5 - 7.0", "7.0 +"), dist = c("< 2", "2 - 5", "5 - 10", "10 - 15"))
		matrix_breaks_digits = c(CR = 1, dist = 0)

	})
	fill_P()
}

.C4A <- new.env(FALSE, parent=globalenv())



fill_P = function() {
	rm(list = ls(envir = .P), envir = .P)
	z = .C4A$z[, c("name", "fullname", "series", "type")]
	if (is.null(z)) return(invisible(NULL))
	x = sort(unique(z$series))
	y1 = structure(lapply(x, function(xi) {
		zi = z[z$series == xi, ]
		structure(as.list(zi$fullname), names = zi$name)
	}), names = x)

	tps = unname(.C4A$types)

	y = structure(lapply(x, function(xi) {
		zi = z[z$series == xi, ]

		tpx = tps[tps %in% unique(zi$type)]

		structure(lapply(tpx, function(ti) {
			zii = zi[zi$type == ti, ]
			structure(as.list(zii$fullname), names = zii$name)
		}), names = tpx)
	}), names = x)

	list2env(y, envir = .P)
}

