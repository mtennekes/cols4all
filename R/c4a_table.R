table_columns = function(type, show.scores) {
	if (type == "cat") {
		qn = c("nmax", "cbfriendly", "highC")
		srt = c("nmax", "rank", "Cmax")
	} else {
		qn = c("cbfriendly", "highC")
		srt = c("rank", "Cmax")
	}

	if (type %in% c("seq", "div")) {
		qn = c(qn, "hueType")
		srt = c(srt, {if (type == "div") "HwidthLR" else "Hwidth"})
	} else {
		qn = c(qn, "harmonic")
		srt = c(srt, "LCrange")
	}

	qn = c(qn, "rank")
	srt = c(srt, "rank")

	if (show.scores) {
		qn = c(qn, .C4A$indicators[[type]], .C4A$hcl)
		srt = c(srt, .C4A$indicators[[type]], .C4A$hcl)
	}
	ql = .C4A$labels[qn]

	list(qn = qn, ql = ql, srt = srt)
}


#' Graphical user interface to analyse palettes
#'
#' Graphical user interface to analyse palettes. `c4a_table` shows a table that can be opened in the browser. `c4a_gui` is a graphical user interface (shiny app) around this table. The package `kableExtra` is required for `c4a_table` and for `c4a_gui` the pacakges `shiny` and `shinyjs`.
#'
#' @section Table Columns:
#'
#' \tabular{lll}{
#'   \strong{Column&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;} \tab \strong{Name} \tab \strong{Description} \cr
#' Max n \tab `"nmax"` \tab Maximum number of colors (`"cat"` only) \cr
#' Colorblind-friendly \tab `"cbfriendly"` \tab Is it color-blind friendy? \cr
#' Harmonic palette \tab `"harmonic"` \tab Are the colors in harmony with each other? \cr
#' Intense colors \tab `"highC` \tab Are there any intense (saturated) colors? \cr
#' Hue type \tab `"hueType"` \tab How many different hue ranges are used? For a sequential (`"seq"`) palette we consider three classes. 1) "single hue" where one hue is used, which is recommended for quantitative analysis. This is indicated by a paint brush icon 2) "spectral hue" where a wide range of hues are used, e.g. a rainbow palette. This less suitable for quantitative analysis but better to read different colors. This is indicated by a rainbow icon 3) a trade-off between the two mentioned classes (no icon used). For a diverging (`"div"`) palette, we also consider similar three classes. 1) "two hues", where one hue is used for the left wing and one for the right wing. 2) "spectral hue" and 3) trade-off. \cr
#' Ranking \tab `"rank"` \tab Ranking of palettes taking the above into account
#' }
#'
#' @section Color-blind-friendliness indicators:
#'
#' \tabular{lll}{
#'   \strong{Indicator&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;} \tab \strong{Name} \tab \strong{Description} \cr
#' Minimum distance \tab `"min_dist"` \tab Minimum distance between any two colors for any color vision deficiency type. This is a measure to which extend categorical palettes (type `"cat"`) are suitable for people with color vision deficiency \cr
#' Minimum step \tab `"min_step"` \tab Minimum distance between two neighboring colors in a sequential (`"seq"`) or diverging (`"div"`) palette, for any color vision deficiency type. The larger, the better. \cr
#' Maximum step \tab `"max_step"` \tab Maximum distance between two neighboring colors in a sequential (`"seq"`) palette, for any color vision deficiency type. For sequential palettes that score the same on `"min_step"`, the ones with lower `"max_step"` values are slightly preferable, because this means that the distances between neighboring colors is more homogeneous. \cr
#' Inter-wing-distance \tab `"inter_wing_dist"` \tab Minimum distance between any color in the left wing to any color in the right wing of a diverging (`"div"`) palette, for any color vision deficiency type. The larger, the better. \cr
#' Inter-wing hue distance \tab `"inter_wing_hue_dist"` \tab Distance between the two hue ranges in both wings of a (`"div"`) palette, for any color vision deficiency type. The larger the better. We consider 100 degrees as sufficient to discriminate two hues.
#' }
#'
#' Color-blind friendliness scores are calculated as:
#'
#' * `"cat"` `min_step`
#' * `"seq"` `min_step` - `max_step` / 1000
#' * `"div"` min(`inter_wing_dist`, `min_step` * 2) + (`inter_wing_hue_dist` >= 100) * 1000
#'
#' Note: these formulas are in development, and not stable. Suggestions are welcome (via github issues).
#'
#' @section General indicators:
#'
#' We use the HCL color space, where H is the hue, C the chroma and L the lightness. See the `colorspace` package (that is used under the hood) for details.
#'
#' \tabular{lll}{
#'   \strong{Indicator&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;} \tab \strong{Name} \tab \strong{Description} \cr
#'   Chroma max \tab `"Cmax"` \tab Maximum chroma value. We have set the threshold for the label "intense colors" at 100. \cr
#' Hue width \tab `"Hwidth"` \tab The width/range of hue values that are used. For instance, if a palette has hue values 100, 140, and 220, the Hwidth is 120. Since hues are provided in degrees, a hue width close to 360 means that many hues are used (e.g. in a rainbow palette). The primary use is to determine the hue type (see above). \cr
#' Hue width L \tab `"HwidthL"` \tab Same, but only for the left wing of the palette (useful for diverging palettes) \cr
#' Hue width R \tab `"HwidthR"` \tab Same, but only for the right wing of the palette (useful for diverging palettes) \cr
#' Luminance range \tab `"Lrange"` \tab The range of luminance values of the colors. The smaller, the better for a what we call harmonic palette. However, this is at the expense of having distinguishable colors (which contribute to color-blind-friendliness). \cr
#' Chroma range \tab `"Crange` \tab The range of chroma values of the colors. Like `"Lrange"`, the lower the better. \cr
#' Lum/Chr range \tab `"LCrange"` \tab Defined as max(2 * `Lrange`, `Crange`), and used to label a palette "harmonic". This formula is determined by some trial-and-error, so suggestions for improvement are welcome.
#' }
#'
#' @param type type of palette: `"cat"` for categorical (aka qualitative), `"seq"` for sequential, and `"div"` for diverging. For `c4a_gui` it only determines which type is shown initially.
#' @param n number of colors. If omitted: for `"cat"` the full palette is displayed, and for `"seq"` and `"div"`, 9 colors. For `c4a_gui` it only determines which number of colors initially.
#' @param cvd.sim color vision deficiency simulation: one of `"none"`, `"deutan"`, `"protan"`, `"tritan"`
#' @param sort column name to sort the data. For column names, see details. Use a `"-"` prefix to reverse the order.
#' @param text.format The format of the text of the colors. One of `"hex"`, `"RGB"` or `"HCL"`.
#' @param text.col The text color of the colors. By default `"same"`, which means that they are the same as the colors themselves (so invisible, but available for selection).
#' @param series Series of palettes to show. See \code{\link{c4a_series}} for options. By default, `"all"`, which means all series. For `c4a_gui` it only determines which series are shown initially.
#' @param contrast vector of two numbers that determine the range that is used for sequential and diverging palettes. Both numbers should be between 0 and 1. The first number determines where the palette begins, and the second number where it ends. For sequential palettes, 0 means the leftmost (normally lightest) color, and 1 the rightmost (often darkest) color. For diverging palettes, 0 means the middle color, and 1 both extremes. If only one number is provided, this number is interpreted as the endpoint (with 0 taken as the start). By default, it is set automatically, based on `n`. See `c4a_gui`, or the internal functions `cols4all::default_contrast_seq` and `cols4all::default_contrast_div` to see what the automatic values are.
#' @param include.na should color for missing values be shown? `FALSE` by default
#' @param show.scores should scores of the quality indicators be printed? See details for a description of those indicators.
#' @param columns number of columns. By default equal to `n` or, if not specified, 12. Cannot be higher than the palette
#' @import colorspace abind
#' @example ./examples/c4a_table.R
#' @export
#' @rdname c4a_gui
#' @name c4a_gui
c4a_table = function(type = c("cat", "seq", "div"), n = NULL, cvd.sim = c("none", "deutan", "protan", "tritan"), sort = "name", text.format = "hex", text.col = "same", series = "all", contrast = NA, include.na = FALSE, show.scores = FALSE, columns = NA) {
	id = NULL

	#if (length(series) == 2) browser()

	if (!requireNamespace("kableExtra")) stop("Please install kableExtra")

	.labels = .C4A$labels

	type = match.arg(type)
	show.ranking = (!is.null(n))
	cvd.sim = match.arg(cvd.sim)
	if (is.na(columns)) columns = if (!is.null(n)) n else 12

	# palettes for selected type, n colors, and optionally for specific series
	z = .C4A$z

	if (is.null(z)) {
		message("No palette series loaded. Please reload cols4all, add series with c4a_series_add, or import data with c4a_sysdata_import")
		return(invisible(NULL))
	}

	if (is.na(contrast[1])) contrast = c4a_default_contrast(n, type)


	zn = get_z_n(z[z$type == type, ], n = n, contrast = contrast)
	if (!series[1] == "all") zn = zn[zn$series %in% series, ]

	if (nrow(zn) == 0) return(NULL)

	zn = show_attach_scores(zn, contrast = contrast)
	# better but slower alternative: calculate all scores read time:
	#zn = get_scores_zn(zn)

	columns = min(columns, max(zn$n))

	k = nrow(zn)

	res = table_columns(type, show.scores)
	qn = res$qn
	ql = res$ql
	srt = res$srt

	isrev = (substr(sort, 1, 1) == "-")
	if (isrev) sort = substr(sort, 2, nchar(sort))


	sortCol = if (sort == "name") "fullname" else srt[which(sort == qn)]
	decreasing = xor(isrev, sortCol %in% .C4A$sortRev)
	zn = zn[order(zn[[sortCol]], decreasing = decreasing), ]

	zn$nlines = ((zn$n-1) %/% columns) + 1

	#zn$Name = gsub(".*\\.", "", zn$name)

	# data.frame with possibly multiple lines per palette (if n > columns)
	e = data.frame(id = unlist(mapply(function(n, i) {
		c(i * 1000 + 1:n)
	}, zn$nlines, 1L:k, SIMPLIFY = FALSE))
	)
	e = within(e, {
		did = floor(id/1000)
		ind = id - did * 1000
		indx = sapply(1L:k, function(i) which.max(ind[did==i]))[did]
		name = ""
		name[did>0] = zn$name[match(did[did>0], 1L:k)]
		label = name
		label[ind!=1] = ""
		series = zn$series[match(did, 1L:k)]
		series[ind!=1] = ""
		row_h = ifelse(ind==indx, 2, 1.4)
	})
	e = cbind(e, zn[match(paste(e$series, e$label, sep = "."), zn$fullname),qn,drop=FALSE])
	colnames(e)[match(qn, colnames(e))] = ql

	# total number of columns
	tot = max(c(zn$n, columns))
	if (columns < tot) tot = (((tot-1) %/% columns) + 1) * columns

	# maximum number of lines per palette
	ml = ceiling(tot / columns)

	# color matrix
	m = local({
		x = lapply(zn$palette, function(p) {
			if (length(p) < tot) {
				c(p, rep("", tot - length(p)))
			} else {
				p
			}
		})
		unname(do.call(rbind, x))
	})

	# color matrix spread over lines
	me = local({
		sid = split(1:tot, f = rep(1:ml, each = columns, length.out = tot))
		x = do.call(rbind, lapply(1:nrow(e), function(i) {
			m[e$did[i], sid[[e$ind[i]]]]
		}))
		colnames(x) = 1:ncol(x)
		x
	})

	if (include.na) {
		me = cbind(me, ' '="", 'Missings' = "")
		me[match(1:k, e$did), ncol(me)] = zn$na
		colNames = c(1:columns, " ", "Missings")
		palList = mapply(c, zn$palette, zn$na, SIMPLIFY = FALSE, USE.NAMES = FALSE)
	} else {
		colNames = as.character(1:columns)
		palList = unname(zn$palette)
	}

	e2 = cbind(e, me)

	# copy links
	txt1 = rep("&#128471;", nrow(e2))
	txt1[e2$ind !=1 ] = ""
	links1 = sapply(1:nrow(e2), function(rw) {
		if (txt1[rw] == "") {
			""
		} else{
			did = e2$did[rw]
			paste0("javascript:navigator.clipboard.writeText(`[&quot;",
				   paste(palList[[did]], collapse = "&quot;, &quot;"), "&quot;]`)")
		}
	}, USE.NAMES = FALSE)
	txt2 = rep("R", nrow(e2))
	txt2[e2$ind !=1 ] = ""
	links2 = sapply(1:nrow(e2), function(rw) {
		if (txt2[rw] == "") {
			""
		} else{
			did = e2$did[rw]
			paste0("javascript:navigator.clipboard.writeText(`c(&quot;",
				   paste(palList[[did]], collapse = "&quot;, &quot;"), "&quot;)`)")
		}
	}, USE.NAMES = FALSE)

	e2[['Copy1']] = kableExtra::cell_spec(txt1, link=links1, tooltip='Copy colors: [&quot;#111111&quot;, &quot;#222222&quot;]', escape = FALSE, extra_css = "text-decoration: none; color: #B4B4B4;")
	e2[['Copy2']] = kableExtra::cell_spec(txt2, link=links2, tooltip='Copy colors: c(&quot;#111111&quot;, &quot;#222222&quot;)', escape = FALSE, extra_css = "text-decoration: none; color: #B4B4B4;")


	sim = switch(cvd.sim,
				 none = function(x) x,
				 deutan = colorspace::deutan,
				 protan = colorspace::protan,
				 tritan = colorspace::tritan)


	for (cn in colNames) {
		cols = e2[[cn]]
		sel = (!is.na(cols) & cols != "")
		cols[!sel] = ""
		cols_cvd = cols
		cols_cvd[sel] = sim(cols[sel])
		textcol = if (text.col == "same") cols_cvd else text.col

		txt = cols
		if (any(sel)) txt[sel] = switch(text.format, hex = cols[sel], RGB = get_rgb_triple(cols[sel]), HCL = get_hcl_triple(cols[sel]))

		#e2[[cn]] = kableExtra::cell_spec(txt, color = textcol, background = cols_cvd, monospace = TRUE, align = "c", extra_css = "border-radius: 0px; width: 100%; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;")

		fz = switch(text.format, hex = "height: 1.5em; font-size: 80%;", "height: 1.5em; font-size: 80%;")

		e2[[cn]] = kableExtra::cell_spec(txt, color = textcol, background = cols_cvd, monospace = TRUE, align = "c", extra_css = paste0("border-radius: 0px; max-width: 18em; display:block; white-space: nowrap; overflow: auto; text-overflow: ellipsis; ", fz))


	}


	rownames(e2) = NULL

	tooltip_cbfriendly = if (is.null(n)) "Colorblind-friendly!" else paste0("Colorblind-friendly! (at least, for n = ", n, ")")
	tooltip_highC = "Watch out for those intense colors!"

	tooltip_RH = "Spectral (&#34;rainbow&#34;) palette: easy to distinguish colors, but less suitable for quantitative analysis"
	tooltip_SH_seq = "Single hue palette: good for quantitative analysis, but harder to distinguish colors"
	tooltip_SH_div = "Each side has its own distinct hue: recommended!"
	tooltip_Harm = "Harmonic, well-balanced colors"

	if ("cbfriendly" %in% qn) e2[[.labels["cbfriendly"]]] = ifelse(!is.na(e2[[.labels["cbfriendly"]]]) & e2[[.labels["cbfriendly"]]] == 1L, kableExtra::cell_spec("&#9786;", tooltip = tooltip_cbfriendly, escape = FALSE), "")
	if ("highC" %in% qn) e2[[.labels["highC"]]] = ifelse(!is.na(e2[[.labels["highC"]]]) & e2[[.labels["highC"]]] == 1L, kableExtra::cell_spec("&#x1f576;", tooltip = tooltip_highC, escape = FALSE), "")


	if ("hueType" %in% qn){
		lab = .labels["hueType"]
		if (type == "seq") {
			e2[[lab]] = ifelse(!is.na(e2[[lab]]) & e2[[lab]] == "RH", kableExtra::cell_spec("&#127752;", tooltip = tooltip_RH, escape = FALSE, extra_css = "font-size: 150%; vertical-align: -0.1em; line-height: 0px;"),
							   ifelse(!is.na(e2[[lab]]) & e2[[lab]] == "SH", kableExtra::cell_spec("&#128396;", tooltip = tooltip_SH_seq, escape = FALSE, extra_css = "font-size: 200%; vertical-align: -0.2em; line-height: 0px;"), ""))
		} else if (type == "div") {
			e2[[lab]] = ifelse(!is.na(e2[[lab]]) & e2[[lab]] == "RH", kableExtra::cell_spec("&#127752;", tooltip = tooltip_RH, escape = FALSE, extra_css = "font-size: 150%; vertical-align: -0.1em; line-height: 0px;"),
							   ifelse(!is.na(e2[[lab]]) & e2[[lab]] == "SH", kableExtra::cell_spec("&#x262F;", tooltip = tooltip_SH_div, escape = FALSE, extra_css = "font-size: 200%; vertical-align: -0.2em; line-height: 0px;"), ""))
		}
	}

	if ("harmonic" %in% qn) {
		hlab = .labels["harmonic"]
		e2[[hlab]] = ifelse(!is.na(e2[[hlab]]) & e2[[hlab]],
							kableExtra::cell_spec("&#127900;", tooltip = tooltip_Harm, escape = FALSE, extra_css = "font-size: 150%; vertical-align: -0.1em; line-height: 0px;"), "")

	}





	ql_icons = intersect(ql, c(.labels[c("cbfriendly", "highC")]))
	ql_other = setdiff(ql, c(.labels[c("cbfriendly", "highC")]))

	for (q in ql_other) {
		e2[[q]] = as.character(e2[[q]])
		e2[[q]][is.na(e2[[q]])] = ""
	}

	e2cols = c("series", "label", ql, colNames, "Copy1", "Copy2")
	e2nms = c("Series", "Name", ql, colNames, " ", " ")

	k = kableExtra::kbl(e2[, e2cols], col.names = e2nms, escape = F)

	for (cN in setdiff(colNames, " ")) {
		k = kableExtra::column_spec(k, which(cN == e2nms), width_min = "6em", width_max = "6em")
	}
	for (i in which(e2nms == " ")) {
		k = kableExtra::column_spec(k, i, width = "1em", extra_css = "padding-left: 10px; padding-right: 0px; text-align: right") #width_min = "1em", width_max = "1em")
	}

	k = kableExtra::column_spec(k, 1, width = "5em", extra_css = "padding-left: 10px; padding-right: 10px; text-align: right")
	k = kableExtra::column_spec(k, 2, width = "5em", extra_css = "padding-left: 0px; padding-right: 10px; text-align: right")
	k = kableExtra::column_spec(k, which(e2cols == "Copy"), width = "2em", extra_css = "padding-left: 10px; padding-right: 0px; text-align: right")
	k = kableExtra::row_spec(k, 0, align = "c", extra_css = "padding-left: 3px; padding-right: 3px; vertical-align: bottom") #max-width: 5em;

	for (q in ql_other) {
		k = kableExtra::column_spec(k, which(q == e2nms), width = "3em", extra_css = "padding-right: 20px; text-align: right")
	}

	for (q in ql_icons) {
		k = kableExtra::column_spec(k, which(q == e2nms), extra_css = "font-size: 250%; line-height: 40%; vertical-align: center; text-align: center; white-space: nowrap;", width = "2em")
	}

	kc = k[1]

	kl = strsplit(kc, "\n")[[1]]
	trIDs = which(kl == "  <tr>")[-1]
	rws1 = trIDs[e2$ind==1] # first line per palette
	rws2 = trIDs[e2$ind!=1] # other lines
	#browser()
	kl[rws1] = paste0("  <tr style=\"height: ", e2$row_h[e2$ind==1], "em;vertical-align:center;max-height: 10px; overflow-y: auto;\">")
	kl[rws2] = paste0("  <tr style=\"height: ", e2$row_h[e2$ind!=1], "em;vertical-align:top;\">")
	#kl[rws[-1]] = paste0("  <tr style=\"vertical-align:top;\">")

	extra = c("<style>", "table {", "\tborder-collapse: collapse;", "\tfont-size: 12px;",
			  "\tborder-collapse: collapse;", "}", "", "td {", "\tborder-radius: 0px;",
			  "\tborder: 1px solid white;", "\tborder-collapse: collapse;",
			  "}", "", "th {", "\ttext-align: center;", "}", "",
			  "a:link {color: black;}", "</style>"
	) # extra = readLines("build/extra.txt"); dput(extra)

	k[1] = paste(c(extra,kl), collapse="\n")
	#k[1] = paste(kl, collapse="\n")
	k

}



