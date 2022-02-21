table_columns = function(type, show.scores) {
	if (type == "cat") {
		qn = c("nmax", "cbfriendly", "highC")
		ql = c(.maxn, .friendly, .highC)
		srt = c("nmax", "rank", "Cmax")
	} else {
		qn = c("cbfriendly", "highC")
		ql = c(.friendly, .highC)
		srt = c("rank", "Cmax")
	}

	if (type %in% c("seq", "div")) {
		qn = c(qn, "hueType")
		ql = c(ql, .hueType)
		srt = c(srt, {if (type == "div") "HwidthLR" else "Hwidth"})
	} else {
		qn = c(qn, "harmonic")
		ql = c(ql, .harmonic)
		srt = c(srt, "LCrange")
	}

	qn = c(qn, "rank")
	ql = c(ql, .rank)
	srt = c(srt, "rank")

	if (show.scores) {
		qn = c(qn, .indicators[[type]], .hcl)
		ql = c(ql, .labels[c(.indicators[[type]], .hcl)])
		srt = c(srt, .indicators[[type]], .hcl)
	}
	list(qn = qn, ql = ql, srt = srt)
}


#' Graphical user interface to analyse palettes
#'
#' Graphical user interface to analyse palettes. `c4a_show` shows a table that can be opened in the browser. `c4a_gui` is a graphical user interface around this table.
#'
#' The following table describes the main columns. A more precise description is provided after that
#'
#' | Label | Name | Description
#' | --- | --- | --- | ---
#' | Max n | `"nmax"` | Maximum number of colors (`"cat"` only)
#' | Colorblind-friendly | `"cbfriendly"` | Is it color-blind friendy?
#' | Harmonic palette | `"harmonic"` | Are the colors in harmony with each other?
#' | Intense colors | `"highC` | Are there any intense (saturated) colors?
#' | Hue type | `"hueType"` | How many different hue ranges are used? For a sequential (`"seq"`) palette we consider three classes. 1) "single hue" where one hue is used, which is recommended for quantitative analysis. This is indicated by a paint brush icon 2) "spectral hue" where a wide range of hues are used, e.g. a rainbow palette. This less suitable for quantitative analysis but better to read different colors. This is indicated by a rainbow icon 3) a trade-off between the two mentioned classes (no icon used). For a diverging (`"div"`) palette, we also consider similar three classes. 1) "two hues", where one hue is used for the left wing and one for the right wing. 2) "spectral hue" and 3) trade-off.
#' | Ranking | `"rank"` | Ranking of palettes taking the above into account
#'
#' The following table are the quality indicators that have been applied to determine color-blind friendliness. These indicators are visible when `show.scores` is set to `TRUE`.
#
#' | Label | Name | Description
#' | --- | --- | --- | ---
#' | Minimum distance | `"min_dist"` | Minimum distance between any two colors for any color vision deficiency type. This is a measure to which extend categorical palettes (type `"cat"`) are suitable for people with color vision deficiency
#' | Minimum step | `"min_step"` | Minimum distance between two neighboring colors in a sequential (`"seq"`) or diverging (`"div"`) palette, for any color vision deficiency type. The larger, the better.
#' | Maximum step | `"max_step"` | Maximum distance between two neighboring colors in a sequential (`"seq"`) palette, for any color vision deficiency type. For sequential palettes that score the same on `"min_step"`, the ones with lower `"max_step"` values are slightly preferable, because this means that the distances between neighboring colors is more homogeneous.
#' | Inter-wing-distance | `"inter_wing_dist"` | Minimum distance between any color in the left wing to any color in the right wing of a diverging (`"div"`) palette, for any color vision deficiency type. The larger, the better.
#' | Inter-wing hue distance | `"inter_wing_hue_dist"` | Distance between the two hue ranges in both wings of a (`"div"`) palette, for any color vision deficiency type. The larger the better. We consider 100 degrees as sufficient to discriminate two hues.
#'
#' Color-blind friendliness scores are calculated as:
#'
#' * `"cat"` `min_step`
#' * `"seq"` `min_step` - `max_step` / 1000
#' * `"div"` min(`inter_wing_dist`, `min_step`) + (`inter_wing_hue_dist` >= 100) * 1000
#'
#' The following table describes the indicators that we used to determine whether we call palette is harmonic, and whether there are intense colors. We use the HCL color space, where H is the hue, C the chroma and L the lightness. See the `colorspace` package (that is used under the hood) for details.
#'
#' | Label | Name | Description
#' | --- | --- | --- | ---
#' | Chroma max | `"Cmax"` | Maximum chroma value. We have set the threshold for the label "intense colors" at 100.
#' | Hue width | `"Hwidth"` | The width/range of hue values that are used. For instance, if a palette has hue values 100, 140, and 220, the Hwidth is 120. Since hues are provided in degrees, a hue width close to 360 means that many hues are used (e.g. in a rainbow palette). The primary use is to determine the hue type (see above).
#' | Hue width L | `"HwidthL"` | Same, but only for the left wing of the palette (useful for diverging palettes)
#' | Hue width R | `"HwidthR"` | Same, but only for the right wing of the palette (useful for diverging palettes)
#' | Luminance range | `"Lrange"` | The range of luminance values of the colors. The smaller, the better for a what we call harmonic palette. However, this is at the expense of having distinguishable colors (which contribute to color-blind-friendliness).
#' | Chroma range | `"Crange` | The range of chroma values of the colors. Like `"Lrange"`, the lower the better.
#' | Lum/Chr range | `"LCrange"` | Defined as max(2 * `Lrange`, `Crange`), and used to label a palette "harmonic". This formula is determined by some trial-and-error, so suggestions for improvement are welcome.
#'
#' @param type type of palette: `"cat"` for categorical (aka qualitative), `"seq"` for sequential, and `"div"` for diverging
#' @param n number of colors. If omitted: for `"cat"` the full palette is displayed, and for `"seq"` and `"div"`, 9 colors.
#' @param cvd.sim color vision deficiency simulation: one of `"none"`, `"deutan"`, `"protan"`, `"tritan"`
#' @param sort column name to sort the data. For column names, see details
#' @param text.col The text color of the colors. By default `"same"`, which means that they are the same as the colors themselves (so invisible, but available for selection).
#' @param series Series of palettes to show. See \code{\link{c4a_series}} for options.
#' @param contrast vector of two numbers that determine the range that is used for sequential and diverging palettes. Both numbers should be between 0 and 1. The first number determines where the palette begins, and the second number where it ends. For sequential palettes, 0 means the leftmost (normally lightest) color, and 1 the rightmost (often darkest) color. For diverging palettes, 0 means the middle color, and 1 both extremes. If only one number is provided, this number is interpreted as the endpoint (with 0 taken as the start). By default, it is set automatically, based on `n`. See `c4a_gui`, or the internal functions `cols4all::default_contrast_seq` and `cols4all::default_contrast_div` to see what the automatic values are.
#' @param include.na should color for missing values be shown? `FALSE` by default
#' @param show.scores should scores of the quality indicators be printed? See details for a description of those indicators.
#' @param columns number of columns. By default equal to `n` or, if not specified, 12. Cannot be higher than the palette
#' @import kableExtra
#' @import colorspace
#' @example ./examples/c4a_show.R
#' @export
#' @rdname c4a_gui
#' @name c4a_gui
c4a_show = function(type = c("cat", "seq", "div"), n = NULL, cvd.sim = c("none", "deutan", "protan", "tritan"), sort = "name", text.col = "same", series = NULL, contrast = NULL, include.na = TRUE, show.scores = FALSE, columns = NA) {

	type = match.arg(type)
	show.ranking = (!is.null(n))
	cvd.sim = match.arg(cvd.sim)
	if (is.na(columns)) columns = if (!is.null(n)) n else 12

	# palettes for selected type, n colors, and optionally for specific series
	z = get(".z", envir = .C4A_CACHE)

	zn = get_z_n(z[z$type == type, ], n = n, contrast = contrast)


	zn = attach_scores(zn, contrast = contrast)
	# better but slower alternative: calculate all scores read time:
	#zn = get_scores_zn(zn)
	if (!is.null(series)) zn = zn[zn$series %in% series, ]

	columns = min(columns, max(zn$n))

	k = nrow(zn)

	res = table_columns(type, show.scores)
	qn = res$qn
	ql = res$ql
	srt = res$srt

	sortCol = if (sort == "name") "fullname" else srt[which(sort == qn)]

	decreasing = !(sortCol %in% c("fullname", "rank"))
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
		row_h = ifelse(ind==indx, 25, 12)
	})
	e = cbind(e, zn[match(e$label, zn$name),qn,drop=FALSE])
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
		me = cbind(me, ' '="", 'NA' = "")
		me[match(1:k, e$did), ncol(me)] = zn$na
		colNames = c(1:columns, " ", "NA")
	} else {
		colNames = as.character(1:columns)
	}

	e2 = cbind(e, me)

	sim = switch(cvd.sim,
				 none = function(x) x,
				 deutan = colorspace::deutan,
				 protan = colorspace::protan,
				 tritan = colorspace::tritan)


	for (cn in colNames) {
		cols = e2[[cn]]
		cols[is.na(cols)] = ""
		cols_cvd = cols
		cols_cvd[cols_cvd != ""] = sim(cols[cols_cvd != ""])
		textcol = if (text.col == "same") cols_cvd else text.col
		e2[[cn]] = kableExtra::cell_spec(cols, color = textcol, background = cols_cvd, monospace = TRUE, align = "c", extra_css = "border-radius: 0px; width: 100%; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;")
	}


	rownames(e2) = NULL

	tooltip_cbfriendly = if (is.null(n)) "Colorblind-friendly!" else paste0("Colorblind-friendly! (at least, for n = ", n, ")")
	tooltip_highC = "Watch out for those intense colors!"

	tooltip_RH = "Spectral (&#34;rainbow&#34;) palette: easy to distinguish colors, but less suitable for quantitative analysis"
	tooltip_SH_seq = "Single hue palette: good for quantitative analysis, but harder to distinguish colors"
	tooltip_SH_div = "Each side has its own distinct hue: recommended!"
	tooltip_Harm = "Harmonic, well-balanced colors"

	if (.friendly %in% ql) e2[[.friendly]] = ifelse(!is.na(e2[[.friendly]]) & e2[[.friendly]] == 1L, kableExtra::cell_spec("&#9786;", tooltip = tooltip_cbfriendly, escape = FALSE), "")
	if (.highC %in% ql) e2[[.highC]] = ifelse(!is.na(e2[[.highC]]) & e2[[.highC]] == 1L, kableExtra::cell_spec("&#x1f576;", tooltip = tooltip_highC, escape = FALSE), "")


	if (.hueType %in% ql){
		if (type == "seq") {
			e2[[.hueType]] = ifelse(!is.na(e2[[.hueType]]) & e2[[.hueType]] == "RH", kableExtra::cell_spec("&#127752;", tooltip = tooltip_RH, escape = FALSE, extra_css = "font-size: 150%; vertical-align: -0.1em; line-height: 0px;"),
				ifelse(!is.na(e2[[.hueType]]) & e2[[.hueType]] == "SH", kableExtra::cell_spec("&#128396;", tooltip = tooltip_SH_seq, escape = FALSE, extra_css = "font-size: 200%; vertical-align: -0.2em; line-height: 0px;"), ""))
		} else if (type == "div") {
			e2[[.hueType]] = ifelse(!is.na(e2[[.hueType]]) & e2[[.hueType]] == "RH", kableExtra::cell_spec("&#127752;", tooltip = tooltip_RH, escape = FALSE, extra_css = "font-size: 150%; vertical-align: -0.1em; line-height: 0px;"),
									ifelse(!is.na(e2[[.hueType]]) & e2[[.hueType]] == "SH", kableExtra::cell_spec("&#x262F;", tooltip = tooltip_SH_div, escape = FALSE, extra_css = "font-size: 200%; vertical-align: -0.2em; line-height: 0px;"), ""))
		}
	}

	if (.harmonic %in% ql) {
		e2[[.harmonic]] = ifelse(!is.na(e2[[.harmonic]]) & e2[[.harmonic]],
								 kableExtra::cell_spec("&#127900;", tooltip = tooltip_Harm, escape = FALSE, extra_css = "font-size: 150%; vertical-align: -0.1em; line-height: 0px;"), "")

	}





	ql_icons = intersect(ql, c(.friendly, .highC))
	ql_other = setdiff(ql, c(.friendly, .highC))

	for (q in ql_other) {
		e2[[q]] = as.character(e2[[q]])
		e2[[q]][is.na(e2[[q]])] = ""
	}

	e2cols = c("series", "label", ql, colNames)
	e2nms = c("Series", "Name", ql, colNames)

	k = kableExtra::kbl(e2[, e2cols], col.names = e2nms, escape = F)
#return(k)

	# for (i in (1:columns)+(length(ql)+1)) {
	# 	k = kableExtra::column_spec(k, i, extra_css = 'width: 5em; overflow: hidden; background-color: #000000"')
	# }
	# for (cN in colNames) {
	# 	k = kableExtra::column_spec(k, which(cN == e2nms), width = "10px")
	# }


	k = kableExtra::column_spec(k, 1, width = "5em", extra_css = "padding-left: 10px; padding-right: 10px; text-align: right")
	k = kableExtra::column_spec(k, 2, width = "5em", extra_css = "padding-left: 0px; padding-right: 10px; text-align: right")
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
	kl[rws1] = paste0("  <tr style=\"height: ", e2$row_h[e2$ind==1], "px;vertical-align:center;max-height: 20px; overflow-y: auto;\">")
	kl[rws2] = paste0("  <tr style=\"height: ", e2$row_h[e2$ind!=1], "px;vertical-align:top;\">")
	#kl[rws[-1]] = paste0("  <tr style=\"vertical-align:top;\">")

	css = readLines("css/table.css")

	k[1] = paste(c(css,kl), collapse="\n")
	#k[1] = paste(kl, collapse="\n")
	k

}



