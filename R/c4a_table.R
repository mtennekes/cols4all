table_columns = function(type, show.scores) {
	if (type %in% c("seq", "div")) {
		qn = character(0)
		srt = character(0)
	} else {
		qn = "nmax"
		srt = "nmax"
	}

	qn = c(qn, "cbfriendly", "chroma", "harmony")
	srt = c(srt, "cbfriendly", "Cmax", "harmonyRank")

	if (type %in% c("seq", "div", "bivs", "bivd", "bivg")) {
		qn = c(qn, "hueType", "contrastWT", "contrastBK", "float")
		srt = c(srt, {if (type %in% c("div", "bivs", "bivd", "bivg")) "HwidthLR" else "Hwidth"}, "CRwt", "CRbk", "Blues")
	} else {
		qn = c(qn, "contrast", "contrastWT", "contrastBK", "float")
		srt = c(srt, "CRmin", "CRwt", "CRbk", "Blues")
	}

	if (show.scores) {
		qn = c(qn, .C4A$indicators[[type]], .C4A$hcl, .C4A$rgb)
		srt = c(srt, .C4A$indicators[[type]], .C4A$hcl, .C4A$rgb)
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
#' }
#'
#' Color-blind friendliness scores are calculated as:
#'
#' * `"cat"` `min_step`
#' * `"seq"` `min_step` - `max_step` / 1000
#' * `"div"` min(`inter_wing_dist`, `min_step` * 2)
#'
#' Note: these formulas are in development, and may change in the near future. Suggestions are welcome (via github issues).
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
#' @param type type of palette: `"cat"` for categorical (aka qualitative), `"seq"` for sequential, `"div"` for diverging, and `"bivs"`/`"bivc"`/`"bivd"`/`"bivg"` for bivariate (seq-seq, seq-cat, seq-div, and uncertainty-seq). For `c4a_gui` it only determines which type is shown initially.
#' @param n,m for univariate palettes, `n` is the number of displayed colors. For bivariate palettes `"biv"`, `n` and `m` are the number of columns and rows respectively. If omitted: for `"cat"` the full palette is displayed, for `"seq"` and `"div"`, 9 colors, and for `"bivs"`/`"bivc"`/`"bivd"`/`"bivg"` 4 columns and rows. For `c4a_gui` it only determines which number of colors initially.
#' @param cvd.sim color vision deficiency simulation: one of `"none"`, `"deutan"`, `"protan"`, `"tritan"`
#' @param sort column name to sort the data. For column names, see details. Use a `"-"` prefix to reverse the order.
#' @param text.format The format of the text of the colors. One of `"hex"`, `"RGB"` or `"HCL"`.
#' @param text.col The text color of the colors. By default `"same"`, which means that they are the same as the colors themselves (so invisible, but available for selection). `"auto"` means automatic: black for light colors and white for dark colors.
#' @param series Series of palettes to show. See \code{\link{c4a_series}} for options. By default, `"all"`, which means all series. For `c4a_gui` it only determines which series are shown initially.
#' @param range vector of two numbers that determine the range that is used for sequential and diverging palettes. Both numbers should be between 0 and 1. The first number determines where the palette begins, and the second number where it ends. For sequential palettes, 0 means the leftmost (normally lightest) color, and 1 the rightmost (often darkest) color. For diverging palettes, 0 means the middle color, and 1 both extremes. If only one number is provided, this number is interpreted as the endpoint (with 0 taken as the start). By default, it is set automatically, based on `n`. See `c4a_gui`, or the internal functions `cols4all::default_range_seq` and `cols4all::default_range_div` to see what the automatic values are.
#' @param include.na should color for missing values be shown? `FALSE` by default
#' @param show.scores should scores of the quality indicators be printed? See details for a description of those indicators.
#' @param columns number of columns. By default equal to `n` or, if not specified, 12. Cannot be higher than the palette
#' @param verbose should messages and warnings be printed?
#' @import colorspace abind
#' @importFrom grDevices hcl
#' @importFrom stats lm predict
#' @importFrom utils tail
#' @example ./examples/c4a_table.R
#' @seealso References of the palettes: \code{\link{cols4all-package}}.
#' @export
#' @rdname c4a_gui
#' @name c4a_gui
c4a_table = function(type = c("cat", "seq", "div", "bivs", "bivc", "bivd", "bivg"), n = NULL, m = NULL, cvd.sim = c("none", "deutan", "protan", "tritan"), sort = "name", text.format = "hex", text.col = "same", series = "all", range = NA, include.na = FALSE, show.scores = FALSE, columns = NA, verbose = TRUE) {
	id = NULL

	#if (length(series) == 2) browser()

	if (!requireNamespace("kableExtra")) stop("Please install kableExtra")

	.labels = .C4A$labels

	type = match.arg(type)
	show.ranking = (!is.null(n))
	cvd.sim = match.arg(cvd.sim)

	if (substr(type, 1, 3) == "biv") {
		if (is.null(n)) n = 3
		if (is.null(m)) m = n
	} else {
		m = 1
	}


	if (is.na(columns)) columns = if (!is.null(n)) n else 12

	# palettes for selected type, n colors, and optionally for specific series
	z = .C4A$z

	if (is.null(z)) {
		if (verbose) message("No palette series loaded. Please reload cols4all, add series with c4a_palettes_add, or import data with c4a_sysdata_import")
		return(invisible(NULL))
	}

	zn = get_z_n(z[z$type == type, ], n = n, m = m, range = range)
	if (!is.null(zn)) {
		if (!series[1] == "all") zn = zn[zn$series %in% series, ]
	}

	if (is.null(zn) || nrow(zn) == 0) {
		if (verbose) message("No palettes of type \"", type, "\"", ifelse(is.null(n), "", paste0(" and length ", n)), " found")
		return(invisible(NULL))
	}

	zn = show_attach_scores(zn)
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

	zn$nlines = ((zn$n * m -1) %/% columns) + 1

	if (substr(type, 1, 3) == "biv") {
		zn$palette = lapply(zn$palette, function(p) as.vector(t(p[nrow(p):1L,])))
	}

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
	#colnames(e)[match(qn, colnames(e))] = ql

	# total number of columns
	tot = max(c(zn$n*m, columns))
	if (columns < tot) tot = (((tot-1) %/% columns) + 1) * columns

	# maximum number of lines per palette
	ml = ceiling(tot / columns)

	# color matrix
	cm = local({
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
			cm[e$did[i], sid[[e$ind[i]]]]
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

	add_link = function(icon, ref, fun, tooltip) {
		txt1 = rep(icon, nrow(e2))
		txt1[e2$ind !=1 ] = ""
		links1 = sapply(1:nrow(e2), function(rw) {
			if (txt1[rw] == "") {
				""
			} else{
				did = e2$did[rw]
				paste0("javascript:navigator.clipboard.writeText(`", do.call(fun, list(x = ref[[did]])), "`)")
			}
		}, USE.NAMES = FALSE)
		kableExtra::cell_spec(txt1, link=links1, tooltip=tooltip, escape = FALSE, extra_css = "text-decoration: none; color: #B4B4B4;")
	}

	e2[['Copy1']] = add_link("©", palList, function(x) paste0("[&quot;", paste0(x, collapse = "&quot;, &quot;"), "&quot;]"), tooltip='Copy colors: [&quot;#111111&quot;, &quot;#222222&quot;]')
	e2[['Copy2']] = add_link("R", palList, function(x) paste0("c(&quot;", paste0(x, collapse = "&quot;, &quot;"), "&quot;)"), tooltip='Copy colors to R: c(&quot;#111111&quot;, &quot;#222222&quot;)')
	e2[['Copy3']] = add_link("📖", zn$cit, function(x) x, tooltip='Copy reference')
	e2[['Copy4']] = add_link("B", zn$bib, function(x) x, tooltip='Copy BibTex reference')




	for (cn in colNames) {
		cols = e2[[cn]]
		sel = (!is.na(cols) & cols != "")
		cols[!sel] = ""
		cols_cvd = cols
		cols_cvd[sel] = sim_cvd(cols[sel], cvd.sim)

		textcol = if (text.col == "same") {
			cols_cvd
		} else if (text.col == "auto") {
			tmp = cols_cvd
			if (any(sel)) tmp[sel] = ifelse(get_hcl_matrix(cols_cvd[sel])[,3]>=50, "#000000", "#FFFFFF")
			tmp
		} else {
			text.col
		}

		txt = cols
		if (any(sel)) txt[sel] = switch(text.format, hex = cols[sel], RGB = get_rgb_triple(cols[sel]), HCL = get_hcl_triple(cols[sel]))

		#e2[[cn]] = kableExtra::cell_spec(txt, color = textcol, background = cols_cvd, monospace = TRUE, align = "c", extra_css = "border-radius: 0px; width: 100%; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;")

		fz = switch(text.format, hex = "height: 1.5em; font-size: 80%;", "height: 1.5em; font-size: 80%;")

		e2[[cn]] = kableExtra::cell_spec(txt, color = textcol, background = cols_cvd, monospace = TRUE, align = "c", extra_css = paste0("border-radius: 0px; max-width: 18em; display:block; white-space: nowrap; overflow: auto; text-overflow: ellipsis; ", fz))


	}


	rownames(e2) = NULL
	for (var in c("cbfriendly", "chroma",  "hueType", "harmony", "contrast", "contrastWT", "contrastBK", "float")) {
		tcv = .C4A$tc[[var]]
		if (any(names(tcv) %in% c("seq", "cat", "div"))) tcv = tcv[[type]]
		if (var %in% qn) {
			chr = as.character(e2[[var]])
			chr[is.na(chr)] = "NA"
			e2[[var]] = tcv[chr]
		}
	}


	all_icons = c("cbfriendly", "chroma", "harmony")

	qn_icons = intersect(qn, all_icons)
	qn_other = setdiff(qn, all_icons)

	for (q in qn_other) {
		e2[[q]] = as.character(e2[[q]])
		e2[[q]][is.na(e2[[q]])] = ""
	}

	e2cols = c("series", "label", qn, colNames, "Copy1", "Copy2", "Copy3", "Copy4")
	e2nms = c("Series", "Name", ql, colNames, "", "", "", "")

	dupl = e2cols[e2nms %in% e2nms[duplicated(e2nms)]]

	e2nms[duplicated(e2nms)] = ""

	k = kableExtra::kbl(e2[, e2cols], col.names = e2nms, escape = F)

	for (cN in colNames) {
		k = kableExtra::column_spec(k, which(cN == e2nms), width_min = "6em", width_max = "6em")
	}
	for (i in which(substr(e2cols, 1, 4) == "Copy")) {
		k = kableExtra::column_spec(k, i, width = "1em", extra_css = "padding-left: 10px; padding-right: 0px; text-align: right") #width_min = "1em", width_max = "1em")
	}

	k = kableExtra::column_spec(k, 1, width = "5em", extra_css = "padding-left: 10px; padding-right: 10px; text-align: right")
	k = kableExtra::column_spec(k, 2, width = "5em", extra_css = "padding-left: 0px; padding-right: 10px; text-align: right")
	k = kableExtra::column_spec(k, which(substr(e2cols, 1, 4) == "Copy"), width = "1em", extra_css = "padding-left: 5px; padding-right: 0px; text-align: right")
	k = kableExtra::row_spec(k, 0, align = "c", extra_css = "padding-left: 3px; padding-right: 3px; vertical-align: bottom; max-width: 0em;") #max-width: 5em;

	for (q in qn_other) {
		if (q %in% dupl) {
			k = kableExtra::column_spec(k, which(q == e2cols), width = "2em", extra_css = "text-align: center; vertical-align: center; overflow: hidden; text-overflow: ellipsis; max-width: 2em; min-width: 2em;")
		} else {
			k = kableExtra::column_spec(k, which(q == e2cols), width = "4em", extra_css = "text-align: center; vertical-align: center; overflow: hidden; text-overflow: ellipsis; max-width: 4em; min-width: 4em;")
		}
	}

	for (q in qn_icons) {
		k = kableExtra::column_spec(k, which(q == e2cols), extra_css = "font-size: 200%; line-height: 40%; vertical-align: center; text-align: center; white-space: nowrap; max-width: 2.2em; min-width: 2.2em;", width = "2.2em")
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
			  "\tborder-top: 1px solid #ffffff00;", "\tborder-bottom: 1px solid #ffffff00;", "\tborder-left: 0px solid #ffffff00;", "\tborder-right: 2px solid #ffffff00;", "\tborder-collapse: collapse;",
			  "}", "", "th {", "\ttext-align: center;", "}", "",
			  "a:link {color: black;}", "</style>"
	) # extra = readLines("build/extra.txt"); dput(extra)

	k[1] = paste(c(extra,kl), collapse="\n")
	#k[1] = paste(kl, collapse="\n")
	k

}



