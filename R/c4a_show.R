table_columns = function(type, advanced) {
	if (type == "cat") {
		qn = c("nmax", "rank", "cbfriendly", "highC")
		ql = c(.maxn, .rank, .friendly, .highC)
		srt = c("nmax", "rank", "rank", "Cmax")
	} else {
		qn = c("rank", "cbfriendly", "highC")
		ql = c(.rank, .friendly, .highC)
		srt = c("rank", "rank", "Cmax")
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

	if (advanced) {
		qn = c(qn, .indicators[[type]], .hcl)
		ql = c(ql, .labels[c(.indicators[[type]], .hcl)])
		srt = c(srt, .indicators[[type]], .hcl)
	}
	list(qn = qn, ql = ql, srt = srt)
}


#' Show cols4all palettes
#'
#' Show cols4all palettes
#'
#' @param x named list of color palettes
#' @param n number of colors
#' @param columns number of columns. By default equal to `n` or, if not specified, 12. Cannot be higher than the palette
#' @param cvd.sim color vision deficiency simulation: one of `"none"`, `"deutan"`, `"protan"`, `"tritan"`
#' @param order.by.score order the palettes by score (`TRUE`, default) or by name?
#' @param text.col The text color of the colors. By default `"same"`, which means that they are the same as the colors themselves (so invisible, but available for selection).
#' @import kableExtra
#' @import colorspace
c4a_show = function(n = NULL, type = c("cat", "seq", "div", "biv"), advanced.mode = FALSE, columns = NA, cvd.sim = c("none", "deutan", "protan", "tritan"), sort = "name", text.col = "same", series = NULL, contrast = NULL, include.na = TRUE) {

	type = match.arg(type)
	show.ranking = (!is.null(n))
	cvd.sim = match.arg(cvd.sim)
	if (is.na(columns)) columns = if (!is.null(n)) n else 12

	# palettes for selected type, n colors, and optionally for specific series
	z = get(".z", envir = .C4A_CACHE)

	zn = get_z_n(z[z$type == type, ], n = n, contrast = contrast)
	zn = attach_scores(zn)
	if (!is.null(series)) zn = zn[zn$series %in% series, ]

	columns = min(columns, max(zn$n))

	k = nrow(zn)

	res = table_columns(type, advanced.mode)
	qn = res$qn
	ql = res$ql
	srt = res$srt

	sortCol = if (sort == "name") "name" else srt[which(sort == qn)]

	decreasing = !(sortCol %in% c("name", "rank"))
	zn = zn[order(zn[[sortCol]], decreasing = decreasing), ]

	zn$nlines = ((zn$n-1) %/% columns) + 1

	zn$Name = gsub(".*\\.", "", zn$name)

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
		e2[[cn]] = kableExtra::cell_spec(cols, color = textcol, background = cols_cvd, monospace = TRUE, align = "c", extra_css = "border-radius: 0px;")
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
	k = kableExtra::column_spec(k, 1, extra_css = "padding-left: 10px;padding-right: 10px;min-width: 120px; text-align: right")
	k = kableExtra::row_spec(k, 0, align = "c", extra_css = "max-width: 5em; vertical-align: bottom")

	for (q in ql_other) {
		k = kableExtra::column_spec(k, which(q == e2nms), extra_css = "padding-right: 20px; max-width: 10em; text-align: right")
	}

	for (q in ql_icons) {
		k = kableExtra::column_spec(k, which(q == e2nms), extra_css = "font-size: 250%; line-height: 40%; vertical-align: center; text-align: center; white-space: nowrap;", width = "3em")
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
	k

}



