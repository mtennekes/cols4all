table_columns = function(type, show.scores) {
	if (type %in% c("seq", "div", "cyc")) {
		qn = character(0)
		qs = character(0)
	} else {
		qn = "nmax"
		qs = "nmax"
	}

	qn = c(qn, "cbfriendly", "fair")
	qs = c(qs, "cbfriendly", "fairness")

	if (type == "seq") {
		qn = c(qn, "hues", "chroma", "contrastWT", "contrastBK", "float")
		qs = c(qs, "Hwidth", "Cmax", "CRwt", "CRbk", "Blues")
		sn = "H"
	} else if (type == "cyc") {
		qn = c(qn, "chroma", "contrastWT", "contrastBK", "float")
		qs = c(qs, "Cmax", "CRwt", "CRbk", "Blues")
		sn = character(0)
	} else if (type %in% c("div", "bivs", "bivd", "bivg")) {
		qn = c(qn, "chroma", "hues", "contrastWT", "contrastBK", "float")
		qs = c(qs, "Cmax",  "HwidthLR", "CRwt", "CRbk", "Blues")
		sn = c("HL", "HR", "Lmid")
	} else if (type == "cat") {
		qn = c(qn, "hues", "chroma", "contrastWT", "contrastBK", "equiluminance", "float", "nameable")
		qs = c(qs, "Hspread", "Cmax", "CRwt", "CRbk", "CRmin", "Blues", "nameability")
		sn = character(0)
	} else if (type == "bivc") {
		qn = c(qn, "chroma", "contrastWT", "contrastBK", "equiluminance", "float", "nameable")
		qs = c(qs, "Cmax", "CRwt", "CRbk", "CRmin", "Blues", "nameability")
		sn = character(0)
	} else {
		stop("Unknown type")
	}


	if (show.scores) {
		qn = c(qn, names(.C4A$CB_ranges[[type]]), .C4A$hcl2, .C4A$rgb)
		qs = c(qs, names(.C4A$CB_ranges[[type]]), .C4A$hcl2, .C4A$rgb)
	}
	ql = gsub("&nbsp;", "", .C4A$labels[qn])

	sl = .C4A$labels[sn]

	list(qn = qn, ql = ql, qs = qs, sn = sn, sl = sl)
}

prep_table = function(type = c("cat", "seq", "div", "cyc", "bivs", "bivc", "bivd", "bivg"), n = NULL, m = NULL, sort = "name", series = "all", filters = character(0), range = NA, colorsort = "orig", show.scores = FALSE, columns = NA, verbose = TRUE, continuous = FALSE) {
	id = NULL

	type = match.arg(type)

	if (!type %in% c("seq", "div", "cyc")) continuous = FALSE

	if (continuous) {
		#n = 15
		columns = n
		#columns = 15
	} else if (is.null(n)) {
		n = if (type == "cat") {
			7
		} else if (type == "bivc") {
			3
		} else .C4A$ndef[type]

	}

	#if (length(series) == 2) browser()

	check_installed_packages("kableExtra")

	.labels = .C4A$labels


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
		if (verbose) message("No palette series loaded. Please reload cols4all, load palette data with c4a_load or c4a_sysdata_import")
		return(invisible(NULL))
	}

	zn = get_z_n(z[z$type == type, ], n = n, m = m, filters = filters, range = range, colorsort = colorsort)
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
	qs = res$qs
	sn = res$sn
	sl = res$sl

	#xn = c(qn, sn)
	#xl = c(ql, sl)

	isrev = (substr(sort, 1, 1) == "-")
	if (isrev) sort = substr(sort, 2, nchar(sort))


	sortCol = if (sort == "name") {
		"fullname"
	} else if (sort %in% c("H", "HL", "HR", "Lmid")) {
		sort
	} else qs[which(sort == qn)]

	if (length(sortCol) == 0L) {
		warning("unknown sort value, available options: \"name\", \"H\", \"HL\", \"HR\", \"Lmid\", \"", paste(qn, collapse = "\", \""), "\"")
		sortCol = "fullname"
	}

	# special case: for "div" palette sorting with HL or HR depends on colorsort: in the latter is "H" swap HL and HR
	if (type == "div" && sortCol %in% c("HL", "HR") && colorsort == "H") {
		ps = zn$palette
		is_rev = vapply(ps, div_rev, FUN.VALUE = logical(1), USE.NAMES = FALSE)
		if (any(is_rev)) {
			dummy = zn$HL[is_rev]
			zn$HL[is_rev] = zn$HR[is_rev]
			zn$HR[is_rev] = dummy
		}
	}

	decreasing = xor(isrev, sortCol %in% .C4A$sortRev)
	zn = zn[order(zn[[sortCol]], decreasing = decreasing), ]

	zn$nlines = ((zn$n * m -1) %/% columns) + 1

	if (substr(type, 1, 3) == "biv") {
		#zn$palette = lapply(zn$palette, function(p) as.vector(t(p[nrow(p):1L,])))
		zn$palette = lapply(zn$palette, function(p) as.vector(t(p)))
	}
	list(zn = zn, n = n, m = m, columns = columns, continuous = continuous, type = type, qn = qn, ql = ql)
}

plot_table = function(p, text.format, text.col, include.na, cvd.sim, verbose) {
	type = p$type
	zn = p$zn
	qn = p$qn
	ql = p$ql

	n = p$n
	m = p$m
	columns = p$columns
	continuous = p$continuous

	k = nrow(zn)

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
	if (continuous) {
		me = matrix("", nrow = k, ncol = 1) # dummy
		colnames(me) = 1
		columns = 1
	} else {
		me = local({
			sid = split(1:tot, f = rep(1:ml, each = columns, length.out = tot))
			x = do.call(rbind, lapply(1:nrow(e), function(i) {
				cm[e$did[i], sid[[e$ind[i]]]]
			}))
			colnames(x) = 1:ncol(x)
			x
		})
	}

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
		kableExtra::cell_spec(txt1, link=links1, tooltip=tooltip, escape = FALSE, extra_css = "font-size: 80%; text-decoration: none; color: #A1A1A1;")
	}
	e2[['Copy1']] = add_link("&#128214;", zn$cit, function(x) x, tooltip='Copy reference (APA style)')
	e2[['Copy2']] = add_link("Bib", zn$bib, function(x) x, tooltip='Copy BibTex reference')
	e2[['Copy3']] = add_link("JS", palList, function(x) paste0("[&quot;", paste0(x, collapse = "&quot;, &quot;"), "&quot;]"), tooltip='Copy colors to standard format (e.g used by Javascript and Python): [&quot;#111111&quot;, &quot;#222222&quot;]')
	e2[['Copy4']] = add_link("R", palList, function(x) paste0("c(&quot;", paste0(x, collapse = "&quot;, &quot;"), "&quot;)"), tooltip='Copy colors to R: c(&quot;#111111&quot;, &quot;#222222&quot;)')




	for (cn in colNames) {
		if (continuous && cn == "1") {
			if (cvd.sim != "none") cm = apply(cm, MARGIN = 2, sim_cvd, cvd = cvd.sim)

			css = apply(cm, MARGIN = 1, function(cols) {
				paste0("border-radius: 0px; display:block; white-space: nowrap; overflow: auto; text-overflow: ellipsis; ",
					   "background-position: right 50px;",
					   #"visibility:hidden",
					   #"font-size:1%;",
					   "height: 1.5em; font-size: 80%;",
					   "background-image: linear-gradient(to right, ", paste0(cols, collapse = ", "), ");")
			})

			textcol = apply(cm, MARGIN = 2, function(cols) {
				paste0(cols, collapse = "; ")
			})


			e2[[cn]] = kableExtra::cell_spec("&nbsp;", monospace = TRUE, align = "c", extra_css = css, escape = FALSE)



		} else {
			cols = e2[[cn]]
			sel = (!is.na(cols) & cols != "")
			cols[!sel] = ""
			cols_cvd = cols
			cols_cvd[sel] = sim_cvd(cols[sel], cvd.sim)

			textcol = if (text.col == "same") {
				cols_cvd
			} else if (text.col == "auto") {
				tmp = cols_cvd
				#if (any(sel)) tmp[sel] = ifelse(get_hcl_matrix(cols_cvd[sel])[,3]>=50, "#000000", "#FFFFFF")
				if (any(sel)) tmp[sel] = ifelse(is_light(cols_cvd[sel]), "#000000", "#FFFFFF")
				tmp
			} else {
				text.col
			}

			txt = cols
			if (any(sel)) txt[sel] = switch(text.format, hex = cols[sel], RGB = get_rgb_triple(cols[sel]), HCL = get_hcl_triple(cols[sel]))

			#e2[[cn]] = kableExtra::cell_spec(txt, color = textcol, background = cols_cvd, monospace = TRUE, align = "c", extra_css = "border-radius: 0px; width: 100%; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;")

			fz = switch(text.format, RGB = "height: 1.5em; font-size: 70%;", "height: 1.5em; font-size: 80%;")

			e2[[cn]] = kableExtra::cell_spec(txt, color = textcol, background = cols_cvd, monospace = TRUE, align = "c", extra_css = paste0("border-radius: 0px; max-width: 18em; display:block; white-space: nowrap; overflow: auto; text-overflow: ellipsis; ", fz))
		}
	}

	# make icons (cannot do that in onLoad due to dependency of suggested kableExtra)
	tc = lapply(.C4A$tc, function(tci) {
		if (any(names(tci) %in% c("seq", "cat", "div", "cyc"))) {
			lapply(tci, function(tcii) {
				lapply(tcii, function(tciii) {
					if (is.list(tciii)) do.call(kableExtra::cell_spec, tciii) else tciii
				})
			})
		} else {
			lapply(tci, function(tcii) {
				if (is.list(tcii)) do.call(kableExtra::cell_spec, tcii) else tcii
			})
		}
	})

	th = lapply(.C4A$th, function(thi) {
		do.call(kableExtra::cell_spec, thi)
	})


	rownames(e2) = NULL
	for (var in c("cbfriendly", "chroma",  "hues", "fair", "nameable", "equiluminance", "contrastWT", "contrastBK", "float")) {
		tcv = tc[[var]]
		if (any(names(tcv) %in% c("seq", "cat", "div", "cyc"))) {
			tcv = if (type %in% names(tcv)) tcv[[type]]	else tcv[["x"]]
		}
		if (var %in% qn) {
			if (var == "cbfriendly") {
				# because sorting order is encoded in the color blind friendly column (see get_friendlyness)
				chr = as.character(round(e2[[var]]))
			} else {
				chr = as.character(e2[[var]])
			}
			chr[is.na(chr)] = "NA"
			e2[[var]] = tcv[chr]
		}
	}


	all_icons = c("cbfriendly", "chroma", "fair")

	qn_icons = intersect(qn, all_icons)
	qn_other = setdiff(qn, all_icons)

	for (q in qn_other) {
		e2[[q]] = as.character(e2[[q]])
		e2[[q]][is.na(e2[[q]])] = ""
	}

	e2cols = c("series", "label", qn, colNames, "Copy1", "Copy2", "Copy3", "Copy4")
	e2nms = c(series = "Series", name = "Name", ql, colNames, references = "References", "", "", "")

	dupl = e2cols[names(e2nms) %in% c("contrastWT", "contrastBK", "equiluminance")] #e2nms %in% e2nms[duplicated(e2nms)] |

	e2nms[duplicated(e2nms)] = ""

	e2th = e2nms

	if (continuous) {
		e2th[e2th == "1"] = ""
	}


	for (i in 1:length(th)) {
		hd = names(th)[i]
		id = which(names(e2nms) == hd)
		if (length(id)) {
			e2th[id] = th[[i]]
		}
	}

	e2$series = kableExtra::cell_spec(e2$series, tooltip = unname(.C4A$zdes[e2$series]))

	k = kableExtra::kbl(e2[, e2cols], col.names = e2th, escape = F)

	# ORIGINAL
	# for (cN in colNames) {
	# 	if (cN == " ") {
	# 		# column between colors and NA-color
	# 		k = kableExtra::column_spec(k, which(cN == e2nms), width_min = "1em", width_max = "1em")
	# 	} else {
	# 		k = kableExtra::column_spec(k, which(cN == e2nms), width_min = "6em", width_max = "6em")
	# 	}
	# }

	# for (i in which(substr(e2cols, 1, 4) == "Copy")) {
	# 	k = kableExtra::column_spec(k, i, width = "1em", extra_css = "padding-left: 10px; padding-right: 0px; text-align: right") #width_min = "1em", width_max = "1em")
	# }


	# FASTER
	ks = strsplit(k[1], "<td style=\"text-align:left;\">", fixed = TRUE)[[1]]
	ins = rep("<td style=\"text-align:left;\">", length(ks) - 1L)

	col_repl = function(ins, cids, str) {
		id = cids + rep(0:(nrow(e2)-1L) * length(e2cols), each = length(cids))
		ins[id] = str
		ins
	}


	css_col_norm = "<td style=\"text-align:left; min-width: 6em; max-width: 6em;\">"
	css_col_ramp = paste0("<td style=\"text-align:left; min-width: ", 6 * n, "em; max-width: 60em;\">")



	ins2 = ins |>
		col_repl(match(colNames[colNames == " "], e2cols), "<td style=\"text-align:left; min-width: 1em; max-width: 1em;\">") |>
		col_repl(match(colNames[colNames == "Missings"], e2cols), css_col_norm) |>
		col_repl(match(colNames[colNames != " " & colNames != "Missings"], e2cols), ifelse(continuous, css_col_ramp, css_col_norm)) |>
		#col_repl(which(substr(e2cols, 1, 4) == "Copy"), "<td style=\"text-align:left; width: 1em; padding-left: 10px; padding-right: 0px; text-align: right\">") |>
		col_repl(1, "<td style=\"text-align:left; width: 5em; padding-left: 10px; padding-right: 10px; text-align: right\">") |>
		col_repl(2, "<td style=\"text-align:left; width: 5em; padding-left: 0px; padding-right: 10px; text-align: right\">") |>
		col_repl(which(substr(e2cols, 1, 4) == "Copy"), "<td style=\"text-align:left; width: 1em; padding-left: 5px; padding-right: 0px; text-align: right\">")


	for (q in qn_other) {
		if (q %in% dupl) {
			ins2 = col_repl(ins2, which(q == e2cols), "<td style=\"text-align:left; width: 2.2em; text-align: center; vertical-align: center; overflow: hidden; text-overflow: ellipsis; max-width: 2.2em; min-width: 2.2em;\">")
		} else {
			ins2 = col_repl(ins2, which(q == e2cols), "<td style=\"text-align:left; width: 4em; text-align: center; vertical-align: center; overflow: hidden; text-overflow: ellipsis; max-width: 4em; min-width: 4em;\">")
		}
	}

	for (q in qn_icons) {
		ins2 = col_repl(ins2, which(q == e2cols), "<td style=\"text-align:left; width: 2.2em; font-size: 200%; line-height: 40%; vertical-align: center; text-align: center; white-space: nowrap; max-width: 2.2em; min-width: 2.2em;\">")
	}

	# colIds1 = match(colNames[colNames != " "], e2cols) + rep(0:(nrow(e2)-1L) * length(e2cols), each = length(colNames[colNames != " "]))
	# ins[colIds1] = "<td style=\"text-align:left; min-width: 6em; max-width: 6em;\">"
	# colIds2 = match(colNames[colNames == " "], e2cols) + rep(0:(nrow(e2)-1L) * length(e2cols), each = length(colNames[colNames == " "]))
	# ins[colIds2] = "<td style=\"text-align:left; min-width: 1em; max-width: 1em;\">"

	k[1] = paste(c(ks[1], unlist(mapply(c, ins2, ks[-1], SIMPLIFY = FALSE, USE.NAMES = FALSE))), collapse = "")


	# k = kableExtra::column_spec(k, 1, width = "5em", extra_css = "padding-left: 10px; padding-right: 10px; text-align: right")
	# k = kableExtra::column_spec(k, 2, width = "5em", extra_css = "padding-left: 0px; padding-right: 10px; text-align: right")
	# k = kableExtra::column_spec(k, which(substr(e2cols, 1, 4) == "Copy"), width = "1em", extra_css = "padding-left: 5px; padding-right: 0px; text-align: right")
	k = kableExtra::row_spec(k, 0, align = "c", extra_css = "padding-left: 3px; padding-right: 3px; vertical-align: bottom; max-width: 0em;") #max-width: 5em;



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

#' Graphical user interface to analyse palettes
#'
#' Graphical user interface to analyse palettes. `c4a_table` shows a table that can be opened in the browser. `c4a_gui` is a graphical user interface (shiny app) around this table.
#'
#' See vignette how the properties are calculated. Parameters, such as threshold values which determined when palettes are classified as "colorblind-friendly", can be specified via \code{\link{c4a_options}}. Also the nameability score function (which is in development) can be specified there. See the examples of \code{\link{c4a_options}} for both use cases.
#'
#' @param type type of palette. Run \code{\link{c4a_types}} to see the implemented types and their description. For `c4a_gui` it only determines which type is shown initially.
#' @param n,m `n` is the number of displayed colors. For bivariate palettes `"biv"`, `n` and `m` are the number of columns and rows respectively. If omitted: for `"cat"` the full palette is displayed, for `"seq"`, `"div"` and `"cyc"`, 7, 9, and 9 colors respectively, and for `"bivs"`/`"bivc"`/`"bivd"`/`"bivg"` 4 columns and rows. For `c4a_gui` it only determines which number of colors initially.
#' @param continuous should the palettes as continuous instead of discrete. Only applicable for `"seq"`, `"div"`, and `"cyc"`.
#' @param filters filters to be applied. A character vector with a subset from:`"nmax"` (only palettes where `n = nmax`, which is only applicable for categorical palettes),  `"cbf"` (colorblind-friendly), `"fair"` (fairness),`"naming"` (nameability),  `"crW"` (sufficient contrast ratio with white), and `"crB"` (sufficient contrast ratio with black). By default an empty vector, so no filters are applied.
#' @param cvd.sim color vision deficiency simulation: one of `"none"`, `"deutan"`, `"protan"`, `"tritan"`
#' @param sort column name to sort the data. The available column names depend on the arguments `type` and `show.scores`. They are listed in the warning message. Use a `"-"` prefix to reverse the order.
#' @param text.format The format of the text of the colors. One of `"hex"`, `"RGB"` or `"HCL"`.
#' @param text.col The text color of the colors. By default `"same"`, which means that they are the same as the colors themselves (so invisible, but available for selection). `"auto"` means automatic: black for light colors and white for dark colors.
#' @param series Series of palettes to show. See \code{\link{c4a_series}} for options. By default, `"all"`, which means all series. For `c4a_gui` it only determines which series are shown initially.
#' @param range vector of two numbers that determine the range that is used for sequential and diverging palettes. Both numbers should be between 0 and 1. The first number determines where the palette begins, and the second number where it ends. For sequential palettes, 0 means the leftmost (normally lightest) color, and 1 the rightmost (often darkest) color. For diverging palettes, 0 means the middle color, and 1 both extremes. If only one number is provided, this number is interpreted as the endpoint (with 0 taken as the start). By default, it is set automatically, based on `n`.
#' @param colorsort Sort the colors (`"cat"` only). Options: `"orig"` (original order), `"Hx"` (hue, where x is a starting number from 0 to 360), `"C"` (chroma), `"L"` (luminance)
#' @param include.na should color for missing values be shown? `FALSE` by default
#' @param show.scores should scores of the quality indicators be printed? See details for a description of those indicators.
#' @param columns number of columns. By default equal to `n` or, if not specified, 12. Cannot be higher than the palette lengths.
#' @param verbose should messages and warnings be printed?
#' @import colorspace abind
#' @importFrom grDevices hcl dev.size contourLines
#' @importFrom stats lm predict dbeta
#' @importFrom utils tail head combn
#' @importFrom png readPNG
#' @importFrom stats rnorm runif
#' @importFrom graphics barplot lines par abline axis
#' @example ./examples/c4a_table.R
#' @seealso References of the palettes: \code{\link{cols4all-package}}.
#' @export
#' @return An HMTL table (`kableExtra` object)
#' @rdname c4a_gui
#' @name c4a_table
c4a_table = function(type = c("cat", "seq", "div", "cyc", "bivs", "bivc", "bivd", "bivg"), n = NULL, m = NULL, continuous = FALSE, filters = character(0), cvd.sim = c("none", "deutan", "protan", "tritan"), sort = "name", text.format = "hex", text.col = "same", series = "all", range = NA, colorsort = "orig", include.na = FALSE, show.scores = FALSE, columns = NA, verbose = TRUE) {
	cvd.sim = match.arg(cvd.sim)
	p = prep_table(type = type, n = n, m = m, continuous = continuous, filters = filters, sort = sort, series = series, range = range, colorsort = colorsort, show.scores = show.scores, columns = columns, verbose = verbose)
	plot_table(p = p, text.format = text.format, text.col = text.col, include.na = include.na, cvd.sim = cvd.sim, verbose = verbose)
}
