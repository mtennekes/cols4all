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
c4a_show = function(n = NULL, type = c("cat", "seq", "div", "biv"), columns = NA, cvd.sim = c("none", "deutan", "protan", "tritan"), order.by.score = TRUE, text.col = "same") {

	type = match.arg(type)

	devel = (!is.null(n)) # scores are shown


	zt = z[z$type == type, ]
	#s = s_cat

	cvd.sim = match.arg(cvd.sim)

	if (is.na(columns)) columns = if (!is.null(n)) n else 12

	zn = get_z_n(zt, n = n)

	columns = min(columns, max(zn$ncolors))

	k = nrow(zn)

	if (devel) {
		rn = paste0("rank", n)
		if (type == "cat") {
			qn = paste0(c("min_dist", "friendly"), n)
			ql = c("Minimum distance", "Colorblind-friendly")
		} else if (type == "seq") {
			qn = paste0(c("min_step", "max_step", "friendly"), n)
			ql = c("Minimum step", "Maximum step", "Colorblind-friendly")
		} else if (type == "div") {
			qn = paste0(c("inter_wing_dist", "min_step", "friendly"), n)
			ql = c("Inter-wing-dist", "Minimum step", "Colorblind-friendly")
		}

		if (n == 1L) order.by.score = FALSE
		if (nrow(zn) > 1) {
			if (order.by.score) {
				o = order(zn[[rn]])
			} else {
				o = order(zn$name)
			}
			zn = zn[o, ]
		}
	} else {
		zn = zn[order(zn$name), ]
	}

	zn$nlines = ((zn$ncolors-1) %/% columns) + 1

	e = data.frame(id = unlist(mapply(function(n, i) {
		c(i * 1000 + 1:n)
	}, zn$nlines, 1L:k, SIMPLIFY = FALSE))
	)
	e$did = floor(e$id/1000)
	e$ind = e$id - e$did * 1000
	e$indx = sapply(1L:k, function(i) which.max(e$ind[e$did==i]))[e$did]
	e$name = ""
	e$name[e$did>0] = zn$name[match(e$did[e$did>0], 1L:k)]
	e$label = ""
	e$label[e$ind==1] = zn$name[match(e$did[e$ind==1], 1L:k)]

	e$row_h = ifelse(e$ind==e$indx, 25, 12)

	if (devel) {
		e = cbind(e, zn[match(e$label, zn$name),qn,drop=FALSE])
		colnames(e)[match(qn, colnames(e))] = ql
	}

	tot = max(c(zn$ncolors, columns))
	if (columns < tot) tot = (((tot-1) %/% columns) + 1) * columns


	x = lapply(zn$palette, function(p) {
		if (length(p) < tot) {
			c(p, rep("", tot - length(p)))
		} else {
			p
		}
	})

	m = unname(do.call(rbind, x))
	ml = ceiling(tot / columns)

	sid = split(1:tot, f = rep(1:ml, each = columns, length.out = tot))
	me = do.call(rbind, lapply(1:nrow(e), function(i) {
		m[e$did[i], sid[[e$ind[i]]]]
	}))

	colnames(me) = 1:ncol(me)
	e2 = cbind(e, me)

	sim = switch(cvd.sim,
				 none = function(x) x,
				 deutan = colorspace::deutan,
				 protan = colorspace::protan,
				 tritan = colorspace::tritan)

	for (i in 1:columns) {
		#e[[paste0("x", i)]] = ""
		cols = e2[[as.character(i)]]
		cols_cvd = cols
		cols_cvd[cols_cvd != ""] = sim(cols[cols_cvd != ""])
		textcol = if (text.col == "same") cols_cvd else text.col
		e2[[as.character(i)]] = kableExtra::cell_spec(cols, color = textcol, background = cols_cvd, monospace = TRUE, align = "c", extra_css = "border-radius: 0px;")
	}


	rownames(e2) = NULL

	if (devel) {
		e2[[ql[length(ql)]]] = ifelse(!is.na(e2[[ql[length(ql)]]]) & e2[[ql[length(ql)]]] == 1L, "&#9786;", "")


		for (i in 1:(length(ql)-1)) {
			e2[[ql[i]]] = as.character(e2[[ql[i]]])
			e2[[ql[i]]][is.na(e2[[ql[i]]])] = ""
		}

		e2cols = c("label", ql, as.character(1:columns))
		e2nms = c("", ql, as.character(1:columns))
	} else {
		e2cols = c("label", as.character(1:columns))
		e2nms = c("", as.character(1:columns))
	}

	k = kableExtra::kbl(e2[, e2cols], col.names = e2nms, escape = F)
#return(k)

	# for (i in (1:columns)+(length(ql)+1)) {
	# 	k = kableExtra::column_spec(k, i, extra_css = 'width: 5em; overflow: hidden; background-color: #000000"')
	# }
	k = kableExtra::column_spec(k, 1, extra_css = "padding-left: 10px;padding-right: 10px;min-width: 120px")

	k = kableExtra::column_spec(k, 1, extra_css = "padding-left: 10px;padding-right: 10px;min-width: 120px")
	k = kableExtra::row_spec(k, 0, align = "c", extra_css = "max-width: 5em; vertical-align: bottom")

	if (devel) {
		for (i in 1:length(ql)) {
			k = kableExtra::column_spec(k, i+1, extra_css = "padding-right: 20px; max-width: 10em;")
		}
		k = kableExtra::column_spec(k, length(ql)+1, extra_css = "font-size: 250%; line-height: 40%; vertical-align: center; text-align: center", width = "3em" )
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



