#' Show cols4all palettes
#'
#' Show cols4all palettes
#'
#' @param x named list of color palettes
#' @param n number of colors
#' @param columns number of columns in case `n` is not specified. Otherwise `columns` is set to `n`
#' @param cvd.sim color vision deficiency simulation: one of `"none"`, `"deutan"`, `"protan"`, `"tritan"`
#' @param order.by.score order the palettes by score (`TRUE`, default) or by name?
#' @param text.col The text color of the colors. By default `"same"`, which means that they are the same as the colors themselves (so invisible, but available for selection).
#' @import kableExtra
#' @import colorspace
c4a_show = function(n = NULL, type = c("cat", "seq", "div", "biv"), columns = 12, cvd.sim = c("none", "deutan", "protan", "tritan"), order.by.score = TRUE, text.col = "same") {

	devel = (!is.null(n)) # scores are shown


	z = z_cat
	s = s_cat

	cvd.sim = match.arg(cvd.sim)
	if (!is.null(n)) columns = n

	zn = get_z_n(z, n = n)

	if (devel) {
		if (n == 1L) order.by.score = FALSE
		sel = attr(zn, "sel")
		sn = s[sel, n, ]
		if (length(zn) > 1) {
			if (order.by.score) {
				o = order(sn[, ncol(sn)])
			} else {
				o = names(zn)
			}
			so = sn[o,]
			zn = zn[o]
		} else {
			so = matrix(sn, nrow = 1, dimnames = list(names(zn), names(sn)))
		}
		so = so[,1:(ncol(so)-1), drop = FALSE]
	} else {
		zn = zn[order(names(zn))]
	}

	d = data.frame(name = names(zn), nlines = sapply(zn, function(xi) (((length(xi)-1) %/% columns) + 1)))
	#if (devel) d = cbind(d, so)
	# add dummy lines


	e = data.frame(id = unlist(mapply(function(n, i) {
		c(i * 1000 + 1:n)
	}, d$nlines, 1:nrow(d), SIMPLIFY = FALSE))
	)
	e$did = floor(e$id/1000)
	e$ind = e$id - e$did * 1000
	e$indx = sapply(1:nrow(d), function(i) which.max(e$ind[e$did==i]))[e$did]
	e$name = ""
	e$name[e$did>0] = d$name[match(e$did[e$did>0], 1:nrow(d))]
	e$label = ""
	e$label[e$ind==1] = d$name[match(e$did[e$ind==1], 1:nrow(d))]

	e$row_h = ifelse(e$ind==e$indx, 25, 12)

	if (devel) {
		e = cbind(e, so[match(e$label, rownames(so)),,drop=FALSE])
		rownames(so) = NULL
		snames = colnames(so)
	} else {
		snames = NULL
	}


	tot = max(c(sapply(zn, length), columns))
	if (columns < tot) tot = (((tot-1) %/% columns) + 1) * columns


	x = lapply(zn, function(zni) {
		if (length(zni) < tot) {
			c(zni, rep("#FFFFFF", tot - length(zni)))
		} else {
			zni
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
		cols_cvd = sim(cols)
		textcol = if (text.col == "same") cols_cvd else text.col
		e2[[as.character(i)]] = kableExtra::cell_spec(cols, color = textcol, background = cols_cvd, monospace = TRUE, align = "c", extra_css = "border-radius: 0px;")
	}


	rownames(e2) = NULL

	if (devel) {
		e2$score = ifelse(e2$min_dist > 8, "&#9786;", "")
		e2cols = c("label", snames, "score", as.character(1:columns))
		e2nms = c("", snames, "", as.character(1:columns))
	} else {
		e2cols = c("label", as.character(1:columns))
		e2nms = c("", as.character(1:columns))
	}

	k = kableExtra::kbl(e2[, e2cols], col.names = e2nms, escape = F)
	k = kableExtra::row_spec(k, 0, align = "c")
	k = kableExtra::column_spec(k, 1, extra_css = "padding-left: 20px;padding-right: 10px;min-width: 120px")

	if (devel) {
		for (i in 1:length(snames)) {
			k = kableExtra::column_spec(k, i+1, extra_css = "padding-right: 20px;")
		}
		k = kableExtra::column_spec(k, length(snames)+2, extra_css = "padding-right: 10px; font-size: 200%")
	}


	kc = k[1]

	kl = strsplit(kc, "\n")[[1]]
	trIDs = which(kl == "  <tr>")[-1]
	rws1 = trIDs[e2$ind==1] # first line per palette
	rws2 = trIDs[e2$ind!=1] # other lines
	#browser()
	kl[rws1] = paste0("  <tr style=\"height: ", e2$row_h[e2$ind==1], "px;vertical-align:center;\">")
	kl[rws2] = paste0("  <tr style=\"height: ", e2$row_h[e2$ind!=1], "px;vertical-align:top;\">")
	#kl[rws[-1]] = paste0("  <tr style=\"vertical-align:top;\">")

	css = readLines("css/table.css")

	k[1] = paste(c(css,kl), collapse="\n")
	k

}



