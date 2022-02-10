#' Show cols4all palettes
#'
#' Show cols4all palettes
#'
#' @param x named list of color palettes
#' @param n number of colors
#' @param columns number of columns in case `n` is not specified. Otherwise `columns` is set to `n`
#' @param cvd.sim color vision deficiency simulation: one of `"none"`, `"deutan"`, `"protan"`, `"tritan"`
#' @import kableExtra
#' @import colorspace
c4a_show = function(n = NULL, type = c("cat", "seq", "div", "biv"), columns = 12, cvd.sim = c("none", "deutan", "protan", "tritan"), devel = TRUE) {

	x = z_cat
	s = z_cat_score

	cvd.sim = match.arg(cvd.sim)
	if (!is.null(n)) columns = n

	x = sel_cat(x, n)
	sel = attr(x, "sel")

	s = rank_cat(sel_scores(s, n)[sel,, drop=FALSE])
	x = x[s$order]
	s = s[s$order,]
	s$order = NULL

	d = data.frame(name = names(x), nlines = sapply(x, function(xi) (((length(xi)-1) %/% columns) + 1)))
	d = cbind(d, s)
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
		e = cbind(e, s[match(e$label, rownames(s)),,drop=FALSE])
		snames = colnames(s)
	} else {
		snames = NULL
	}


	tot = max(c(sapply(x, length), columns))
	if (columns < tot) tot = (((tot-1) %/% columns) + 1) * columns


	x2 = lapply(x, function(xi) {
		if (length(xi) < tot) {
			c(xi, rep("#FFFFFF", tot - length(xi)))
		} else {
			xi
		}
	})

	m = unname(do.call(rbind, x2))
	ml = ceiling(tot / columns)

	sid = split(1:tot, f = rep(1:ml, each = columns, length.out = tot))

	me = do.call(rbind, lapply(1:nrow(e), function(i) {
		m[e$did[i], sid[[e$ind[i]]]]
	}))

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
		e2[[as.character(i)]] = kableExtra::cell_spec(cols, color = cols_cvd, background = cols_cvd, monospace = TRUE, align = "c", extra_css = "border-radius: 0px;")
	}



	k = kableExtra::kbl(e2[, c("label", as.character(1:columns), snames)], col.names = c("", as.character(1:columns), snames), escape = F)
	k = kableExtra::row_spec(k, 0, align = "c")
	k = kableExtra::column_spec(k, 1, extra_css = "padding-left: 20px;padding-right: 10px")

	kc = k[1]

	kl = strsplit(kc, "\n")[[1]]
	rws = which(kl == "  <tr>")

	kl[rws[-1]] = paste0("  <tr style=\"height:", e2$row_h, "px;vertical-align:top;\">")

	css = readLines("css/table.css")

	k[1] = paste(c(css,kl), collapse="\n")
	k

}



