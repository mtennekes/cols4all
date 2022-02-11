#' Select categorical palettes with specific length
#'
#' @param x named list of palettes. Each list item is either a color vector or a list of color vectors for specific lengths (names should correspond to those lengths.
#' @param n desired number colors. When not specific, the maximum is taken.
get_z_n = function(x, n = NULL) {
	x2 = lapply(x, function(xi) {
		index = attr(xi, "index")
		if (!is.null(index)) {
			if (is.null(n)) n = as.character(names(index)[length(index)])
			ind = which(names(index) == as.character(n))[1]
			if (length(ind)) {
				unname(xi[index[[as.character(n)]]])
			} else {
				xi
			}
		} else {
			xi
		}
	})
	l = sapply(x2, length)

	if (is.null(n)) {
		sel = NULL
		y = x2
	} else {
		sel = (l >= n)
		y = lapply(x2[sel], function(pal) pal[1:n])
	}

	attr(y, "sel") = sel
	y
}

get_scores = function(z, nmax, type = "cat") {
	q = switch(type, cat = "min_dist", seq = "min_step", cyc = "min_step", div = c("inter_wing_dist", "min_step"))
	p = length(z)

	qfun = paste0("check_", type, "_pal")

	m = array(NA, dim = c(p, nmax, length(q) + 1), dimnames = list(names(z), 1:nmax, c(q, "rank")))
	for (n in 2:nmax) {
		zn = get_z_n(z, n =n)
		sel = attr(zn, "sel")
		s = do.call(rbind, lapply(zn, qfun))
		m[sel, n, 1:length(q)] = s

		r = rank(-s, ties.method = "first")
		m[sel, n, length(q) + 1] = r
	}
	m
}

sel_scores = function(x, n = NULL) {
	l = if (is.null(n)) {
		lapply(x, function(xi) {
			if (is.list(xi)) unname(xi[[length(xi)]]) else xi
		})
	} else {
		lapply(x, function(xi) {
			if (is.list(xi)) {
				nc = names(xi)

				ind = which(nc == as.character(n))[1]
				if (length(ind)) {
					unname(xi[[as.character(n)]])
				} else {
					xi[[length(xi)]]
				}
			} else {
				xi
			}
		})
	}
	cnames = names(l[[1]])
	m = do.call(rbind, l)
	colnames(m) = cnames
	as.data.frame(m)
}

rank_cat = function(m) {
	m$order = order(m$min_dist, decreasing = TRUE)
	m
}

dist_to_col = function(pal, col) {
	colorblindcheck::palette_dist(c(col, pal))[1,-1]
}

min_dist_cvd = function(pal) {
	min(sapply(c("deu", "pro", "tri"), function(cvd, x) {
		min(colorblindcheck::palette_dist(x = x, cvd = cvd), na.rm = TRUE)
	}, x = pal))
}

remove_black_white = function(pal, th = 5) {
	blcks = dist_to_col(pal, "#000000") <= th
	almost_blcks = dist_to_col(pal, "#0D0D0D") <= th # 1/20 grey

	whts = dist_to_col(pal, "#FFFFFF") <= th
	almost_whts = dist_to_col(pal, "#F2F2F2") <= th # 19/20 grey
	pal[!blcks & !whts & !almost_blcks & !almost_blcks]
}

