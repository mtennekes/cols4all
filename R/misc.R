#' Select categorical palettes with specific length
#'
#' @param x named list of palettes. Each list item is either a color vector or a list of color vectors for specific lengths (names should correspond to those lengths.
#' @param n desired number colors. When not specific, the maximum is taken.
sel_cat = function(x, n = NULL) {
	y = if (is.null(n)) {
		lapply(x, function(xi) {
			if (is.list(xi)) unname(xi[[length(xi)]]) else xi
		})
	} else {
		y = lapply(x, function(xi) {
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
		yl = sapply(y, length)
		sel = (yl >= n)
		lapply(y[sel], function(pal) pal[1:n])
	}
	attr(y, "sel") = sel
	y
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

