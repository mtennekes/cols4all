#' Select categorical palettes with specific length
#'
#' @param x named list of palettes. Each list item is either a color vector or a list of color vectors for specific lengths (names should correspond to those lengths.
#' @param n desired number colors. When not specific, the maximum is taken.
get_z_n = function(z, n = NULL) {

	if (any(z$type == "cat")) {
		zcat = if (is.null(n)) z[z$type == "cat", ] else z[z$type == "cat" & z$nmax >= n, ]
		zcat$ncolors = if (is.null(n)) zcat$nmax else rep(n, nrow(zcat))

		zcat$palette = mapply(function(p, k) {
			index = attr(p, "index")
			if (is.null(index)) {
				ind = 1L:k
			} else{
				ind = unname(index[[as.character(k)]])
			}
			p[ind]
		}, zcat$palette, zcat$ncolors, SIMPLIFY = FALSE)
	} else {
		zcat = NULL
	}
	if (any(z$type %in% c("seq", "div"))) {
		zsd = z[z$type %in% c("seq", "div"), ]
		zsd$ncolors = if (is.null(n)) 9 else rep(n, nrow(zsd))

		zsd$palette = mapply(function(p, k) {
			if (k != length(p)) p = colorRampPalette(p)(k)
			p
		}, zsd$palette, zsd$ncolors, SIMPLIFY = FALSE)
	} else {
		zsd = NULL
	}
	rbind(zcat, zsd)
}

attach_scores = function(z, nmax.cat = 36, nmax.oth = 9) {



	q = c(paste0("min_dist", 2:nmax.cat),
		  paste0("min_step", 2:nmax.oth),
		  paste0("max_step", 2:nmax.oth),
		  paste0("inter_wing_dist", 2:nmax.oth),
		  paste0("rank", 2:max(nmax.cat, nmax.oth)),
		  paste0("friendly", 2:max(nmax.cat, nmax.oth)))

	m = matrix(as.integer(NA), nrow=nrow(z), ncol = length(q), dimnames = list(NULL, q))

	# categorical
	for (n in 2:nmax.cat) {
		zn = get_z_n(z[z$type == "cat",], n =n)
		s = do.call(rbind, lapply(zn$palette, check_cat_pal))
		r = rank(-s, ties.method = "first")
		f = as.integer(s > 7)

		mn = cbind(s,r,f)

		m[match(zn$name, z$name), paste0(c("min_dist", "rank", "friendly"), n)] = mn
	}

	# sequential
	for (n in 2:nmax.oth) {
		zn = get_z_n(z[z$type == "seq",], n =n)
		s = do.call(rbind, lapply(zn$palette, check_seq_pal))
		sr = s[,1] - s[,2] / 1000 # order min_step, those with equal store to -max_step

		r = rank(-sr, ties.method = "first")
		f = as.integer(s[,1] > 7)

		mn = cbind(s,r,f)

		m[match(zn$name, z$name), paste0(c("min_step", "max_step", "rank", "friendly"), n)] = mn
	}

	# diverging
	for (n in 2:nmax.oth) {
		zn = get_z_n(z[z$type == "div",], n =n)
		s = do.call(rbind, lapply(zn$palette, check_div_pal))
		sr = pmin(s[,1], s[,2] * 2)

		r = rank(-sr, ties.method = "first")
		f = as.integer(s[,1] > 10 & s[,2] > 5)

		mn = cbind(s,r,f)

		m[match(zn$name, z$name), paste0(c("inter_wing_dist", "min_step", "rank", "friendly"), n)] = mn
	}

	cbind(z, m)
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

is_light <- function(col) {
	colrgb <- col2rgb(col)
	apply(colrgb * c(.299, .587, .114), MARGIN=2, sum) >= 128
}
