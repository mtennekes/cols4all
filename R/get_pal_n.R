rampPal = function(palette, n, space = c("rgb", "Lab")) {
	space = match.arg(space)
	if (length(palette) == n) {
		attributes(palette) = NULL
		palette
	} else {
		colorRampPalette(palette, space = space, interpolate = "linear")(n)
	}
}

get_pal_n = function(n, m = NA, name, type, series, palette, nmin, nmax, ndef, range = NA, n_invalid = "error",...) {
	n_orig = n
	if (is.na(m)) m = n
	if (n > nmax || n < nmin) {
		if (n_invalid == "error") return(NULL)
		n = ndef
	}
	index = attr(palette, "index")
	range_matrix = attr(palette, "range_matrix")
	space = attr(palette, "space")

	x = if (type == "cat") {
		if (is.null(index)) {
			palette[1:n]
		} else {
			palette[index[[n]]]
		}
	} else if (type %in% c("seq", "div")) {
		if (is.na(range[1])) {
			if (!is.null(index)) {
				pal = palette[index[[min(n, length(index))]]]
				rng = c(0, 1)
			} else if (!is.null(range_matrix)) {
				pal = palette
				rng = range_matrix[min(n, nrow(range_matrix)), ]
			} else {
				pal = palette
				rng = c(0, 1)
			}
		} else {
			if (!is.null(index)) {
				pal = palette[index[[length(index)]]]
			} else {
				pal = palette
			}
			rng = range
		}

		if (type == "seq") {
			if (rng[1] == 0 && rng[2] == 1) {
				rampPal(pal, n, space = space)
			} else {
				rngIDs <- round(seq(rng[1]*100, rng[2]*100, length.out=n))+1
				rampPal(pal, 101, space = space)[rngIDs]
			}
		} else {
			if (rng[1] == 0 && rng[2] == 1) {
				rampPal(pal, n, space = space)
			} else {
				breaks = seq(-10,10, length.out=n+1)
				rngIDs <- map2divscaleID(breaks=breaks, range=rng)
				rampPal(pal, 101, space = space)[rngIDs]
			}
		}
	} else if (substr(type, 1, 3) == "biv") {
		if (is.na(range[1])) range = c(0, 1)
		if (all(dim(palette) == c(m, n)) && range[1] == 0 && range[2] == 1) {
			palette
		} else {
			rangeIDsm <- round(seq(range[1]*100, range[2]*100, length.out=m))+1
			rangeIDsn <- round(seq(range[1]*100, range[2]*100, length.out=n))+1

			p2 = t(apply(palette, MARGIN = 1, FUN = function(x) {
				rampPal(x, 101, space = space)[rangeIDsn]
			}))
			res = apply(p2, MARGIN = 2, FUN = function(x) {
				rampPal(x, 101, space = space)[rangeIDsm]
			})

			if (type == "bivs" && n == m) {
				if (aregreys(diag(palette))) {
					diag(res) = convert2grey(diag(res))
				}
			} else if (type == "bivd") {
				if (aregreys(palette[, (ncol(palette) + 1)/2])) {
					res[, (ncol(res)+1)/2] = convert2grey(res[, (ncol(res)+1)/2])
				}
			}
			res
		}
	}


	# for cat only?
	if (type == "cat") {
		if (n_orig != n) {
			if (n_invalid == "repeat") {
				x = rep(x, length.out = n_orig)
			} else if (n_invalid == "interpolate") {
				x = rampPal(x, n_orig)
			}
		}
	}
	x
}




map2divscaleID <- function(breaks, n=101, range=1) {
	nbrks <- length(breaks)

	if (length(range)==1) {
		range <- c(0, range)
	}
	crange <- range[2] - range[1]

	lw <- breaks[1]
	hg <- breaks[nbrks]

	# omit infinity values
	if (lw==-Inf) lw <- breaks[2]
	if (hg==Inf) hg <- breaks[nbrks-1]
	mx <- max(abs(c(lw, hg)))


	is.div <- any(breaks<0) && any(breaks>0)

	cat0 <- !any(breaks==0)

	h <- ((n-1)/2)+1

	if (is.div && !cat0) {
		npos <- sum(breaks>0)
		nneg <- sum(breaks<0)
		step <- round((h-1)*crange/((max(npos, nneg)-.5)*2))
	} else {
		npos <- sum(breaks>=0) - !is.div
		nneg <- sum(breaks<=0) - !is.div
		step <- 0
	}

	pid <- h + step
	nid <- h - step

	ids <- rep(h, nbrks-1)
	if (npos>0) ids[(nbrks-npos):(nbrks-1)] <- pid +
		seq((n-pid)/mx*hg*range[1], (n-pid)/mx*hg*range[2], length.out=npos)
	if (nneg>0) ids[1:nneg] <- seq(nid-((nid-1)/mx*-lw*range[2]), nid-((nid-1)/mx*-lw*range[1]),
								   length.out=nneg)
	if (is.div && cat0) ids[nneg] <- h
	round(ids)
}
