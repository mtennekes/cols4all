get_pal_n = function(n, m = n, name, type, series, palette, nmax, range = NA, n_too_large = "error",...) {
	n_orig = n
	if (n > nmax) {
		if (n_too_large == "error") return(NULL)
		n = nmax
	}
	index = attr(palette, "index")

	if (is.na(range[1])) range = c4a_default_range(n, type)

	x = if (type == "cat") {
		if (is.null(index)) {
			palette[1:n]
		} else {
			palette[index[[n]]]
		}
	} else if (type == "seq") {
		if (range[1] == 0 && range[2] == 1) {
			colorRampPalette(palette, space = "Lab")(n)
		} else {
			rangeIDs <- round(seq(range[1]*100, range[2]*100, length.out=n))+1
			colorRampPalette(palette, space = "Lab")(101)[rangeIDs]
		}
	} else if (type == "div") {
		if (range[1] == 0 && range[2] == 1) {
			colorRampPalette(palette, space = "Lab")(n)
		} else {
			breaks = seq(-10,10, length.out=n+1)
			rangeIDs <- map2divscaleID(breaks=breaks, range=range)
			colorRampPalette(palette, space = "Lab")(101)[rangeIDs]
		}
	} else if (type == "biv") {
		rangeIDsm <- round(seq(range[1]*100, range[2]*100, length.out=m))+1
		rangeIDsn <- round(seq(range[1]*100, range[2]*100, length.out=n))+1

		p2 = t(apply(palette, MARGIN = 1, FUN = function(x) {
			colorRampPalette(x, space = "Lab")(101)[rangeIDsn]
		}))
		t(apply(p2, MARGIN = 2, FUN = function(x) {
			colorRampPalette(x, space = "Lab")(101)[rangeIDsm]
		}))
	}


	# for cat only?
	if (type == "cat") {
		if (n_orig != n) {
			if (n_too_large == "repeat") {
				x = rep(x, length.out = n_orig)
			} else if (n_too_large == "interpolate") {
				x = colorRampPalette(x, space = "Lab")(n_orig)
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
