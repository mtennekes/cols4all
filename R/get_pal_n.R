get_pal_n = function(n, name, type, series, palette, nmax, contrast = NULL, n_too_large = "error",...) {
	n_orig = n
	if (n > nmax) {
		if (n_too_large == "error") return(NULL)
		n = nmax
	}
	index = attr(palette, "index")

	x = if (is.null(index)) {
		if (type == "cat") {
			palette[1:n]
		} else {
			if (is.null(contrast)) {
				fun = paste0("default_contrast_", type)
				contrast = do.call(fun, list(k = n))
			}

			if (contrast[1] == 0 && contrast[2] == 1) {
				colorRampPalette(palette, space = "Lab")(n)
			} else {
				if (type == "seq") {
					contrastIDs <- round(seq(contrast[1]*100, contrast[2]*100, length.out=n))+1
				} else if (type == "div") {
					contrastIDs <- map2divscaleID(breaks=seq(-10,10, length.out=n+1), contrast=contrast)
				}
				colorRampPalette(palette, space = "Lab")(101)[contrastIDs]
			}
		}
	} else {
		palette[index[[n]]]
	}
	if (n_orig != n) {
		if (n_too_large == "repeat") {
			x = rep(x, length.out = n_orig)
		} else if (n_too_large == "interpolate") {
			x = colorRampPalette(x, space = "Lab")(n_orig)
		}
	}
	x
}


default_contrast_seq <- function(k) {
	c1 <- max((9-k) * (.15/6), 0)
	c2 <- min(.7 + (k-3) * (.3/6), 1)
	c(c1,c2)
}

default_contrast_div <- function(k) {
	c(0, min(.6 + (k-3) * (.4/8), 1))
}

map2divscaleID <- function(breaks, n=101, contrast=1) {
	nbrks <- length(breaks)

	if (length(contrast)==1) {
		contrast <- c(0, contrast)
	}
	crange <- contrast[2] - contrast[1]

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
		seq((n-pid)/mx*hg*contrast[1], (n-pid)/mx*hg*contrast[2], length.out=npos)
	if (nneg>0) ids[1:nneg] <- seq(nid-((nid-1)/mx*-lw*contrast[2]), nid-((nid-1)/mx*-lw*contrast[1]),
								   length.out=nneg)
	if (is.div && cat0) ids[nneg] <- h
	round(ids)
}
