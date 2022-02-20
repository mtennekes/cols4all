attach_scores = function(z) {

	type = if (all(z$type == "cat")) "cat" else if (all(z$type == "seq")) "seq" else if (all(z$type == "div")) "div" else "mixed"

	k = nrow(z)

	s = get(".s", envir = .C4A_CACHE)


	s2 = s[match(z$name, dimnames(s)[[1]]), , , drop = FALSE]

	s3 = do.call(rbind, lapply(1:k, function(i) {
		# maximum n to take scores from (cat: dim max, seq/div, the scores for the largest palettes)
		mmax = if (type == "cat") dim(s2)[3] else min(z$n[i], which(is.na(s2[i, "rank", ][-1]))[1])
		m = min(z$n[i], mmax)
		s2[i,,m]
	}))
	z2 = cbind(z, as.data.frame(s3))

	# remove ranking if multiple types are included or multiple n
	#if (is.null(n) || (!all(z2$type == z2$type[1]))) z2$rank = NULL

	z2$cbfriendly = get_friendlyness(z2)
	a = analyse_hcl(z2$palette)
	z2 = cbind(z2, a)

	z2$highC = z2$Cmax >= 100

	if (type == "div") {
		z2$hueType = ifelse(z2$HwidthL >= 90 | z2$HwidthR >= 90, "RH",
					 ifelse(z2$HwidthL < 20 & z2$HwidthR < 20, "SH", "MH"))

		z2$HwidthLR = pmax(z2$HwidthL, z2$HwidthR)

		z2$rank[z2$cbfriendly] = z2$rank[z2$cbfriendly] - 1e9 - ((!z2$highC[z2$cbfriendly]) * 1e6) - ((z2$hueType[z2$cbfriendly] == "SH") * 1e3)

		z2$rank[!z2$cbfriendly] = z2$rank[!z2$cbfriendly] - ((!z2$highC[!z2$cbfriendly]) * 1e-3) - ((z2$hueType[!z2$cbfriendly] == "SH") * 1e-6)

	} else if (type == "seq") {
		z2$hueType = ifelse(z2$Hwidth < 15, "SH", ifelse(z2$Hwidth < 180, "MH", "RH"))

		z2$rank[z2$cbfriendly] = z2$rank[z2$cbfriendly] - 1e9 - ((!z2$highC[z2$cbfriendly]) * 1e6)

		z2$rank[!z2$cbfriendly] = z2$rank[!z2$cbfriendly] - ((!z2$highC[!z2$cbfriendly]) * 1e-3)

	} else if (type == "cat") {
		z2$harmonic = (z2$LCrange < 80)

		z2$rank[z2$cbfriendly] = z2$rank[z2$cbfriendly] - 1e9 + ((z2$LCrange[z2$cbfriendly]) * 1e6) + (z2$highC[z2$cbfriendly] * 1e3)

		z2$rank[!z2$cbfriendly] = z2$rank[!z2$cbfriendly] + ((z2$LCrange[!z2$cbfriendly]) * 1e-3) + (z2$highC[!z2$cbfriendly] * 1e-6)
	}
	z2$rank = floor(rank(z2$rank))
	z2
}


get_friendlyness = function(zn) {
	with(zn, {
		ifelse(type == "cat", min_dist >= 10,
		ifelse(type == "seq", min_step >= 5,
		ifelse(type == "div", inter_wing_dist >= 10 & inter_wing_hue_dist >=100 & min_step >= 5, FALSE)))
	})
}

get_pal_n = function(n, name, type, series, palette, nmax, contrast = NULL, ...) {
	if (n > nmax) return(NULL)
	#pal = palette[[1]]
	index = attr(palette, "index")


#	if (name == "polychrome") browser()

	if (is.null(index)) {
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
