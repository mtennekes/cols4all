#' Select categorical palettes with specific length
#'
#' @param x named list of palettes. Each list item is either a color vector or a list of color vectors for specific lengths (names should correspond to those lengths.
#' @param n desired number colors. When not specific, the maximum is taken.
get_z_n = function(n = NULL, z = .z, s = .s, contrast) {
	if (!is.null(n)) {
		z2 = z[n <= z$nmax, ]
		z2$palette = do.call(mapply, c(list(FUN = get_pal_n, MoreArgs = list(n = n, contrast = contrast), SIMPLIFY = FALSE), as.list(z2)))
		z2$n = n
	} else {
		z2 = z
		z2$n = ifelse(is.infinite(z2$nmax), 11, z2$nmax)
		z2$palette = do.call(mapply, c(list(FUN = get_pal_n, MoreArgs = list(contrast = contrast), SIMPLIFY = FALSE), as.list(z2)))
	}

	type = if (all(z2$type == "cat")) "cat" else if (all(z2$type == "seq")) "seq" else if (all(z2$type == "div")) "div" else "mixed"

	k = nrow(z2)

	s2 = s[match(z2$name, dimnames(s)[[1]]), , , drop = FALSE]

	s3 = do.call(rbind, lapply(1:k, function(i) {
		s2[i, , min(z2$n[i], dim(s2)[3])]
	}))
	z3 = cbind(z2, as.data.frame(s3))

	# remove ranking if multiple types are included or multiple n
	#if (is.null(n) || (!all(z3$type == z3$type[1]))) z3$rank = NULL

	z3$cbfriendly = get_friendlyness(z3)
	a = analyse_hcl(z3$palette)
	z3 = cbind(z3, a)

	z3$highC = z3$Cmax >= 100

	if (type == "div") {
		z3$hueType = ifelse(z3$HwidthL >= 90 | z3$HwidthR >= 90, "RH",
					 ifelse(z3$HwidthL < 20 & z3$HwidthR < 20, "SH", "MH"))

		z3$rank[z3$cbfriendly] = z3$rank[z3$cbfriendly] - 1e9 - ((!z3$highC[z3$cbfriendly]) * 1e6) - ((z3$hueType[z3$cbfriendly] == "SH") * 1e3)

		z3$rank[!z3$cbfriendly] = z3$rank[!z3$cbfriendly] - ((!z3$highC[!z3$cbfriendly]) * 1e-3) - ((z3$hueType[!z3$cbfriendly] == "SH") * 1e-6)

	} else if (type == "seq") {
		z3$hueType = ifelse(z3$Hwidth < 15, "SH", ifelse(z3$Hwidth < 180, "MH", "RH"))

		z3$rank[z3$cbfriendly] = z3$rank[z3$cbfriendly] - 1e9 - ((!z3$highC[z3$cbfriendly]) * 1e6)

		z3$rank[!z3$cbfriendly] = z3$rank[!z3$cbfriendly] - ((!z3$highC[!z3$cbfriendly]) * 1e-3)

	} else if (type == "cat") {
		z3$harmonic = (z3$LCrange < 80)

		z3$rank[z3$cbfriendly] = z3$rank[z3$cbfriendly] - 1e9 - ((!z3$highC[z3$cbfriendly]) * 1e6) - (z3$harmonic[z3$cbfriendly] * 1e3)

		z3$rank[!z3$cbfriendly] = z3$rank[!z3$cbfriendly] - ((!z3$highC[!z3$cbfriendly]) * 1e-3) - (z3$harmonic[!z3$cbfriendly] * 1e-6)
	}
	z3$rank = rank(z3$rank)
	z3
}


get_friendlyness = function(zn) {
	with(zn, {
		ifelse(type == "cat", min_dist >= 8,
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
