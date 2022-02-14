#' Select categorical palettes with specific length
#'
#' @param x named list of palettes. Each list item is either a color vector or a list of color vectors for specific lengths (names should correspond to those lengths.
#' @param n desired number colors. When not specific, the maximum is taken.
get_z_n = function(n = NULL, z = .z, s = .s) {
	if (!is.null(n)) {
		z2 = z[n <= z$nmax, ]
		z2$palette = do.call(mapply, c(list(FUN = get_pal_n, MoreArgs = list(n = n), SIMPLIFY = FALSE), as.list(z2)))
		z2$n = n
	} else {
		z2 = z
		z2$n = ifelse(is.infinite(z2$nmax), 9, z2$nmax)
		z2$palette = do.call(mapply, c(list(FUN = get_pal_n, SIMPLIFY = FALSE), as.list(z2)))
	}


	k = nrow(z2)

	s2 = s[match(z2$name, dimnames(s)[[1]]), , , drop = FALSE]

	s3 = do.call(rbind, lapply(1:k, function(i) {
		s2[i, , z2$n[i]]
	}))
	z3 = cbind(z2, as.data.frame(s3))

	# remove ranking if multiple types are included or multiple n
	if (is.null(n) || (!all(z3$type == z3$type[1]))) z3$rank = NULL

	z3$cbfriendly = get_friendlyness(z3)
	a = analyse_hcl(z3$palette)
	z3 = cbind(z3, a)

	z3$highC = z3$Crel >= 95

	z3$hueType = ifelse(z3$Hwidth < 15, "SH", ifelse(z3$Hwidth < 180, "MH", "RH"))

	z3
}


get_friendlyness = function(zn) {
	with(zn, {
		ifelse(type == "cat", min_dist >= 8,
		ifelse(type == "seq", min_step >= 5,
		ifelse(type == "div", inter_wing_dist >= 10 & min_step >= 5, FALSE)))
	})
}

get_pal_n = function(n, name, type, series, palette, nmax, ...) {
	if (n > nmax) return(NULL)
	#pal = palette[[1]]
	index = attr(palette, "index")


#	if (name == "polychrome") browser()

	if (is.null(index)) {
		if (type == "cat") palette[1:n] else colorRampPalette(palette)(n)
	} else {
		palette[index[[n]]]
	}
}
