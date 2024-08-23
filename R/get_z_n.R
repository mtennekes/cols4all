get_z_n = function(z, n = NA, m = NA, n.only = FALSE, range = NA) {
	# if (!is.null(n)) {
	if (is.na(n) && is.na(m)) {
		sel = TRUE
	} else if (is.na(m)) {
		if (n.only) {
			sel = n == z$nmax
		} else {
			sel = n <= z$nmax & n >= z$nmin
		}
	} else {
		if (n.only) {
			sel = n == z$nmax & m <= z$mmax & m >= z$mmin
		} else {
			sel = n <= z$nmax & n >= z$nmin & m <= z$mmax & m >= z$mmin
		}
	}


	z2 = z[sel, ]
	if (nrow(z2) == 0) return(NULL)
	z2$palette = do.call(mapply, c(list(FUN = get_pal_n, MoreArgs = list(n = n, m = m, range = range), SIMPLIFY = FALSE), as.list(z2)))
	z2$n = n
	z2$m = m

	# } else {
	# 	z2 = z
	# 	z2$n = z2$ndef
	# 	z2$m = z2$n
	# 	z2$palette = do.call(mapply, c(list(FUN = get_pal_n, MoreArgs = list(range = range), SIMPLIFY = FALSE), as.list(z2)))
	# }
	z2
}
