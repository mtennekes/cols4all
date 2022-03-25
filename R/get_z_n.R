get_z_n = function(z, n, m = NA, range = c(0, 1)) {
	if (!is.null(n)) {
		z2 = z[n <= z$nmax & n >= z$nmin, ]
		if (nrow(z2) == 0) return(NULL)
		z2$palette = do.call(mapply, c(list(FUN = get_pal_n, MoreArgs = list(n = n, m = m, range = range), SIMPLIFY = FALSE), as.list(z2)))
		z2$n = n
	} else {
		z2 = z
		z2$n = z2$ndef
		z2$m = z2$n
		z2$palette = do.call(mapply, c(list(FUN = get_pal_n, MoreArgs = list(range = range), SIMPLIFY = FALSE), as.list(z2)))
	}
	z2
}
