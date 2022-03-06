get_z_n = function(z, n, range = c(0, 1)) {
	if (!is.null(n)) {
		z2 = z[n <= z$nmax, ]
		if (nrow(z2) == 0) return(NULL)
		z2$palette = do.call(mapply, c(list(FUN = get_pal_n, MoreArgs = list(n = n, range = range), SIMPLIFY = FALSE), as.list(z2)))
		z2$n = n
	} else {
		z2 = z
		z2$n = ifelse(is.infinite(z2$nmax), 9, z2$nmax)
		z2$palette = do.call(mapply, c(list(FUN = get_pal_n, MoreArgs = list(range = range), SIMPLIFY = FALSE), as.list(z2)))
	}
	z2
}
