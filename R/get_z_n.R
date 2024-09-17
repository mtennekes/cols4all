get_z_n = function(z, n = NA, m = NA, filters = character(0), range = NA, colorsort = "orig") {
	# if (!is.null(n)) {

	if ("nmax" %in% filters) {
		n.only = TRUE
		filters = setdiff(filters, "nmax")
	} else {
		n.only = FALSE
	}

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
	z2$palette = do.call(mapply, c(list(FUN = get_pal_n, MoreArgs = list(n = n, m = m, range = range, colorsort = colorsort), SIMPLIFY = FALSE), as.list(z2)))
	z2$n = n
	z2$m = m


	if (length(filters)) {
		z3 = show_attach_scores(z2)

		sels = lapply(filters, function(f) {
			if (f == "cbf") {
				z3$cbfriendly >= 1
			} else if (f == "fair") {
				z3$fair == "H"
			} else if (f == "naming") {
				z3$nameable
			} else if (f == "crW") {
				z3$contrastWT
			} else if (f == "crB") {
				z3$contrastBK
			}
		})
		fsel = Reduce("&", sels)
		z2 = z2[fsel, ]
	 }

	z2
}
