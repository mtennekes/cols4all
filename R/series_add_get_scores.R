series_add_get_scores = function(z) {
	nmax = .C4A$nmax
	nmin = .C4A$nmin
	mdef = .C4A$mdef
	k = nrow(z)


	nmaxmax = max(nmax)

	a = array(as.integer(NA), dim = c(nrow(z), length(.C4A$sc) + length(.C4A$hcl) + length(.C4A$rgb), nmaxmax), dimnames = list(z$fullname, c(.C4A$sc, .C4A$hcl, .C4A$rgb), NULL))


	types = unname(.C4A$types)


	for (tp in types) {
		check_fun = paste0("check_", tp, "_pal")
		for (n in nmin[[tp]]:nmax[tp]) {
			m = if (tp == "bivs") n else if (tp %in% c("bivc", "bivd", "bivg")) mdef[[tp]] else 1

			zn = get_z_n(z[z$type == tp,], n =n, m = m)

			if (!is.null(zn)) {
				q = do.call(rbind, lapply(zn$palette, check_fun))
				#if (!is.integer(q[1,1])) browser()
				r = 1L #-q#rank(-q, ties.method = "first")

				mn = q

				qnames = colnames(mn)

				a[match(zn$fullname, dimnames(a)[[1]]), match(qnames, dimnames(a)[[2]]), n] = mn
			}
		}

	}

	a
}
