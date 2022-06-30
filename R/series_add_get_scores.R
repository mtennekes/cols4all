series_add_get_scores = function(z) {
	nmax = .C4A$nmax
	k = nrow(z)


	nmaxmax = max(nmax)

	a = array(as.integer(NA), dim = c(nrow(z), length(.C4A$sc) + length(.C4A$hcl), nmaxmax), dimnames = list(z$fullname, c(.C4A$sc, .C4A$hcl), NULL))


	types = unname(.C4A$types)


	for (tp in types) {
		check_fun = paste0("check_", tp, "_pal")
		for (n in 2:nmax[tp]) {
			m = if (tp == "bivs") n else if (tp %in% c("bivc", "bivd", "bivg")) 5 else 1

			zn = get_z_n(z[z$type == tp,], n =n, m = m)

			if (!is.null(zn)) {
				q = do.call(rbind, lapply(zn$palette, check_fun))
				if (!is.integer(q[1,1])) browser()
				r = 1L #-q#rank(-q, ties.method = "first")

#browser()
				mn = cbind(q,rank=r)

				qnames = colnames(mn)

				a[match(zn$fullname, dimnames(a)[[1]]), match(qnames, dimnames(a)[[2]]), n] = mn
			}
		}

	}

#browser()
#
# 	# categorical
# 	for (n in 2:nmax['cat']) {
# 		zn = get_z_n(z[z$type == "cat",], n =n, m = 1)
#
# 		if (!is.null(zn)) {
# 			q = do.call(rbind, lapply(zn$palette, check_cat_pal))
# 			r = -q#rank(-q, ties.method = "first")
#
# 			mn = cbind(q,r)
#
# 			a[match(zn$fullname, dimnames(a)[[1]]), c("min_dist", "rank"), n] = mn
# 		}
# 	}
#
# 	# sequential
# 	for (n in 2:nmax['seq']) {
# 		zn = get_z_n(z[z$type == "seq",], n =n, m = 1)
#
# 		if (!is.null(zn)) {
# 			q = do.call(rbind, lapply(zn$palette, check_seq_pal))
# 			qr = q[,1] - q[,2] / 1000 # order min_step, those with equal store to -max_step
#
# 			r = -qr#rank(-qr, ties.method = "first")
#
# 			mn = cbind(q,r)
#
# 			a[match(zn$fullname, dimnames(a)[[1]]), c("min_step", "max_step", "rank"), n] = mn
# 		}
# 	}
#
# 	# diverging
# 	for (n in 2:nmax['div']) {
# 		zn = get_z_n(z[z$type == "div",], n =n, m = 1)
#
# 		if (!is.null(zn)) {
#
# 			q = do.call(rbind, lapply(zn$palette, check_div_pal))
# 			qr = pmin(q[,1], q[,2] * 2)
#
# 			r = -qr#rank(-qr, ties.method = "first")
#
# 			mn = cbind(q,r)
#
# 			a[match(zn$fullname, dimnames(a)[[1]]), c("inter_wing_dist", "min_step", "rank"), n] = mn
# 		}
# 	}
#
# 	# bivariate seq-seq
# 	for (n in 2:nmax['bivs']) {
# 		zn = get_z_n(z[z$type == "bivs",], n = n, m = n)
#
# 		if (!is.null(zn)) {
#
# 			q = do.call(rbind, lapply(zn$palette, check_bivs_pal))
# 			qr = pmin(q[,1], q[,2] * 2)
#
# 			r = -qr#rank(-qr, ties.method = "first")
#
# 			mn = cbind(q,r)
#
# 			a[match(zn$fullname, dimnames(a)[[1]]), c("inter_wing_dist", "min_step", "rank"), n] = mn
# 		}
# 	}
#
# 	# bivariate cat-seq
# 	for (n in 2:nmax['bivc']) {
# 		zn = get_z_n(z[z$type == "bivc",], n = n, m = 5)
#
# 		if (!is.null(zn)) {
# 			q = do.call(rbind, lapply(zn$palette, check_bivc_pal))
# 			r = -q#rank(-q, ties.method = "first")
#
# 			mn = cbind(q,r)
#
# 			a[match(zn$fullname, dimnames(a)[[1]]), c("min_dist", "rank"), n] = mn
# 		}
# 	}
#
# 	# bivariate div-seq
# 	for (n in seq(3, nmax['bivd'], by = 2)) {
# 		zn = get_z_n(z[z$type == "bivd",], n = n, m = 5)
#
# 		if (!is.null(zn)) {
#
# 			q = do.call(rbind, lapply(zn$palette, check_bivd_pal))
# 			qr = pmin(q[,1], q[,2] * 2)
#
# 			r = -qr#rank(-qr, ties.method = "first")
#
# 			mn = cbind(q,r)
#
# 			a[match(zn$fullname, dimnames(a)[[1]]), c("inter_wing_dist", "min_step", "rank"), n] = mn
# 		}
# 	}
#
#
# 	# bivariate unc-seq
# 	for (n in 2:nmax['bivg']) {
# 		zn = get_z_n(z[z$type == "bivg",], n = n, m = 5)
#
# 		if (!is.null(zn)) {
#
# 			q = do.call(rbind, lapply(zn$palette, check_bivg_pal))
# 			qr = pmin(q[,1], q[,2] * 2)
#
# 			r = -qr#rank(-qr, ties.method = "first")
#
# 			mn = cbind(q,r)
#
# 			a[match(zn$fullname, dimnames(a)[[1]]), c("inter_wing_dist", "min_step", "rank"), n] = mn
# 		}
# 	}

	a
}
