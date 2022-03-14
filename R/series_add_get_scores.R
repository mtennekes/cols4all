series_add_get_scores = function(z) {
	nmax = .C4A$nmax
	k = nrow(z)


	nmaxmax = max(nmax)

	a = array(as.integer(NA), dim = c(nrow(z), length(.C4A$sc), nmaxmax), dimnames = list(z$fullname, .C4A$sc, NULL))

	# s = list(min_dist = as.list(rep(as.integer(NA), nmax)),
	# 		 min_step = as.list(rep(as.integer(NA), nmax)),
	# 		 max_step = as.list(rep(as.integer(NA), nmax)),
	# 		 inter_wing_dist = as.list(rep(as.integer(NA), nmax)),
	# 		 rank = as.list(rep(as.integer(NA), nmax)))

	# categorical
	for (n in 2:nmax['cat']) {
		zn = get_z_n(z[z$type == "cat",], n =n)

		if (!is.null(zn)) {
			q = do.call(rbind, lapply(zn$palette, check_cat_pal))
			r = -q#rank(-q, ties.method = "first")

			mn = cbind(q,r)

			a[match(zn$fullname, dimnames(a)[[1]]), c("min_dist", "rank"), n] = mn
		}
	}

	# sequential
	for (n in 2:nmax['seq']) {
		zn = get_z_n(z[z$type == "seq",], n =n)

		if (!is.null(zn)) {
			q = do.call(rbind, lapply(zn$palette, check_seq_pal))
			qr = q[,1] - q[,2] / 1000 # order min_step, those with equal store to -max_step

			r = -qr#rank(-qr, ties.method = "first")

			mn = cbind(q,r)

			a[match(zn$fullname, dimnames(a)[[1]]), c("min_step", "max_step", "rank"), n] = mn
		}
	}

	# diverging
	for (n in 2:nmax['div']) {
		zn = get_z_n(z[z$type == "div",], n =n)

		if (!is.null(zn)) {

			q = do.call(rbind, lapply(zn$palette, check_div_pal))
			qr = pmin(q[,1], q[,2] * 2)

			r = -qr#rank(-qr, ties.method = "first")

			mn = cbind(q,r)

			a[match(zn$fullname, dimnames(a)[[1]]), c("inter_wing_dist", "min_step", "rank"), n] = mn
		}
	}

	# bivariate seq-seq
	for (n in 3:nmax['bivs']) {
		zn = get_z_n(z[z$type == "bivs",], n = n, m = n)

		if (!is.null(zn)) {

			q = do.call(rbind, lapply(zn$palette, check_bivs_pal))
			qr = pmin(q[,1], q[,2] * 2)

			r = -qr#rank(-qr, ties.method = "first")

			mn = cbind(q,r)

			a[match(zn$fullname, dimnames(a)[[1]]), c("inter_wing_dist", "min_step", "rank"), n] = mn
		}
	}

	# bivariate cat-seq
	n = 3
	for (m in 3:nmax['bivc']) {
		zn = get_z_n(z[z$type == "bivc",], n = n, m = m)

		if (!is.null(zn)) {

			q = do.call(rbind, lapply(zn$palette, check_bivc_pal))
			qr = pmin(q[,1], q[,2] * 2)

			r = -qr#rank(-qr, ties.method = "first")

			mn = cbind(q,r)

			a[match(zn$fullname, dimnames(a)[[1]]), c("inter_wing_dist", "min_step", "rank"), n] = mn
		}
	}

	# bivariate unc-seq
	for (n in 3:nmax['bivu']) {
		zn = get_z_n(z[z$type == "bivu",], n = n)

		if (!is.null(zn)) {

			q = do.call(rbind, lapply(zn$palette, check_bivu_pal))
			qr = pmin(q[,1], q[,2] * 2)

			r = -qr#rank(-qr, ties.method = "first")

			mn = cbind(q,r)

			a[match(zn$fullname, dimnames(a)[[1]]), c("inter_wing_dist", "min_step", "rank"), n] = mn
		}
	}

	a
}
