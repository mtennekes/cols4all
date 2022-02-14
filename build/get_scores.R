
get_scores = function(z, nmax) {
	k = nrow(z)

	sc = c("min_dist", "min_step", "max_step", "inter_wing_dist", "rank")
	a = array(as.integer(NA), dim = c(nrow(z), length(sc), nmax), dimnames = list(z$name, sc, NULL))

	# s = list(min_dist = as.list(rep(as.integer(NA), nmax)),
	# 		 min_step = as.list(rep(as.integer(NA), nmax)),
	# 		 max_step = as.list(rep(as.integer(NA), nmax)),
	# 		 inter_wing_dist = as.list(rep(as.integer(NA), nmax)),
	# 		 rank = as.list(rep(as.integer(NA), nmax)))

	# categorical
	for (n in 2:nmax) {
		zn = get_z_n(z[z$type == "cat",], n =n)
		q = do.call(rbind, lapply(zn$palette, check_cat_pal))
		r = rank(-q, ties.method = "first")

		mn = cbind(q,r)

		a[match(zn$name, dimnames(a)[[1]]), c("min_dist", "rank"), n] = mn
	}

	# sequential
	for (n in 2:nmax) {
		zn = get_z_n(z[z$type == "seq",], n =n)
		q = do.call(rbind, lapply(zn$palette, check_seq_pal))
		qr = q[,1] - q[,2] / 1000 # order min_step, those with equal store to -max_step

		r = rank(-qr, ties.method = "first")

		mn = cbind(q,r)

		a[match(zn$name, dimnames(a)[[1]]), c("min_step", "max_step", "rank"), n] = mn
	}

	# diverging
	for (n in 2:nmax) {
		zn = get_z_n(z[z$type == "div",], n =n)
		q = do.call(rbind, lapply(zn$palette, check_div_pal))
		qr = pmin(q[,1], q[,2] * 2)

		r = rank(-qr, ties.method = "first")

		mn = cbind(q,r)

		a[match(zn$name, dimnames(a)[[1]]), c("inter_wing_dist", "min_step", "rank"), n] = mn
	}

	a
}
