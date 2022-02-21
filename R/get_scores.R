get_scores_zn = function(zn) {
	type = zn$type[1]
	if (any(zn$type != type)) stop("get_scores_zn only works for one type")
	n = zn$n[1]

	fun = paste0("check_", type, "_pal")

	q = do.call(rbind, lapply(zn$palette, get(fun)))

	if (type == "cat") {
		r = -q
	} else if (type == "seq") {
		qr = q[,1] - q[,2] / 1000 # order min_step, those with equal store to -max_step
		r = -qr#rank(-qr, ties.method = "first")
	} else if (type == "div") {
		qr = pmin(q[,1], q[,3] * 2) + (q[,2] >= 100) * 1000
		r = -qr#rank(-qr, ties.method = "first")
	}
	colnames(r) = "rank"
	mn = cbind(q,r)
	cbind(zn, mn)
}

get_scores = function(z, nmax = c(cat = 36, seq = 15, div = 15)) {
	k = nrow(z)


	nmaxmax = max(nmax)

	sc = c("min_dist", "min_step", "max_step", "inter_wing_dist", "inter_wing_hue_dist", "rank")
	a = array(as.integer(NA), dim = c(nrow(z), length(sc), nmaxmax), dimnames = list(z$fullname, sc, NULL))

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
			qr = pmin(q[,1], q[,3] * 2) + (q[,2] >= 100) * 1000

			r = -qr#rank(-qr, ties.method = "first")

			mn = cbind(q,r)

			a[match(zn$fullname, dimnames(a)[[1]]), c("inter_wing_dist", "inter_wing_hue_dist", "min_step", "rank"), n] = mn
		}
	}

	a
}
