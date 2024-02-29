palette_dist_bg = function (x, bgcol = "#FFFFFF", cvd = NULL, severity = 1, metric = 2000) {
	if (is.null(bgcol)) {
		colorblindcheck::palette_dist(x, cvd = cvd, severity = severity, metric = metric)
	} else {
		n = length(x)
		x2 = c(x, bgcol)

		m2 = colorblindcheck::palette_dist(x2, cvd = cvd, severity = severity, metric = metric)

		bdiff = m2[1:n,n+1]

		b = matrix(((rep(bdiff, times = n) + rep(bdiff, each = n)) / 2), nrow = n)

		m = m2[1:n, 1:n]
		(m^2) / b
	}
}
