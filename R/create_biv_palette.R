create_biv_palette = function(palette, biv.method) {
	if (!(biv.method %in% c("fromdiv", "byrow", "bycol"))) {
		n = as.integer(substr(biv.method, nchar(biv.method), nchar(biv.method)))
		biv.method = substr(biv.method, 1, nchar(biv.method) - 1)
		if (!(biv.method %in% c("fromdiv", "byrow", "bycol"))) stop("Invalid biv.method", call. = FALSE)
	} else {
		if (biv.method == "fromdiv") {
			n = (length(palette) - 1L)/2 + 1L
			m = n
		} else {
			n = round(sqrt(length(palette)))
			m = round(length(palette) / n)
			if ((n * m) != length(palette)) stop("Please specify number of columns X with \"", biv.method, "X\"", call. = FALSE)
		}
	}

	if (biv.method == "fromdiv") {
		a = get_hcl_matrix(palette)

		np = length(palette)

		h1 = matrix(a[n:1, 1], nrow = n, ncol = n, byrow = FALSE)
		h2 = matrix(a[n:np, 1], nrow = n, ncol = n, byrow = TRUE)
		h = matrix(a[n, 1], nrow = n, ncol = n)
		h[lower.tri(h)] = h1[lower.tri(h)]
		h[upper.tri(h)] = h2[upper.tri(h)]

		cr1 = a[n:1, 2]
		cr2 = a[n:np, 2]

		cr = matrix(0, nrow = n, ncol = n)
		for (i in 2:n) {
			cr[i, 1:i] = seq(cr1[i], 0, length.out = i)
			cr[1:i, i] = seq(cr2[i], 0, length.out = i)
		}


		la1 = a[n:1, 3]
		la2 = a[n:np, 3]
		la0 = (la1 + la2) / 2

		la0b = local({
			x = 1:n
			y = la0
			fit2 <- lm(y~poly(x,2,raw=TRUE))
			xx <- seq(1,n*sqrt(2),length.out=n)
			predict(fit2, data.frame(x=xx))
		})

		l = matrix(la0[1], nrow = n, ncol = n)

		for (i in 2:n) {
			l[i, 1:i] = seq(la1[i], la0b[i], length.out = i)
			l[1:i, i] = seq(la2[i], la0b[i], length.out = i)
		}
		l[l<0] = 0
		l[l>100] = 100


		# l = matrix(la0, nrow = n, ncol = n)
		# l1 = t(mapply(seq, la1, la0, length.out = n))
		# l2 = mapply(seq, la2, la0, length.out = n)
		# l[lower.tri(l)] = l1[lower.tri(l1)]
		# l[upper.tri(l)] = l2[upper.tri(l2)]

		matrix(do.call(hcl, list(h = h, c = cr, l = l)), ncol = n, byrow = TRUE)
	} else {
		matrix(palette, ncol = n, byrow = biv.method == "byrow")
	}
}

