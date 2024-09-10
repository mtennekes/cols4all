aregreys = function(x) {
	all(get_hcl_matrix(x)[,2] < 1)
}

# ensure the diagonal are pure greys
convert2grey = function(x) {
	greys = rowSums(attr(colorspace::hex2RGB(x), "coords")) / 3
	rgb(greys, greys, greys)
}

create_biv_palette = function(palette, biv.method) {
	if (!(biv.method %in% c("div2seqseq", "div2catseq", "seq2uncseq", "byrow", "bycol"))) {
		n = as.integer(substr(biv.method, nchar(biv.method), nchar(biv.method)))
		biv.method = substr(biv.method, 1, nchar(biv.method) - 1)
		if (!(biv.method %in% c("div2seqseq", "byrow", "bycol"))) stop("Invalid biv.method", call. = FALSE)
	} else {
		np = length(palette)
		if (biv.method == "div2seqseq") {
			if ((np %% 2) != 1) stop("n is even but should be odd", call. = FALSE)
			n = (np - 1L)/2 + 1L
			m = n
		} else if (biv.method == "div2catseq") {
			if ((np %% 2) != 1) stop("n is even but should be odd", call. = FALSE)
			n = (np - 1L)/2 + 1L
			m = n - 1L
		} else if (biv.method == "seq2uncseq") {
			n = np
			m = 5
		} else {
			n = round(sqrt(length(palette)))
			m = round(length(palette) / n)
			if ((n * m) != length(palette)) stop("Please set the biv.method to \"", biv.method, "X\", where X is the number of columns", call. = FALSE)
		}
	}

	if (biv.method == "div2seqseq") {
		a = get_hcl_matrix(palette)

		# hue matrix
		h1 = matrix(a[n:1, 1], nrow = n, ncol = n, byrow = FALSE)
		h2 = matrix(a[n:np, 1], nrow = n, ncol = n, byrow = TRUE)
		h = matrix(a[n, 1], nrow = n, ncol = n)
		h[lower.tri(h)] = h1[lower.tri(h)]
		h[upper.tri(h)] = h2[upper.tri(h)]

		# chroma matrix
		cr1 = a[n:1, 2]
		cr2 = a[n:np, 2]

		cr = matrix(0, nrow = n, ncol = n)
		for (i in 2:n) {
			cr[i, 1:i] = seq(cr1[i], 0, length.out = i)
			cr[1:i, i] = seq(cr2[i], 0, length.out = i)
		}

		# limunance matrix
		la1 = a[n:1, 3]
		la2 = a[n:np, 3]
		la0 = (la1 + la2) / 2

		la0b = local({
			x = 1:n
			y = la0
			fit2 <- stats::lm(y~poly(x,2,raw=TRUE))
			xx <- seq(1,n*sqrt(2),length.out=n)
			stats::predict(fit2, data.frame(x=xx))
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

		mat = t(matrix(do.call(grDevices::hcl, list(h = h, c = cr, l = l)), ncol = n, byrow = TRUE))
		diag(mat) = convert2grey(diag(mat))
		mat
	} else if (biv.method == "div2catseq") {
		a = get_hcl_matrix(palette)

		l1 = a[m:1, 3]
		l2 = a[(m+2):np, 3]
		l0 = (l1 + l2) / 2
		h1 = a[m:1, 1]
		h2 = a[(m+2):np, 1]

		c1 = a[m:1, 2]
		c2 = a[(m+2):np, 2]

		h = matrix(c(h1, h1, h1, h2, h2), ncol = n, byrow = FALSE)
		cr = matrix(c(c1, c1/2, rep(0,m), c2/2, c2), ncol = n, byrow = FALSE)
		l = matrix(c(l1, (l1+l0)/2, l0, (l2+l0)/2, l2), ncol = n, byrow = FALSE)
		mat = matrix(do.call(grDevices::hcl, list(h = h, c = cr, l = l)), ncol = n, byrow = FALSE)

		# convert middle col to pure greys if they are
		mid_col = mat[, (n+1)/2]
		if (aregreys(mid_col)) {
			mat[, (n+1)/2] = convert2grey(mid_col)
		}
		mat
	} else if (biv.method == "seq2uncseq") {
		a = get_hcl_matrix(palette)
		b = a
		b[,2] = 0
		pa = grDevices::hcl(h  = a[,1], c = a[,2], l = a[,3])
		pb = grDevices::hcl(h  = b[,1], c = b[,2], l = b[,3])
		pb = convert2grey(pb)
		unname(do.call(rbind, mapply(function(ca, cb) {
			colorRampPalette(c(ca, cb))(m)
		}, pa, pb, SIMPLIFY = FALSE)))

	} else {
		matrix(palette, ncol = n, byrow = biv.method == "byrow")
	}
}


