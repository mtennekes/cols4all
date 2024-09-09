c4a_naming_matrix = function(pal) {
	do.call(.C4A$naming_fun, c(list(pal = pal), .C4A$naming_fun_args))
}

c4a_naming_matrix_softmax = function(pal, a = NA, th = NA) {
	m = c4a_naming_matrix(pal)
	if (is.na(a)) a = .C4A$naming_softmax$a
	if (is.na(th)) th = .C4A$naming_softmax$th
	matrix_softmax(m, a = a, th = th)

}


nameability = function(pal, a = NA, th = NA) {
	if (is.na(a)) a = .C4A$naming_softmax$a
	if (is.na(th)) th = .C4A$naming_softmax$th
	s = c4a_naming_matrix_softmax(pal, a = a, th = th)
	s[s>0] = 1

	max(colSums(s)) <= 1 && max(rowSums(s)) <= 1
}

name_max = function(pal) {
	m = c4a_naming_matrix(pal)
	apply(m, which.min, MARGIN = 1)
}

## method 1: difference to color centroids (e.g. boynton colors)
naming_dist_centroid = function(pal, weights) {
	m = diff_matrix(pal, .C4A$naming_colors)
}

diff_matrix = function(x, y) {
	xLAB = hex2LAB(x)
	yLAB = hex2LAB(y)

	t(apply(xLAB, MARGIN = 1, FUN = function(col) {
		spacesXYZ::DeltaE(col, yLAB, metric = "2000")
	}))
}

hex2LAB = function(x) {
	methods::as(colorspace::hex2RGB(x), "LAB")@coords
}


matrix_softmax = function(m, a, th) {
	s = t(apply(m, MARGIN = 1, softmax, a = a, simplify = T))
	s[s<th] = 0
	s
}

softmax = function(x, a = 1) {
	e = exp(1)
	ex = e^(-a*x)
	ex / sum(ex)
}




## method 2: H C L distributions per color (fitted on annotated dataset)
naming_sample_from_distribution = function(pal, model) {
	hcl = get_hcl_matrix(pal)
	nms = names(model)
	z = sapply(1:length(pal), function(i) {
		sapply(nms, function(nm) {
			s = sapply(c("H", "C", "L"), function(d) {
				r = model[[nm]][[d]]
				v = hcl[i, d]
				if (d == "H") {
					vs = c((v - r$mn) / (r$mx - r$mn),
						   ((v-360) - r$mn) / (r$mx - r$mn),
						   ((v+360) - r$mn) / (r$mx - r$mn))
				} else {
					vs = (v - r$mn) / (r$mx - r$mn)
				}
				sum(dbeta(vs, r$fit$estimate["shape1"], r$fit$estimate["shape2"]))
			})
			#unname(s[1] * s[3])
			prod(s)
		})
	})
	zt = t(z)
	zt2 = zt / rowSums(zt)
	-zt2
}




###

create_name_data = function() {
	hcl_df = data.frame(h = stats::runif(20000, min = 0, max = 360),
						c = stats::runif(20000, min = 0, max = 1),
						l = stats::runif(20000, min = 0, max = 100))

	hcl_df$max_c = colorspace::max_chroma(h = hcl_df$h, l = hcl_df$l)
	hcl_df$c = hcl_df$c * hcl_df$max_c
	allcols = hcl(hcl_df$h, hcl_df$c, hcl_df$l)
	allcols[1:length(.C4A$naming_colors)] = unname(.C4A$naming_colors) # to make sure every name color has matches, no matter what model parameters are used

	ids = name_max(allcols)

	x = split(allcols, ids)
	x = lapply(x, head, 200)
	x = lapply(x, function(xi) {
		if (length(xi) != 200) {
			xi = sample(xi, size = 200, replace = TRUE)
		} else {
			xi
		}
	})

	names(x) = names(.C4A$naming_colors)

	dfs = lapply(x, function(v) {

		n = length(v)
		m = 1000


		rads = sample(seq(0, .5, length.out = m), size = n, replace = TRUE, prob = 1:m)
		alphs = sample(seq(0, 360, length.out = m), size = n, replace = TRUE)

		df = data.frame(hex = v)
		df$x = 0.5 + rads * sin(alphs * 2 * pi)
		df$y = 0.5 + rads * cos(alphs * 2 * pi)
		df
	})
	names(dfs) = names(.C4A$naming_colors)
	dfs
}


update_nameability = function() {
	s = .C4A$s
	z = .C4A$z
	sname = .C4A$s[,dimnames(s)[[2]] == "nameability", ]
	pals = dimnames(sname)[[1]]
	cats = which(z$type == "cat")
	mins = z$nmin
	maxs = z$nmax

	m = do.call(rbind, mapply(function(pal, mn, mx) {
		res = logical(ncol(sname))
		res[mn:mx] = vapply(mn:mx, function(n) {
			p = c4a(pal, n = n)
			nameability(p)
		}, FUN.VALUE = logical(1))
		res
	}, pals[cats], mins[cats], maxs[cats], SIMPLIFY = FALSE, USE.NAMES = FALSE))
	sname[cats, ] = m
	.C4A$s[,dimnames(s)[[2]] == "nameability", ] = sname

}


