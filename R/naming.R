
boynton = c(Green = "#859F68", Blue = "#5792A4", Purple = "#7E6A89", Pink = "#C7848F",
			Yellow = "#E7B352", Brown = "#8F5F49", Orange = "#D97447", Red = "#9D4149",
			White = "#D8CEBA", Gray = "#868782", Black = "#394245")

softmax = function(x, a = 1) {
	e = exp(1)
	ex = e^(-a*x)
	ex / sum(ex)
}


hex2LAB = function(x) {
	methods::as(colorspace::hex2RGB(x), "LAB")@coords
}




diff_matrix = function(x, y) {
	xLAB = hex2LAB(x)
	yLAB = hex2LAB(y)

	t(apply(xLAB, MARGIN = 1, FUN = function(col) {
		spacesXYZ::DeltaE(col, yLAB, metric = "2000")
	}))
}

diff_boynton = function(pal) {
	m = diff_matrix(pal, boynton)
	m / rep(.C4A$boynton_weights, each = nrow(m))
}

diff_boynton_softmax = function(pal, a = 2, th = 0.1) {
	m = diff_boynton(pal)
	s = t(apply(m, MARGIN = 1, softmax, a = a, simplify = T))
	s[s<th] = 0
	s
}

# softmax_matrix = function(x, y, a = 2) {
# 	m = diff_matrix(x, y)
# 	t(apply(m, MARGIN = 1, softmax, a = a, simplify = T))
# }


# naming_score_matrix = function(pal, a = 2, th = 0.1) {
# 	s = softmax_matrix(pal, boynton, a = a)
# 	s[s<th] = 0
# 	s
# }

# match_colors = function(pal, a = 2, th = .1) {
# 	s = diff_boynton_softmax(pal, a, th)
# 	apply(s, MARGIN = 2, function(a) {
# 		ids = which(a>0)
# 		ids[order(a[ids], decreasing = TRUE)]
# 	}, simplify = FALSE)
# }

nameability = function(pal, a = 2, th = .1) {
	s = diff_boynton_softmax(pal, a = a, th = th)
	s[s>0] = 1

	max(colSums(s)) <= 1 && max(rowSums(s)) <= 1
}

# naming_scores = function(pal, a = 2, th = .1) {
# 	s = softmax_matrix(pal, boynton, a = a)
# 	s[s<th] = 0
# 	apply(s, MARGIN = 1, function(x) min(x[x!=0]))
# }
name_max = function(pal) {
	m = diff_boynton(pal)
	apply(m, which.min, MARGIN = 1)
}


create_name_data = function() {
	hcl_df = data.frame(h = stats::runif(20000, min = 0, max = 360),
						c = stats::runif(20000, min = 0, max = 1),
						l = stats::runif(20000, min = 0, max = 100))

	hcl_df$max_c = colorspace::max_chroma(h = hcl_df$h, l = hcl_df$l)
	hcl_df$c = hcl_df$c * hcl_df$max_c
	allcols = hcl(hcl_df$h, hcl_df$c, hcl_df$l)
	allcols[1:11] = unname(boynton) # to make sure every boynton color has matches, no matter what weights are used

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

	names(x) = names(boynton)

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
	names(dfs) = names(boynton)
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


