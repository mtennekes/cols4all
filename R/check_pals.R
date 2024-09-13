# Check diverging palette
#
# Check diverging palette. It computes two quality indices. \code{inter_wing_dist} minimal distance between from one wing (any color) to the other wing (any color); let a step be the distance from one color to a neighboring color, then: \code{min_step} is the minimal step. These two quality indices are computed for all three color vision deficiency types: per quality indicator, the worst score is returned.
#
# @param p diverging palette with odd number of colors (so with a middle color)
# @return vector of three quality indices
check_div_pal = function(p) {
	n = length(p)

	is_even = ((n %% 2) != 1)
	nh = floor(n/2)

	# needed for inter_wing_dist
	p2 = c(rampPal(p[1:nh], 9), rampPal(p[(nh+1+(!is_even)):n], 9))
	n2 = 18
	nh2 = n2 / 2

	cvds = c("deutan", "protan", "tritan")

	scores = t(sapply(cvds, function(cvd) {
		inter_wing_dist = local({
			dm = get_dist_matrix(p2, cvd = cvd)
			min(dm[1:nh2, (nh2+1):n2])
		})

		min_step_size = local({
			dm = get_dist_matrix(p, cvd = cvd)
			step_sizes = mapply(function(i,j) dm[i,j], 1:(n-1), 2:n)
			min(step_sizes)
		})
		c(inter_wing_dist = round(inter_wing_dist * 100), min_step = round(min_step_size * 100))
	}))
	inter_wing_dist = min(scores[,1])
	min_step = min(scores[,2])


	sc = as(c(inter_wing_dist = inter_wing_dist, min_step = min_step), "integer")
	prop = hcl_prop(p)
	rgb = rgb_prop(p)

	c(sc, prop, rgb)
}


check_bivs_pal = function(p) {
	if (nrow(p) != ncol(p)) {
		stop("ncol != nrow", call. = FALSE)
	}

	p1 = p[1,]
	p2 = p[,1]
	pd = diag(p)

	#if (nrow(p) == 5) browser()

	x12 = check_div_pal(c(rev(p1[-1]), p2))
	x1d = check_div_pal(c(rev(p1[-1]), pd))
	x2d = check_div_pal(c(rev(p2[-1]), pd))

	sc = pmin(x12, x1d, x2d)[1:2]

	p2 = c(as.vector(p[lower.tri(p)]), p[1,1], as.vector(p[upper.tri(p)]))

	prop = hcl_prop(p2)
	rgb = rgb_prop(p)

	c(sc, prop, rgb)
}

check_bivc_pal = function(p) {
	nr = nrow(p)

	res = lapply(1L:nr, function(i) {
		check_cat_pal(p[i, ])
	})

	sc = do.call(pmin, res)[1:2]

	p2 = as.vector(p)

	prop = hcl_prop(p2)
	rgb = rgb_prop(p)

	c(sc, prop, rgb)
}

check_bivd_pal = function(p) {
	# if (ncol(p) %% 2 == 0) {
	# 	stop("ncol should be odd", call. = FALSE)
	# }

	c1 = 1
	c3 = ncol(p)
	c2 = (c1+c3)/2


	x13 = check_div_pal(c(rev(p[,c1]), "#FFFFFF", p[,c3]))
	x12 = check_div_pal(c(rev(p[,c1]), "#FFFFFF", p[,c2]))
	x23 = check_div_pal(c(rev(p[,c2]), "#FFFFFF", p[,c3]))

	sc = pmin(x12, x13, x23)[1:2]

	p2 = c(rev(p[, 1]), p[1, round((ncol(p)+1)/2)], p[, ncol(p)])
	prop = hcl_prop(p2)
	rgb = rgb_prop(p)

	c(sc, prop, rgb)
}

check_bivg_pal = function(p) {
	sc = check_div_pal(c(rev(p[,1]), "#FFFFFF", p[,ncol(p)]))[1:2]

	p2 = c(rev(p[, 1]), p[1, round((ncol(p)+1)/2)], p[, ncol(p)])
	prop = hcl_prop(p2)
	rgb = rgb_prop(p)

	c(sc, prop, rgb)

}


# Check sequential palette
#
# Check sequential palette. It computes two quality indices. \code{min_step} and \code{max_step} are the minimum and maximum step respectively, where a step is the distance from one color to a neighboring color. \code{min_step} is the leading indicator: the higher, the better the palette. From palettes with equal \code{min_step}, those with the lowest \code{max_step} can be considered as better, because the steps are more uniform. These two quality indices are computed for all three color vision deficiency types: per quality indicator, the worst score is returned.
#
# and \code{max_step} is low, although the former is much more important.
#
# @param p sequential palette
# @return vector of three quality indices
check_seq_pal = function(p) {
	n = length(p)

	cvds = c("deutan", "protan", "tritan")

	scores = t(sapply(cvds, function(cvd) {
		m = get_dist_matrix(p, cvd = cvd)
		step_sizes = mapply(function(i,j) m[i,j], 1:(n-1), 2:n)
		min_step_size = min(step_sizes)
		max_step_size = max(step_sizes)
		#mean_step_size = mean(step_sizes)
		#step_indicator = max(abs(step_sizes - mean_step_size)) / mean_step_size
		min_dist = min(m, na.rm = TRUE)

		c(min_step = round(min_step_size * 100), max_step = round(max_step_size * 100), min_dist = round(min_dist * 100))
	}))

	sc = as(c(min_step = min(scores[,1]), max_step = min(scores[,2]), min_dist = min(scores[,3])), "integer")
	prop = hcl_prop(p)
	rgb = rgb_prop(p)

	c(sc, prop, rgb)
}

# Check cyclic palette
#
check_cyc_pal = function(p) {
	if (p[1] != tail(p,1)) stop("first color should be equal to last color")
	check_seq_pal(head(p, -1))
}

# Check categorical palette
#
# Check categorical palette. It computes one quality indicator: the \code{min_dist}, the minimal distance between any two colors. This is computed for all three color vision deficiency types: the worst (i.e. lowest) score is returned.
check_cat_pal = function(p) {
	if (length(p) == 1) return(c(min_dist = Inf))
	cvds = c("deutan", "protan", "tritan")

	scores = sapply(cvds, function(cvd) {
		get_dist_matrix(p, cvd = cvd)
	})

	sc = c(min_dist = as.integer(round(min(scores, na.rm = TRUE) * 100)), nameability = as.integer(nameability(p)))
	prop = hcl_prop(p)
	rgb = rgb_prop(p)

	c(sc, prop, rgb)
}


is_light <- function(col) {
	colrgb <- col2rgb(col)
	apply(colrgb * c(.299, .587, .114), MARGIN=2, sum) >= 128
}




# get hcl coordinates
get_hcl_matrix = function(p, rounded = FALSE) {
	x = as(colorspace::hex2RGB(p), "polarLUV")@coords[,c("H", "C", "L"), drop = FALSE]
	if (rounded) round(x) else x
}

get_hc_or_l = function(p, dim = c("H", "C", "L")) {
	dim = match.arg(dim)
	x = as(colorspace::hex2RGB(p), "polarLUV")@coords[, dim]
}


get_hcl_triple = function(p) {
	x = get_hcl_matrix(p, rounded = TRUE)
	apply(x, MARGIN = 1, paste, collapse = ",")
}

get_rgb_triple = function(p) {
	x = round(colorspace::hex2RGB(p)@coords * 255)
	#paste0("(", apply(x, MARGIN = 1, paste, collapse = ", "), ")")
	apply(x, MARGIN = 1, paste, collapse = ",")
}


# hue width: h)w far are hues apart from each other?
# method: find largest gap, i.e. hue range for which no hues are present. Hwidth = 360 - gap
get_hue_width = function(hs) {
	hs = na.omit(hs)
	if (!length(hs)) {
		w = 0
		h_max = 0
	} else {
		hs = c(hs, hs + 360)
		gap = 0
		gap_max = 0
		h_max = 0
		for (h in 0:720) {
			if (any(hs == h)) {
				gap = 0
			} else {
				gap = gap + 1
			}
			if (gap > gap_max) {
				gap_max = gap
				h_max = h
			}
		}
		w = round(360 - gap_max)
	}
	HR = (h_max + w) %% 360
	HL = (h_max + 1) %% 360

	if (HR < HL) HR = HR + 360

	H = (HL + HR) / 2
	if (H > 360) H = H - 360

	attr(w, "hueR") = HR
	attr(w, "hueL") = HL
	attr(w, "hue") = H

	w
}



# HCL characteristics
# analyse_hcl = function(p, type) {
#
#
# 	if (type == "bivs") {
# 		p = c(as.vector(p[lower.tri(p)]), p[1,1], as.vector(p[upper.tri(p)]))
# 	} else if (type == "bivd") {
# 		p = c(rev(p[, 1]), p[1, round((ncol(p)+1)/2)], p[, ncol(p)])
# 	} else if (type == "bivg") {
# 		p = c(rev(p[, 1]), p[1, round((ncol(p)+1)/2)], p[, ncol(p)])
# 	} else if (type == "bivc") {
# 		p = as.vector(p)
# 	}
#
#
# 	c(rgb_prop(p), hcl_prop(p))
# }

# approx_wave = function(p) {
# 	co = unname(t(col2rgb(p)))
# 	co[rowSums(co) == 0, ] = 1
#
# 	round((co[,3] * 440 + co[,2] * 540 + co[,1] * 565) / rowSums(co))
# }


approx_blues = function(p) {
	co = unname(t(col2rgb(p)))
	co[rowSums(co) == 0, ] = 1
	round(co[,3] / apply(co[,1:2], MARGIN = 1, max) * 100)
}


approx_reds = function(p) {
	co = unname(t(col2rgb(p)))
	co[rowSums(co) == 0, ] = 1
	round(co[,1] / apply(co[,2:3], MARGIN = 1, max) * 100)
}


rgb_prop = function(p) {

	blues = approx_blues(p)


	c(Blues = max(blues))
}

hcl_prop = function(p) {
	m = get_hcl_matrix(p)

	# hue width: how far are hues apart from each other?
	h = round(m[,1])
	h[m[,2]<=.C4A$Cgray] = NA

	Hwidth = get_hue_width(h)

	n = length(p)
	is_even = ((n %% 2) != 1)
	nh = floor(n/2)

	hL = h[1:nh]
	hR = h[(nh+1+!is_even):n]


	Hwidth = get_hue_width(h)
	HwidthL = get_hue_width(hL)
	HwidthR = get_hue_width(hR)

	H = round(attr(Hwidth, "hue"))
	HL = round(attr(HwidthL, "hue"))
	HR = round(attr(HwidthR, "hue"))

	Lmid = unname(round({if (!is_even) m[nh+1, 3] else mean(m[c(nh, nh+1), 3])}))

	Cmax = round(max(m[,2]))
	Lrange = round(max(m[,3]) - min(m[,3]))
	Crange = round(max(m[,2]) - min(m[,2]))

	#LCrange = round(max(Lrange * .C4A$LrangeWeight, Crange * (1-.C4A$LrangeWeight)))

	CRmin = round(get_CRmin(p) * 100)
	CRwt = round(get_CRbg(p, bg = "#ffffff") * 100)
	CRbk = round(get_CRbg(p, bg = "#000000") * 100)

	as(c(Cmax = Cmax, H = H, HL = HL, HR = HR, Lmid = Lmid, Hwidth = Hwidth, HwidthL = HwidthL, HwidthR = HwidthR, Lrange = Lrange, Crange = Crange, CRmin = CRmin, CRwt = CRwt, CRbk = CRbk), "integer") #LCrange = LCrange,
}
#
# encode = function(x, digits = 0, id1 = 0L, id2 = 0L) {
# 	as.integer(round(x, digits = digits) * 10^digits + id1 * 10000 + id2 * 1000000)
# }


get_CRbg = function(p, bg = "#ffffff") {
	n = length(p)
	CRs = sapply(p, function(pi) colorspace::contrast_ratio(pi, bg))
	id = which.min(CRs)[1]
	unname(CRs[id])
	#structure(CRs[id], names = id)
}


get_CRmin = function(p, show.which = FALSE) {
	n = length(p)
	CRs = sapply(1:(n-1), function(i) {
		CRs = sapply(p[(i+1):n], function(pj) colorspace::contrast_ratio(p[i], pj))
		id = which.min(CRs)[1]
		structure(CRs[id], names = paste(i, id + i, sep = "_"))
	})
	id = which.min(CRs)[1]
	if (show.which) {
		message("id:", names(CRs)[id])
		ids = as.integer(strsplit(names(CRs)[id], split = "_", fixed = TRUE)[[1]])
		c4a_plot_Plus_Reversed(p[ids[1]], p[ids[2]])
	}
	unname(CRs[id])
	#structure(CRs[id], ids = strsplit(names(CRs[id]), split = "_", fixed = TRUE))

}


