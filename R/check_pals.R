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

	cvds = c("deu", "pro", "tri")

	scores = t(sapply(cvds, function(cvd) {
		m = colorblindcheck::palette_dist(p, cvd = cvd)
		if (is_even) {
			inter_wing_dist = min(m[1:nh, (nh+1):n])
		} else {
			inter_wing_dist = min(m[1:nh, (nh+2):n])
		}
		step_sizes = mapply(function(i,j) m[i,j], 1:(n-1), 2:n)

		min_step_size = min(step_sizes)

		p2 = switch(cvd,
					deu = colorspace::deutan(p),
					pro = colorspace::protan(p),
					tri = colorspace::tritan(p),
					p)
		m = get_hcl_matrix(p2)
		m[,3][m[,2]<10] = NA

		h1 = m[1:nh, 3]
		h2 = m[(nh+1+!is_even):n, 3]

		inter_wing_hue_dist = if (all(is.na(h1)) && all(is.na(h2))) {
			0
		} else if (all(is.na(h1)) || all(is.na(h2))) {
			120 # actually depends on chroma of other hues
		} else {
			m2 = expand.grid(h1 = h1, h2 = h2)
			inter_wing_hue_dist = min(abs(m2$h2 - m2$h1), na.rm = TRUE)
		}

		c(inter_wing_dist = round(inter_wing_dist), inter_wing_hue_dist = round(inter_wing_hue_dist), min_step = round(min_step_size))
	}))
	inter_wing_dist = min(scores[,1])
	inter_wing_hue_dist = min(scores[,2])
	min_step = min(scores[,3])


	c(inter_wing_dist = inter_wing_dist, inter_wing_hue_dist = inter_wing_hue_dist, min_step = min_step)
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

	cvds = c("deu", "pro", "tri")

	scores = t(sapply(cvds, function(cvd) {
		m = colorblindcheck::palette_dist(p, cvd = cvd)
		step_sizes = mapply(function(i,j) m[i,j], 1:(n-1), 2:n)
		min_step_size = min(step_sizes)
		max_step_size = max(step_sizes)
		#mean_step_size = mean(step_sizes)
		#step_indicator = max(abs(step_sizes - mean_step_size)) / mean_step_size

		c(min_step = round(min_step_size), max_step = round(max_step_size))
	}))
	c(min_step = min(scores[,1]), max_step = min(scores[,2]))
}

# Check cyclic palette
#
# Check cyclic palette. Same as \code{check_seq_pal}, but also the difference between the first and last color is considered as step
#
check_cyc_pal = function(p) {
	n = length(p)
	cvds = c("deu", "pro", "tri")

	scores = t(sapply(cvds, function(cvd) {
		m = colorblindcheck::palette_dist(c(p, p[1]), cvd = cvd)
		step_sizes = mapply(function(i,j) m[i,j], 1:n, 2:(n+1))
		min_step_size = min(step_sizes)
		max_step_size = max(step_sizes)
		#mean_step_size = mean(step_sizes)
		#step_indicator = max(abs(step_sizes - mean_step_size)) / mean_step_size
		c(min_step = round(min_step_size), max_step = round(max_step_size))
	}))
	c(min_step = min(scores[,1]), max_step = min(scores[,2]))
}

# Check categorical palette
#
# Check categorical palette. It computes one quality indicator: the \code{min_dist}, the minimal distance between any two colors. This is computed for all three color vision deficiency types: the worst (i.e. lowest) score is returned.
check_cat_pal = function(p) {
	if (length(p) == 1) return(c(min_dist = Inf))
	cvds = c("deu", "pro", "tri")

	scores = sapply(cvds, function(cvd) {
		colorblindcheck::palette_dist(p, cvd = cvd)
	})
	c(min_dist = round(min(scores, na.rm = TRUE)))
}



# get hcl coordinates
get_hcl_matrix = function(p) {
	as(hex2RGB(p), "polarLUV")@coords
}

# hue width: how far are hues apart from each other?
# method: find largest gap, i.e. hue range for which no hues are present. Hwidth = 360 - gap
get_hue_width = function(hs) {
	hs = na.omit(hs)
	if (!length(hs)) {
		0
	} else {
		hs = c(hs, hs + 360)
		gap = 0
		gap_max = 0
		for (h in 0:720) {
			if (any(hs == h)) {
				gap = 0
			} else {
				gap = gap + 1
			}
			if (gap > gap_max) gap_max = gap
		}
		Hwidth = round(360 - gap_max)
	}

}

# HCL characteristics
analyse_hcl = function(p) {
	m = get_hcl_matrix(p)

	# hue width: how far are hues apart from each other?
	h = round(m[,3])
	h[m[,2]<=10] = NA

	Hwidth = get_hue_width(h)

	n = length(p)
	is_even = ((n %% 2) != 1)
	nh = floor(n/2)

	hL = h[1:nh]
	hR = h[(nh+1+!is_even):n]


	Hwidth = get_hue_width(h)
	HwidthL = get_hue_width(hL)
	HwidthR = get_hue_width(hR)


	#Crels = pmin(round((m[,2] / .maxC[round(m[,3]) + 1]) * 100), 100)


	Cmax = round(max(m[,2]))

	#Crel = max(Crels)

	# Lmin = min(m[,1]), Lmax = max(m[,1]),
	# Cmin = min(m[,2]), Cmax = max(m[,2]),

	Lrange = round(max(m[,1]) - min(m[,1]))
	#Crange = round(max(Crels) - min(Crels))
	Crange = round(max(m[,2]) - min(m[,2]))

	# LCrange = round((Lrange + Crange) / 2)
	# LCrange = round((Lrange*2 + Crange) / 3)
	LCrange = round(max(Lrange*2, Crange))


	c(Cmax = Cmax, Hwidth = Hwidth, HwidthL = HwidthL, HwidthR = HwidthR, Lrange = Lrange, Crange = Crange, LCrange = LCrange)
}

