#' Check diverging palette
#'
#' Check diverging palette. It computes two quality indices. \code{inter_wing_dist} minimal distance between from one wing (any color) to the other wing (any color); let a step be the distance from one color to a neighboring color, then: \code{min_step} is the minimal step. These two quality indices are computed for all three color vision deficiency types: per quality indicator, the worst score is returned.
#'
#' @param p diverging palette with odd number of colors (so with a middle color)
#' @return vector of three quality indices
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
		m2 = expand.grid(x1 = m[1:nh, 3], x2 = m[(nh+1+!is_even):n, 3])
		inter_wing_hue_dist = min(abs(m2$x2 - m2$x1), na.rm = TRUE)

		c(inter_wing_dist = round(inter_wing_dist), inter_wing_hue_dist = round(inter_wing_hue_dist), min_step = round(min_step_size))
	}))
	inter_wing_dist = min(scores[,1])
	inter_wing_hue_dist = min(scores[,2])
	min_step = min(scores[,3])


	c(inter_wing_dist = inter_wing_dist, inter_wing_hue_dist = inter_wing_hue_dist, min_step = min_step)
}


#' Check sequential palette
#'
#' Check sequential palette. It computes two quality indices. \code{min_step} and \code{max_step} are the minimum and maximum step respectively, where a step is the distance from one color to a neighboring color. \code{min_step} is the leading indicator: the higher, the better the palette. From palettes with equal \code{min_step}, those with the lowest \code{max_step} can be considered as better, because the steps are more uniform. These two quality indices are computed for all three color vision deficiency types: per quality indicator, the worst score is returned.
#'
#' and \code{max_step} is low, although the former is much more important.
#'
#' @param p sequential palette
#' @return vector of three quality indices
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

#' Check cyclic palette
#'
#' Check cyclic palette. Same as \code{check_seq_pal}, but also the difference between the first and last color is considered as step
#'
#' @param p cyclic palette
#' @return vector of three quality indices
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

#' Check categorical palette
#'
#' Check categorical palette. It computes one quality indicator: the \code{min_dist}, the minimal distance between any two colors. This is computed for all three color vision deficiency types: the worst (i.e. lowest) score is returned.
#'
#' and \code{max_step} is low, although the former is much more important.
#'
#' @param p sequential palette
#' @return vector of three quality indices
check_cat_pal = function(p) {
	if (length(p) == 1) return(c(min_dist = Inf))
	cvds = c("deu", "pro", "tri")

	scores = sapply(cvds, function(cvd) {
		colorblindcheck::palette_dist(p, cvd = cvd)
	})
	c(min_dist = round(min(scores, na.rm = TRUE)))
}


#' Calculate the minimum distance a palette (any color in this palette) and a color.
dist_to_col = function(pal, col) {
	colorblindcheck::palette_dist(c(col, pal))[1,-1]
}

#' Function to remove (near) blacks and whites
remove_black_white = function(pal, th = 5) {
	blcks = dist_to_col(pal, "#000000") <= th
	almost_blcks = dist_to_col(pal, "#0D0D0D") <= th # 1/20 grey

	whts = dist_to_col(pal, "#FFFFFF") <= th
	almost_whts = dist_to_col(pal, "#F2F2F2") <= th # 19/20 grey
	pal[!blcks & !whts & !almost_blcks & !almost_blcks]
}

#' Determine if colors are light (otherwise dark, so binary).
is_light <- function(col) {
	colrgb <- col2rgb(col)
	apply(colrgb * c(.299, .587, .114), MARGIN=2, sum) >= 128
}

# get hcl coordinates
get_hcl_matrix = function(p) {
	as(hex2RGB(p), "polarLUV")@coords
}

#' HCL characteristics
analyse_hcl = function(pals) {
	t(sapply(pals, function(p) {
		m = get_hcl_matrix(p)

		# find largest hue gap
		hs = round(unique(m[,3][m[,2]>10]))
		if (!length(hs)) {
			Hwidth = 0
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
		Crel = min(max(round((m[,2] / .maxC[round(m[,3]) + 1]) * 100)), 100)


		# Lmin = min(m[,1]), Lmax = max(m[,1]),
		# Cmin = min(m[,2]), Cmax = max(m[,2]),



		c(Crel = Crel, Hwidth = Hwidth)
	}))
}

