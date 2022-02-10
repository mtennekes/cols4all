#' Check diverging palette
#'
#' Check diverging palette. It computes two quality indices. \code{inter_wing_dist} minimal distance between from one wing (any color) to the other wing (any color); let a step be the distance from one color to a neighboring color, then: \code{min_step} is the minimal step. These two quality indices are computed for all three color vision deficiency types: per quality indicator, the worst score is returned.
#'
#' @param p diverging palette with odd number of colors (so with a middle color)
#' @return vector of three quality indices
check_div_pal = function(p) {
	n = length(p)
	if ((n %% 2) != 1) stop("p needs to be odd-numbered")
	nh = floor(n/2)

	cvds = c("deu", "pro", "tri")

	scores = t(sapply(cvds, function(cvd) {
		m = colorblindcheck::palette_dist(p, cvd = cvd)
		inter_wing_dist = min(m[1:nh, (nh+2):n])
		step_sizes = mapply(function(i,j) m[i,j], 1:(n-1), 2:n)
		min_step_size = min(step_sizes)
		#mean_step_size = mean(step_sizes)
		#step_indicator = max(abs(step_sizes - mean_step_size)) / mean_step_size
		c(inter_wing_dist = round(inter_wing_dist), min_step = round(min_step_size))
	}))
	c(inter_wing_dist = min(scores[,1]), min_step = min(scores[,2]))
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


check_cat_list = function(x) {
	lapply(x, function(xi) {
		index = attr(xi, "index")
		if (is.null(index)) {
			check_cat_pal(xi)
		} else {
			lapply(index, function(ind) {
				pal = xi[ind]
				check_cat_pal(pal)
			})
		}
	})
}
