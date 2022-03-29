# Get the default range
#
# Get the default range for gradient palettes, so sequential `"seq"` and diverging `"div"`. The range is a vector of two numbers between 0 and 1 that determine the range that is used for sequential and diverging palettes. The first number determines where the palette begins, and the second number where it ends. For sequential `"seq"` palettes, 0 means the leftmost (normally lightest) color, and 1 the rightmost (often darkest) color. For diverging `"seq"` palettes, 0 means the middle color, and 1 both extremes. If only one number is provided, this number is interpreted as the endpoint (with 0 taken as the start). This function sets the default based on the number of colors `n`: for small values of `n` the default range is smaller than `c(0, 1)` for aesthetically more pleasing palettes. See `c4a_gui` how the range effects the palettes. See also examples below. The range can be set manually with the argument `range` of \code{\link{c4a}}.
range_seq = function(n = 9, nmin = 3, nmax = 9, slope_min = 1/30, slope_max = 1/30) {
	nmax = min(nmax, n)

	ns = nmin:nmax

	# n x 2 matrix with 0 1 (full range)
	rm = matrix(c(0,1), nrow = n, ncol = 2, byrow = TRUE)

	# lb and ub between nmin and nmax
	lb = slope_min * (rev(ns) - nmin)
	ub = 1 - slope_max * (rev(ns) - nmin)

	# impute values lower than nmin
	if (nmin > 1) {
		lb = c(rep(lb[1], nmin - 1), lb)
		ub = c(rep(ub[1], nmin - 1), ub)
		v1 = mean(c(lb[1], ub[1]))

		lb[1] = v1
		ub[1] = v1
	}

	rm[1:nmax, 1] = lb
	rm[1:nmax, 2] = ub

	rm
}

range_div = function(n = 11, nmin = 3, nmax = 11, slope = 1/20) {
	nmax = min(nmax, n)

	ns = nmin:nmax

	# n x 2 matrix with 0 1 (full range)
	rm = matrix(c(0,1), nrow = n, ncol = 2, byrow = TRUE)

	# lb and ub between nmin and nmax
	lb = rep(0, length(ns))
	ub = 1 - slope * (rev(ns) - nmin)

	# impute values lower than nmin
	if (nmin > 1) {
		lb = c(rep(lb[1], nmin - 1), lb)
		ub = c(rep(ub[1], nmin - 1), ub)
		lb[1] = 0
		ub[1] = 0
	}

	rm[1:nmax, 1] = lb
	rm[1:nmax, 2] = ub

	rm
}
