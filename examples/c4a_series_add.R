# Palettes from Statistics Netherlands
cbsnl_cols = list(
	cool = c("#00a1cd", "#0058b8", "#afcb05", "#53a31d",
		"#f39200", "#af0e80", "#ffcc00", "#e94c0a"),
	warm = c("#e94c0a", "#ffcc00", "#af0e80", "#f39200",
		"#53a31d", "#afcb05", "#0058b8", "#00a1cd"),
	blues = c("#c0e7ff", "#77cbe5", "#3d95d4", "#2256a0", "#143564"),
	reds =  c("#ffc597", "#f89e6b", "#e74d15", "#c01f26", "#82001e"),
	greens = c("#edf0c7", "#c9de85", "#85bc22", "#348a3a", "#0f5f34"),
	purples = c("#f8c1d9", "#e38cbf", "#be3e8d", "#8b176f", "#460042")
)

cbsnl_types = c("cat", "cat", "seq", "seq", "seq", "seq")

c4a_series_add(cbsnl_cols, xNA = "grey88", types = cbsnl_types, series = "cbsnl")

\dontrun{
c4a_gui(series = "cbsnl", n = 8)
}

# Palettes proposed by
# M.A. Petroff (2021), Accessible Color Sequences for Data Visualization,
# https://arxiv.org/pdf/2107.02270.pdf
petroff = local({
	rgb6 = list(c(87,144,252),
		 c(248,156,32),
		 c(228,37,54),
		 c(150,74,139),
		 c(156,156,161),
		 c(122,33,221))

	rgb8 = list(c(24,69,251),
		 c(255,94,2),
		 c(201,31,22),
		 c(200,73,169),
		 c(173,173,125),
		 c(134,200,221),
		 c(87,141,255),
		 c(101,99,100))

	rgb10 = list(c(63,144,218),
		 c(255,169,14),
		 c(189,31,1),
		 c(148,164,162),
		 c(131,45,182),
		 c(169,107,89),
		 c(231,99,0),
		 c(185,172,112),
		 c(113,117,129),
		 c(146,218,221))

	rgb2hex = function(x) rgb(x[1], x[2], x[3], maxColorValue = 255)

	list(petroff6 = sapply(rgb6, rgb2hex),
		 petroff8 = sapply(rgb8, rgb2hex),
		 petroff10 = sapply(rgb10, rgb2hex))
})

c4a_series_add(petroff, xNA = NA, types = "cat", series = "petroff")

