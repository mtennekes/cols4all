#######################################
# Palettes from Statistics Netherlands
#######################################
cbsnl_cols = list(
	charts = c("#00a1cd", "#0058b8", "#afcb05", "#53a31d", "#d9328a", "#7d4791",
			   "#f39200", "#c90c0f", "#0581a2", "#163a72", "#899d0c", "#488225",
			   "#af0e80", "#56217a", "#da5914", "#9c1006"),
	map_blue7 = c("#e1f4fd", "#c0e7ff", "#77cbe5", "#3d95d4", "#2256a0", "#143564",
				  "#09183c"),
	map_green7 = c("#f1f6de", "#edf0c7", "#c9de85", "#85bc22", "#348a3a", "#0f5f34",
				   "#114625"),
	map_red7 = c("#fedfc7", "#ffc597", "#f89e6b", "#e74d15", "#c01f26", "#82001e",
				 "#5b0708"),
	map_purple7 = c("#fbe2ed", "#f8c1d9", "#e38cbf", "#be3e8d", "#8b176f", "#490045",
					"#2d002c"),
	map_blue5 = c("#c0e7ff", "#77cbe5", "#3d95d4", "#2256a0", "#143564"),
	map_green5 = c("#edf0c7", "#c9de85", "#85bc22", "#348a3a", "#0f5f34"),
	map_red5 = c("#ffc597", "#f89e6b", "#e74d15", "#c01f26", "#82001e"),
	map_purple5 = c("#f8c1d9", "#e38cbf", "#be3e8d", "#8b176f", "#490045"),
	map_purple_green = c("#490045", "#be3e8d", "#f8c1d9", "#e5e5e5", "#edf0c7", "#85bc22",
						 "#0f5f34"),
	map_red_blue = c("#82001e", "#e74d15", "#ffc597", "#e5e5e5", "#c0e7ff", "#3d95d4",
					 "#143564")
)

cbsnl_types = c("cat", rep("seq", 8), "div", "div")
n = c(NA, 7, 7, 7, 7, 5, 5, 5, 5, 7, 7)

c4a_series_add(cbsnl_cols, xNA = "grey88", nmin = n, nmax = n, types = cbsnl_types, series = "cbsnl")

\dontrun{
c4a_gui(series = "cbsnl", n = 8)
}

# Palettes proposed by
# M.A. Petroff (2021), Accessible Color Sequences for Data Visualization,
# https://arxiv.org/pdf/2107.02270.pdf
rgb2hex = function(x) rgb(x[1], x[2], x[3], maxColorValue = 255)

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


	list(petroff6 = sapply(rgb6, rgb2hex),
		 petroff8 = sapply(rgb8, rgb2hex),
		 petroff10 = sapply(rgb10, rgb2hex))
})
petroff2 = list(petroff9 = petroff$petroff10[-9])

c4a_series_add_as_is(petroff, xNA = NA, types = "cat", series = "petroff")
c4a_series_add_as_is(petroff2, xNA = petroff$petroff10[9], types = "cat", series = "petroff")
