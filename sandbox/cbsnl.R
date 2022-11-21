#######################################
# Palettes from Statistics Netherlands
#######################################

cbsnl_cols = list(
	charts = c("#00a1cd", "#0058b8", "#afcb05", "#53a31d", "#d9328a", "#7d4791",
			   "#f39200", "#c90c0f", "#0581a2", "#163a72", "#899d0c", "#488225",
			   "#af0e80", "#56217a", "#da5914", "#9c1006"),
	map_blue = structure(c("#e1f4fd", "#c0e7ff", "#77cbe5", "#3d95d4", "#2256a0", "#143564",
				  "#09183c"), index = list('5' = 2:6, '6' = 2:7, '7' = 1:7)),
	map_green = structure(c("#f1f6de", "#edf0c7", "#c9de85", "#85bc22", "#348a3a", "#0f5f34",
				   "#114625"), index = list('5' = 2:6, '6' = 2:7, '7' = 1:7)),
	map_red = structure(c("#fedfc7", "#ffc597", "#f89e6b", "#e74d15", "#c01f26", "#82001e",
				 "#5b0708"), index = list('5' = 2:6, '6' = 2:7, '7' = 1:7)),
	map_purple = structure(c("#fbe2ed", "#f8c1d9", "#e38cbf", "#be3e8d", "#8b176f", "#490045",
					"#2d002c"), index = list('5' = 2:6, '6' = 2:7, '7' = 1:7)),
	map_purple_green = c("#490045", "#be3e8d", "#f8c1d9", "#e5e5e5", "#edf0c7", "#85bc22",
						 "#0f5f34"),
	map_red_blue = c("#82001e", "#e74d15", "#ffc597", "#e5e5e5", "#c0e7ff", "#3d95d4",
					 "#143564")
)

cbsnl_types = c("cat", rep("seq", 4), "div", "div")
nmin = c(NA, 5, 5, 5, 5, 7, 7)
nmax = c(NA, 7, 7, 7, 7, 7, 7)

cbsnl_bib = utils::bibentry(bibtype = "Misc",
					  author = person("Statistics Netherlands"),
					  title = "CBS - Statistics Netherlands",
					  url = "https://www.cbs.nl/en-gb",
					  year = 2022)


dat = c4a_data(cbsnl_cols, xNA = "grey88",
			   nmin = nmin, nmax = nmax, ndef = nmin,
			   types = cbsnl_types, series = "cbsnl", bib = cbsnl_bib)
saveRDS(dat, file = "sandbox/cbsnl.rds")
c4a_load(dat)

\dontrun{
c4a_gui(series = "cbsnl")
}
