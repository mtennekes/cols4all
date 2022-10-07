
library(sf)
library(grid)
nc <- st_read(system.file("shape/nc.shp", package="sf")) |> st_transform(crs = 2264)
nc = nc[, "NAME"]
nc$geometry =  st_sfc(lapply(nc$geometry, function(nci) {
	nci = st_multipolygon(lapply(nci, function(ncii) lapply(ncii, round)))
}))

bbx = tmaptools::bb(st_bbox(nc), -1.1)

cols = c("white", "black")
ind = as.integer(substr(nc$NAME, 1, 1) %in% LETTERS[1:13]) + 1

shp = sf::st_as_grob(nc$geometry, gp = gpar(fill = cols[ind], col = NA))


vars = load("R/sysdata.rda")


rgb_data = local({
	xrange = c(0.11, 0.68)
	yrange = c(0.05,0.62)
	res = c(201, 201)

	d = expand.grid(x = seq(xrange[1], xrange[2], length.out = res[1]),
					y = seq(yrange[1], yrange[2], length.out = res[2]),
					Y = seq(0, 100, by = 0.5))

	X = xyY2XYZ(d$x, d$y, Y = d$Y)
	Z = as(X, "RGB")

	coZ = coords(Z)

	cols_sel = !(is.na(rowSums(coZ)) |
				 	rowSums(is.nan(coZ)) > 0 |
				 	rowSums(coZ < 1e-6) == 3  |
				 	rowSums(coZ < 0) |
				 	rowSums(coZ > 1))

	df = as.data.frame(d[cols_sel,])
	df$hex = hex(Z[cols_sel])


	library(tidyverse)
	df_m = df |>
		group_by(x,y) |>
		arrange(desc(Y)) |>
		slice(1) |>
		ungroup() |>
		arrange(x, y)

	e = expand.grid(x = seq(xrange[1], xrange[2], length.out = res[1]),
					y = seq(yrange[1], yrange[2], length.out = res[2])) |>
		as.data.frame()

	df_e = e |>
		left_join(df_m) #|>
		#replace_na(list(hex = "#FFFFFF")) |>
		#mutate(hex = ifelse(hex == "#000000", "#FFFFFF", hex))

	toM = function(x, nr) {
		m = matrix(x, nrow = nr, byrow = TRUE)
		m[nr:1,]
	}


	cols = df_e$hex

	list(xrange = xrange,
					yrange = yrange,
					res = res,
					cols = cols)

})



save(.z, .s, .zbib, shp, bbx, rgb_data, file = "R/sysdata.rda", compress = "xz")






