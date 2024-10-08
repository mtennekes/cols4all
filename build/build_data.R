
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

shp_c = st_coordinates(st_centroid(nc))

shp = sf::st_as_grob(nc$geometry, gp = gpar(fill = cols[ind], col = NA))


vars = load("R/sysdata.rda")

Ls = seq(10,100,by=1)
LCH = colorspace::polarLUV(Ls, 50, 50)
Ys = colorspace::coords(as(LCH, "XYZ"))[,2]

rgb_data = local({
	library(colorspace)
	xrange = c(0.11, 0.68)
	yrange = c(0.05,0.62)
	res = c(201, 201)

	d = expand.grid(x = seq(xrange[1], xrange[2], length.out = res[1]),
					y = seq(yrange[1], yrange[2], length.out = res[2]),
					Y = Ys)

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

	toM = function(x, nr) {
		m = matrix(x, nrow = nr, byrow = TRUE)
		m[nr:1,]
	}


	cols_list = lapply(Ys, function(Y2) {
		if (Y2 == 100) {
			df_m = df |>
				group_by(x,y) |>
				arrange(desc(Y)) |>
				slice(1) |>
				ungroup() |>
				arrange(x, y)
		} else {
			df_m = df |>
				filter(Y == Y2) |>
				arrange(x, y)
		}
		e = expand.grid(x = seq(xrange[1], xrange[2], length.out = res[1]),
						y = seq(yrange[1], yrange[2], length.out = res[2])) |>
			as.data.frame()
		df_e = e |>
			left_join(df_m) #|>
		df_e$hex
	})
	names(cols_list) = paste0("L", Ls)

	list(xrange = xrange,
		 yrange = yrange,
		 res = res,
		 cols_list = cols_list)

})

# keep only step 10 luminance
rgb_data$cols_list = rgb_data$cols_list[paste0("L", seq(10, 100, by = 10))]

rdata = list(
	scatter.x = stats::rnorm(100, mean = 0, sd = .5),
	scatter.y = stats::rnorm(100, mean = 0, sd = .5),
	bars.x = stats::rnorm(5, mean = 40, sd = 10),
	necklace.h = stats::runif(5000, min = 0, max = 360),
	necklace.c = stats::runif(5000, min = .3, max = 1),
	necklace.l = stats::runif(5000, min = 30, max = 90),
	necklace.d = stats::runif(5000)
)



rdata$name_data = create_name_data()

rdata = c(rdata, local({
	set.seed(13)
	x = 1:100

	k=36

	s = lapply(1:k, function(i) {
		cumsum(rnorm(length(x)))
	})

	# scale per 2 lines
	for (i in seq(1,k,by=2)) {
		ids = c(i,i+1)
		r = range(c(s[[ids[1]]], s[[ids[2]]]))
		s[ids] = lapply(s[ids], function(si) {
			si = si - r[1]
			si / diff(r)
		})
	}

	list(lines.x = x,
		 lines.s = s)
}), local({
	require(colorspace)
	require(volcano3D) # for polar grid

	add_coords2= function(df, width = 180) {
		df = within(df, {
			x = sin(H / 180 * pi) * (C / 180) * width
			y = cos(H / 180 * pi) * (C / 180) * width
		})
	}

	hdf_all = expand.grid(H = seq(0, 360, by = 1),
						  L = seq(0, 100, by = 0.5),
						  Crel = seq(0, 1, by = 0.05))
	hdf_all$Cmax = colorspace::max_chroma(h = hdf_all$H, l = hdf_all$L)
	hdf_all$C = hdf_all$Crel * hdf_all$Cmax
	hdf_all$hex = hcl(hdf_all$H, hdf_all$C, hdf_all$L)

	hdf_all = hdf_all[!duplicated(hdf_all$hex), ]

	hdf_all$prob = ifelse(hdf_all$C == hdf_all$Cmax, 0.8, 0.01)


	rgb_extremes = c("#FF0000", "#00FF00", "#0000FF")
	hcl_extremes = as.data.frame((hex2RGB(rgb_extremes) |> as("polarLUV"))@coords)[,3:1]
	hcl_extremes2 = data.frame(H = hcl_extremes$H,
							   L = hcl_extremes$L,
							   Crel = 1,
							   Cmax = hcl_extremes$C,
							   C = hcl_extremes$C,
							   hex = rgb_extremes,
							   prob = 1e15)
	hdf_all = rbind(hcl_extremes2, hdf_all)


	hdf_all = add_coords2(hdf_all)
	hdf_all$text = paste0(hdf_all$hex, "\n", "H ", round(hdf_all$H), ", C ", round(hdf_all$C), ", L ", round(hdf_all$L))


	hdf = hdf_all[sample(nrow(hdf_all),10000, prob = hdf_all$prob),]
	hdf_pg = polar_grid(r_axis_ticks = c(0, 45, 90, 135, 180),#seq(0, 180, by = 45),
						z_axis_ticks = seq(0, 100, by = 20))


	list(hdf = hdf,
		 hdf_pg = hdf_pg )

}))




save(.z, .s, .zbib, .zdes, shp, shp_c, bbx, rgb_data, rdata, names_NL_model, names_NL_colors, file = "R/sysdata.rda", compress = "xz")






