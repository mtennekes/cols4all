library(colorspace)
library(grid)

xyY2XYZ = function(x, y, Y) {
	X = x * (Y / y)
	Z = (1-x-y) * (Y / y)
	XYZ(X, Y, Z)
}


# take 3

d = expand.grid(x = seq(0, 1, by = 0.005),
				y = seq(0, 1, by = 0.005),
				Y = seq(0, 100, by = 0.5))

cols = hex(xyY2XYZ(d$x, d$y, Y = d$Y))

cols_sel = !is.na(cols) & cols != "#000000"

df = as.data.frame(d[cols_sel,])
df$hex = cols[cols_sel]


library(tidyverse)
df_m = df |>
	group_by(x,y) |>
	arrange(desc(Y)) |>
	slice(1) |>
	ungroup() |>
	arrange(x, y)

e = expand.grid(x = seq(0, 1, by = 0.005),
				y = seq(0, 1, by = 0.005)) |>
	as.data.frame()

df_e = e |>
	left_join(df_m) |>
	replace_na(list(hex = "#FFFFFF")) |>
	mutate(hex = ifelse(hex == "#000000", "#FFFFFF", hex))


grid.newpage()

cols = matrix(df_e$hex, nrow = 201, byrow = TRUE)
cols = cols[201:1,]

grid.raster(cols)
