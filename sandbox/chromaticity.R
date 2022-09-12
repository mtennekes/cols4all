library(colorspace)
library(grid)

colorspace::XYZ()

x = expand.grid(x = seq(0, 1, length.out = 100),
				y = seq(0, 1, length.out = 100))


xyY2XYZ = function(x, y, Y) {
	X = x * (Y / y)
	Z = (1-x-y) * (Y / y)
	XYZ(X, Y, Z)
}

x$hex = hex(xyY2XYZ(x$x, x$y, 0.9))

grid.newpage()

m = matrix(x$hex, nrow = 100)

grid.raster(m)

# Generate a fake dataset
set.seed(20190320)
coldat <- as.data.frame(matrix(runif(n = 30, min = 0.15, max = 0.5), nrow = 10, ncol = 3))

# Make sure this dataset works with the cieplot() function
attr(coldat, "clrsp") <- "CIEXYZ"
colnames(coldat) <- c("x", "y", "z")
library(pavo)
cieplot(coldat)

?RGB

## ART (Tweet 2022-09-12)
d = expand.grid(R = seq(0, 1, length.out = 100),
				G = seq(0, 1, length.out = 100),
				B = seq(0, 1, length.out = 100))


e = expand.grid(X = seq(0, 95.047, length.out = 100),
				Y = seq(0, 100, length.out = 100),
				Z = seq(0, 108.883, length.out = 100))

cls = with(d, sRGB(R=R, G=G, B=B))

CLS = as(cls, "XYZ")


CLS2 = with(e, XYZ(X=X,Y=Y,Z=Z))

hex = colorspace::hex(CLS)

hex2 = colorspace::hex(CLS2)

co = colorspace::coords(CLS2)


x = co[,1] / rowSums(co)
y = co[,2] / rowSums(co)

x[is.nan(x)] = NA
y[is.nan(y)] = NA

xr = round(x, digits = 3) * 1000
yr = round(y, digits = 3) * 1000

m = matrix(NA, nrow = 1001, ncol = 1001)
for (i in 1:length(xr)) {
	m[1001-yr[i], xr[i] + 1] = hex[i]
}

grid.raster(m)


## Take 2
offset = 0
e = expand.grid(X = seq(0, 95.047 + offset, length.out = 300),
				Y = seq(0, 100 + offset, length.out = 300),
				Z = seq(0, 108.883 + offset, length.out = 300))

CLS = with(e, XYZ(X=X,Y=Y,Z=Z))

hex = colorspace::hex(CLS, fixup = TRUE)

co = colorspace::coords(CLS)


x = co[,1] / rowSums(co)
y = co[,2] / rowSums(co)

x[is.nan(x)] = NA
y[is.nan(y)] = NA

xr = round(x, digits = 3) * 1000
yr = round(y, digits = 3) * 1000

m = matrix(NA, nrow = 1001, ncol = 1001)
for (i in 1:length(xr)) {
	if (!is.na(hex[i])) m[1001-yr[i], xr[i] + 1] = hex[i]
}
grid.newpage()
grid.raster(m)

# take 3

d = expand.grid(x = seq(0, 1, by = 0.01),
				y = seq(0, 1, by = 0.01),
				Y = seq(0, 100, by = 0.5))

cols = hex(xyY2XYZ(d$x, d$y, Y = d$Y))

df = as.data.frame(d[!is.na(cols),])
df$hex = cols[!is.na(cols)]

library(tidyverse)
df_m = df |>
	group_by(x,y) |>
	arrange(desc(Y)) |>
	slice(1) |>
	ungroup() |>
	arrange(x, y)

e = expand.grid(x = seq(0, 1, by = 0.01),
				y = seq(0, 1, by = 0.01)) |>
	as.data.frame()

df_e = e |>
	left_join(df_m)

grid.newpage()

cols = matrix(df_e$hex, nrow = 101)

grid.raster(cols)


str(xr)

s = sample.int(length(xr), 10000)

plot(x[s], y[s], pch=16)

s = sample.int(nrow(co), size = 1000)
plot(co[s,2], y[s], pch=16)

