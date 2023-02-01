c4a_plot_Plus_Reversed = function(col1 = "#A93028", col2 = "#1F6758", borders = "black", lwd = 0, orientation = c("portrait", "landscape")) {
	orientation = match.arg(orientation)
	#library(terra)
	#library(sf)
	#library(stars)
	x = png::readPNG(system.file("img/plus_rev.png", package = "cols4all"))

	if (lwd > 0) {
		y = round(png::readPNG(system.file("img/plus_rev_borders.png", package = "cols4all")), 1)
	}


	if (orientation == "landscape") {
		x = aperm(x, perm = c(2, 1, 3))
		if (lwd > 0) y = aperm(y, perm = c(2, 1))
	}

	id1 = (x[,,1] == x[1,1,1])

	m = col2rgb(c(col1, col2)) / 255

	x[id1] = rep(m[,1], each = sum(id1))
	x[!id1] = rep(m[,2], each = sum(!id1))

	if (lwd > 0) x[y[,] <= c(0.4, 0.5, 0.8)[lwd]] = ifelse(borders == "black", 0, 1)

	r = grDevices::as.raster(x)

	grb = grid::rasterGrob(r)
	grid::grid.draw(grb)
}

#
# x = png::readPNG("inst/img/floating_rings.png")
#
# x2 = x[seq(1,1020,by=2), seq(1,1020,by=2),]
#
# x = x[,,1:3]
# x[x>0 & x < 0.01] = 0
#
#
# dim(x2)
#
# x3 = array(0, dim = c(550, 550, 3))
#
# x3[21:530, 21:530, ] = x2[,,1:3]
#
# png::writePNG(x3, "inst/img/floating_rings3.png")


c4a_plot_floating_rings = function(col1 = "red", col2 = "blue", borders = "black", dark = TRUE) {
	x = png::readPNG(system.file("img/floating_rings.png", package = "cols4all"))

	id1 = (x[,,1] == 1)
	id2 = (x[,,3] == 1)

	m = col2rgb(c(col1, col2)) / 255

	x[id1] = rep(m[,1], each = sum(id1))
	x[id2] = rep(m[,2], each = sum(id2))

	if (!dark) x[!id1 & !id2] = 1

	r = grDevices::as.raster(x)

	grb = grid::rasterGrob(r)
	grid::grid.draw(grb)
}
