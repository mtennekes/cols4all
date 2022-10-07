c4a_plot_Plus_Reversed = function(col1 = "blue", col2 = "red", borders = "black", lwd = 0, orientation = c("portrait", "landscape")) {
	orientation = match.arg(orientation)
	#library(terra)
	#library(sf)
	#library(stars)
	x = png::readPNG("inst/img/Richard-Anuszkiewicz-_Plus-Reversed.png")

	if (lwd > 0) {
		y = round(png::readPNG("inst/img/Richard-Anuszkiewicz-_Plus-Reversed_borders.png"), 1)
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


	grid::grid.raster(r)
}
