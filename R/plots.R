# plot_bitmap = function(x, add=FALSE) {
# 	res = dim(x)[2:1] # get the resolution, [x, y]
# 	if (!add) # initialize an empty plot area if add==FALSE
# 		#par(plt = c(0, 1, 0, 1))
# 		plot(50,50,xlim=c(1,res[1]),ylim=c(1,res[2]),asp=1, xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
# 	rasterImage(x,1,1,res[1],res[2])
# }


c4a_example_Plus_Reversed = function(col1 = "blue", col2 = "red", borders = "black", lwd = 0, orientation = c("portrait", "landscape")) {
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

c4a_example_bars = function(col1 = "blue", col2 = "red", borders = "black", lwd = 0) {
	set.seed(1234)
	if (lwd == 0) borders = NA
	x = rnorm(5, 40, 10)
	m = matrix(c(x, 100-x), nrow = 2, byrow = TRUE, dimnames = list(c("v1", "v2"), LETTERS[1:5]))
	barplot(m, col = c(col1, col2), border = borders, xlab = "Group", ylab = "Percentage", space = 0.2)

	for (i in 1:5) {
		cx = (i - 1) + i * 1/5
		lines(c(cx, cx + 1), c(m[1, i], m[1, i]), lwd = lwd, col = borders, lend = "butt")
	}

}


c4a_example_map = function(col1 = "blue", col2 = "red", borders = "black", lwd = 0, crop = FALSE) {
	shp$gp$fill = ifelse(shp$gp$fill == "white", col1, col2)

	if (crop) {
		xrange = bbx[3] - bbx[1]
		bbx[1] = bbx[1] + xrange * 0.6
		bbx[3] = bbx[3] - xrange * 0.2
	}

	if (lwd == 0) borders = NA
	if (is.na(borders)) {
		shp$gp$col = shp$gp$fill
	} else {
		shp$gp$col = borders
		shp$gp$lwd = lwd
	}

	sasp = (bbx[3] - bbx[1]) / (bbx[4] - bbx[2])
	dasp = dev.size()[1] / dev.size()[2]

	cy = (bbx[4] + bbx[2]) / 2
	cx = (bbx[3] + bbx[1]) / 2

	if (sasp > dasp) {
		h = (bbx[4] - bbx[2]) * (sasp / dasp)
		bbx[2] = cy - h/2
		bbx[4] = cy + h/2
	} else {
		w = (bbx[3] - bbx[1]) * (dasp / sasp)
		bbx[1] = cx - w/2
		bbx[3] = cx + w/2
	}

	grid::grid.newpage()

	# if (dasp > 1) {
	# 	pushViewport(viewport(width = unit(1, "snpc"), height = unit(1/dasp, "snpc")))
	# } else {
	# 	pushViewport(viewport(width = unit(dasp, "snpc"), height = unit(1, "snpc")))
	# }

	grid::pushViewport(grid::viewport(xscale = bbx[c(1, 3)], yscale = bbx[c(2, 4)]))


	grid::grid.draw(shp)
}

