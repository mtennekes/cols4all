plot_bitmap = function(x, add=FALSE) {
	res = dim(x)[2:1] # get the resolution, [x, y]
	if (!add) # initialize an empty plot area if add==FALSE
		plot(1,1,xlim=c(1,res[1]),ylim=c(1,res[2]),asp=1,type='n',xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
	rasterImage(x,1,1,res[1],res[2])
}


plus_rev = function(col1, col2) {
	x = png::readPNG("sandbox/Richard-Anuszkiewicz-_Plus-Reversed2_.png")

	id1 = (x[,,1] == x[1,1,1])

	m = col2rgb(c(col1, col2)) / 255



	x[id1] = rep(m[,1], each = sum(id1))
	x[!id1] = rep(m[,2], each = sum(!id1))
	plot_bitmap(x)
}

