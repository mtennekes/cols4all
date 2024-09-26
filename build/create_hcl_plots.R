hs = seq(0, 360, by = .1)
lseq = seq(0, 100, by = .1)

library(pbapply)

res = pblapply(hs, function(h) {
	cs = sapply(lseq, function(l) {
		colorspace::max_chroma(h, l)
	})
	list(l = lseq[which.max(cs)],
		 c = max(cs))
})

ls = sapply(res, "[[", "l")
cs = sapply(res, "[[", "c")

df = data.frame(h = hs, c = cs, l = ls)

df$hex = hcl(df$h, df$c, df$l)


maxc_plot = function() {
	oldp = par(mfrow = c(2,1),
		#mai = rep(1,4),
		mai = c(0.65,0.65,0.1,0.1),
		oma = c(0, 0, 0, 0))

	plot(df$h, df$c, col = "#000000", pch = 19, cex = 2.3, xaxt = "n", xlab = "", ylab = "")
	grid(lty = 1, col = "gray60", lwd = 0.5)
	axis(1, at = seq(0,360, by = 60))
	points(df$h, df$c, col = df$hex, pch = 19, cex = 2)
	title("", xlab = "Hue", ylab = "Chroma", line = 2)

	plot(df$h, df$l, col = "#000000", pch = 19, cex = 2.3, xaxt = "n", xlab = "", ylab = "")
	grid(lty = 1, col = "gray60", lwd = 0.5)
	axis(1, at = seq(0,360, by = 60))
	points(df$h, df$l, col = df$hex, pch = 19, cex = 2)
	title("", xlab = "Hue", ylab = "Luminance", line = 2)
	par(oldp)
}


width = 300
asp = 2/3
res = width / 4

png("inst/img/max_chromax1.png", width = width, height = width / asp, res = res)
maxc_plot()
dev.off()

png("inst/img/max_chromax2.png", width = width * 2, height = width / asp * 2, res = res * 2)
maxc_plot()
dev.off()


x = png::readPNG("inst/img/hcl_spacex2.png")
