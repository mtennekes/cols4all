library(grid)
#install.packages("extrafont")

# library(sysfonts)
# download.file("http://simonsoftware.se/other/xkcd.ttf", dest="xkcd.ttf", mode="wb")
# font_add("xkcd", "~/xkcd.ttf")




grid.t = function(text, x, y) {
	nlines = length(strsplit(text, "\n", fixed = TRUE)[[1]])
	grid.roundrect(x = x, y = y, width = convertWidth(stringWidth(text), unitTo = "npc") + unit(3, "lines"), height = unit(nlines+max(0,(nlines-1)*0.5),"lines"), r = unit(0.5, "npc"), gp=gpar(col=NA, fill = "white"))
	grid.text(text, x = x, y = y, gp = gpar(cex = 1.2, fontfamily = "xkcd", fontface = "bold"))

}


unlink("temp", recursive = T, force = T)
dir.create("temp")
for (i in 1:4) {
	png(paste0("temp/hue_lines", i, ".png"), width = 375, height = 375, bg = "transparent")
	#c4a_plot_confusion_lines(cols = c4a("brewer.set1", 7))
	if (i >= 1) {
		grid.lines(x = c(0.12, 0.25), y = c(0.63, 0.55)+0.1, arrow = arrow(length = unit(0.02, "npc")), gp = gpar(lwd = 2))
		grid.t("Color space", 0.1, 0.65+0.1)
		grid.polyline(x = c(0.08, 0.34, 0.93, 0.08), y = c(0.03, 0.96, 0.49, 0.03), gp = gpar(lwd = 2))
	}
	if (i >= 2) {
		grid.t("Colors along each line\nhave the same hue", 0.75, 0.85)
		grid.lines(x = c(0.56, 0.355), y = c(0.88, 0.49), gp = gpar(lwd = 3, lty = "dotted"))
	}
	if (i >= 3) {
		grid.t("1, 2, 3, ... Palette colors\n-They may appear brighter-", 0.7, 0.15)
	}
	if (i >= 4) {
		grid.t("See below how coor blind people perceive these colors!", x = 0.5, y = 0.03)
	}
	dev.off()
	#x = png::readPNG(paste0("temp/hue_lines", i, ".png"))
	#x[,,4][x[,,1] == 1 & x[,,2] == 1 & x[,,3] == 1] = 0.4
	#png::writePNG(x, paste0("temp/hue_lines", i, ".png"))
}
library(gifski)
gifski::gifski(png_files = list.files(path = "temp", full.names = TRUE), width = 375, height = 375, delay = 1.5, gif_file = "inst/img/hue_lines.gif", loop = FALSE)


unlink("temp", recursive = T, force = T)
dir.create("temp")
for (i in 1:4) {
	png(paste0("temp/conf_lines", i, ".png"), width = 375, height = 375, bg = "transparent")
	c4a_plot_confusion_lines(cols = c4a("brewer.set1", 7), cvd = "deutan")
	if (i >= 1) {
		#grid.lines(x = c(0.12, 0.25), y = c(0.63, 0.55)+0.1, arrow = arrow(length = unit(0.02, "npc")), gp = gpar(lwd = 2))
		grid.polyline(x = c(0.08, 0.34, 0.93, 0.08), y = c(0.03, 0.96, 0.49, 0.03), gp = gpar(lwd = 2))
		grid.t("The same color space,\nbut perceived by color\nblind people - deutans", 0.2, 0.65+0.1)
	}
	if (i >= 2) {
		grid.t("Colors along each line\nmay be hard to distinguish", 0.55, 0.3)
		grid.lines(x = c(0.7, 0.16), y = c(0.368, 0.385), gp = gpar(lwd = 3, lty = "dotted"))
	}
	dev.off()
	#x = png::readPNG(paste0("temp/hue_lines", i, ".png"))
	#x[,,4][x[,,1] == 1 & x[,,2] == 1 & x[,,3] == 1] = 0.4
	#png::writePNG(x, paste0("temp/hue_lines", i, ".png"))
}
library(gifski)
gifski::gifski(png_files = list.files(path = "temp", full.names = TRUE), width = 375, height = 375, delay = 1.5, gif_file = "inst/img/conf_lines.gif", loop = FALSE)

