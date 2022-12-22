library(grid)
library(gifski)
#install.packages("extrafont")

# library(sysfonts)
# download.file("http://simonsoftware.se/other/xkcd.ttf", dest="xkcd.ttf", mode="wb")
# font_add("xkcd", "~/xkcd.ttf")

#options(shiny.launch.browser = .rs.invokeShinyWindowExternal)


grid.t = function(text, x, y, scale = 1, bg = TRUE) {
	nlines = length(strsplit(text, "\n", fixed = TRUE)[[1]])
	if (bg) grid.roundrect(x = x, y = y, width = scale * (convertWidth(stringWidth(text), unitTo = "npc") + unit(3, "lines")), height = scale * (unit(nlines+max(0,(nlines-1)*0.5),"lines")), r = unit(0.5, "npc"), gp=gpar(col=NA, fill = "white"))
	grid.text(text, x = x, y = y, gp = gpar(cex = 1.2 * scale, fontfamily = "xkcd", fontface = "bold"))

}

################################
#### HUE lines #################
################################
for (scale in 1:2) {
	unlink("temp", recursive = T, force = T)
	dir.create("temp")
	for (i in 1:4) {
		png(paste0("temp/hue_lines", i, ".png"), width = 375*scale, height = 375*scale, bg = "transparent")
		#c4a_plot_confusion_lines(cols = c4a("brewer.set1", 7))
		if (i >= 1) {
			grid.lines(x = c(0.12, 0.25), y = c(0.63, 0.55)+0.1, arrow = arrow(length = unit(0.02, "npc")), gp = gpar(lwd = 2 * scale))
			grid.t("Color space", 0.1, 0.65+0.1, scale)
			grid.polyline(x = c(0.08, 0.34, 0.93, 0.08), y = c(0.03, 0.96, 0.49, 0.03), gp = gpar(lwd = 2 * scale))
		}
		if (i >= 2) {
			grid.t("Colors along each line\nhave the same hue", 0.75, 0.85, scale)
			grid.lines(x = c(0.56, 0.355), y = c(0.88, 0.49), gp = gpar(lwd = 3 * scale, lty = "dotted"))
		}
		if (i >= 3) {
			grid.t("1, 2, 3, ... Palette colors\n-They may appear brighter-", 0.7, 0.15, scale)
		}
		if (i >= 4) {
			grid.t("See below how color blind people perceive this !", x = 0.5, y = 0.03, scale)
		}
		dev.off()
	}
	gifski::gifski(png_files = list.files(path = "temp", full.names = TRUE), width = 375*scale, height = 375*scale, delay = 1.5, gif_file = paste0("inst/img/hue_lines", scale, "x.gif"), loop = FALSE)

}

################################
#### conf lines #################
################################

coords = list(D = list(x = c(0.7, 0.16), y = c(0.368, 0.385)),
			  P = list(x = c(0.25, 0.65), y = c(0.63, 0.35)),
			  T = list(x = c(0.18, 0.47), y = c(0.09, 0.86)))

for (scale in 1:2) {
	for (X in c("D", "P", "T")) {
		unlink("temp", recursive = T, force = T)
		dir.create("temp")
		for (i in 1:4) {
			png(paste0("temp/conf_lines", X, i, ".png"), width = 375 * scale, height = 375 * scale, bg = "transparent")
			#c4a_plot_confusion_lines(cols = c4a("brewer.set1", 7), cvd = switch(X, D="deutan", P="protan", T="tritan"))
			if (i >= 1) {
				#grid.lines(x = c(0.12, 0.25), y = c(0.63, 0.55)+0.1, arrow = arrow(length = unit(0.02, "npc")), gp = gpar(lwd = 2))
				grid.polyline(x = c(0.08, 0.34, 0.93, 0.08), y = c(0.03, 0.96, 0.49, 0.03), gp = gpar(lwd = 2 * scale))
				grid.t(paste0("The same color space,\nbut perceived by color\nblind people - ", switch(X, D="deutans", P="protans", T="tritans")), 0.2, 0.65+0.1, scale)
			}
			if (i >= 2) {
				grid.t("Colors along each line\nmay be hard to distinguish", 0.55, 0.3, scale)
				grid.lines(coords[[X]]$x, coords[[X]]$y, gp = gpar(lwd = 4 * scale, lty = "dotted"))
			}
			dev.off()
		}
		gifski::gifski(png_files = list.files(path = "temp", full.names = TRUE), width = 375 * scale, height = 375 * scale, delay = 1.5, gif_file = paste0("inst/img/conf_lines", X, scale, "x.gif"), loop = FALSE)
	}
}


################################
#### dist matrix #################
################################

for (scale in 1:2) {
	unlink("temp", recursive = T, force = T)
	dir.create("temp")
	for (i in 1:6) {
		png(paste0("temp/dist", i, ".png"), width = 500 * scale, height = 375 * scale, bg = "transparent")
		#c4a_plot_dist_matrix(c4a("brewer.accent", 7))
		if (i >= 1) {
			grid.t("Color 1", x = 0.05, y = 0.92, scale)
			grid.t("Color 2", x = 0.1, y = 0.97, scale)
			grid.lines(x = c(0.05, 0.05), y = c(0.9, 0.82), arrow = arrow(length = unit(0.015, "npc")), gp = gpar(lwd = 2 * scale))
			grid.lines(x = c(0.15, 0.2), y = c(0.97, 0.97), arrow = arrow(length = unit(0.015, "npc")), gp = gpar(lwd = 2 * scale))
		}
		if (i >= 2) {
			grid.t("How similiar are these 2 colors?\n- click to select other colors-", 0.7, 0.55, scale)
			grid.circle(x = 0.45, y = 0.55, r = 0.06, gp = gpar(fill = "#FFFFFF", lwd = 3 * scale))
			grid.t("?", 0.45, 0.55, scale * 1.5, bg = FALSE)
			grid.lines(x = c(0.16, 0.39), y = c(0.55, 0.55), gp = gpar(lwd = 3 * scale))
			grid.lines(x = c(0.45, 0.45), y = c(0.64, 0.82), gp = gpar(lwd = 3 * scale))
			#grid.lines(x = c(0.56, 0.355), y = c(0.88, 0.49), gp = gpar(lwd = 3, lty = "dotted"))
		}

		if (i >= 3) {
			grid.t("Symbol = similar", 0.7, 0.4, scale)
			grid.curve(x1 = 0.8, x2 = 0.94, y1 = 0.4, y2 = 0.7, gp = gpar(lwd = 3 * scale), arrow = arrow(length = unit(0.02, "npc")))
		}
		if (i >= 4) {
			grid.t("No symbol = not similar,\ntherefore each to distinguish", 0.7, 0.3, scale)
		}
		if (i >= 5) {
			grid.t("At least, for people with normal color vision !", 0.55, 0.14, scale)
		}
		if (i >= 6) {
			grid.t("See below how 'easy' this is for color blind people...", 0.6, 0.08, scale)
		}



		dev.off()
	}
	gifski::gifski(png_files = list.files(path = "temp", full.names = TRUE), width = 500 * scale, height = 375 * scale, delay = 1.5, gif_file = paste0("inst/img/simi", scale, "x.gif"), loop = FALSE)

}


################################
#### perc dist matrices ########
################################

for (X in c("D", "P", "T")) {
	unlink("temp", recursive = T, force = T)
	dir.create("temp")
	for (i in 1:4) {
		png(paste0("temp/conf_lines", X, i, ".png"), width = 375, height = 375, bg = "transparent")
		#c4a_plot_confusion_lines(cols = c4a("brewer.set1", 7), cvd = switch(X, D="deutan", P="protan", T="tritan"))
		if (i >= 1) {
			#grid.lines(x = c(0.12, 0.25), y = c(0.63, 0.55)+0.1, arrow = arrow(length = unit(0.02, "npc")), gp = gpar(lwd = 2))
			grid.polyline(x = c(0.08, 0.34, 0.93, 0.08), y = c(0.03, 0.96, 0.49, 0.03), gp = gpar(lwd = 2))
			grid.t(paste0("The same color space,\nbut perceived by color\nblind people - ", switch(X, D="deutans", P="protans", T="tritans")), 0.2, 0.65+0.1)
		}
		if (i >= 2) {
			grid.t("Colors along each line\nmay be hard to distinguish", 0.55, 0.3)
			grid.lines(coords[[X]]$x, coords[[X]]$y, gp = gpar(lwd = 4, lty = "dotted"))
		}
		dev.off()
	}
	gifski::gifski(png_files = list.files(path = "temp", full.names = TRUE), width = 375, height = 375, delay = 1.5, gif_file = paste0("inst/img/conf_lines", X, ".gif"), loop = FALSE)
}



