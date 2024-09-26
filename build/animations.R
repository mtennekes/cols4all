library(grid)
library(gifski)
#install.packages("extrafont")

# library(sysfonts)
# download.file("http://simonsoftware.se/other/xkcd.ttf", dest="xkcd.ttf", mode="wb")
# font_add("xkcd", "~/xkcd.ttf")

options(shiny.launch.browser = .rs.invokeShinyWindowExternal)


grid.t = function(text, x, y, scale = 1, bg = TRUE, r = 0.5, just = "center") {
	nlines = length(strsplit(text, "\n", fixed = TRUE)[[1]])
	if (bg) grid.roundrect(x = x, y = y, just = just, width = scale * (convertWidth(stringWidth(text), unitTo = "npc") + unit(3, "lines")), height = scale * (unit(nlines+max(0,(nlines-1)*0.5),"lines")), r = unit(r, "npc"), gp=gpar(col=NA, fill = "white"))
	grid.text(text, x = x, y = y, just = just, gp = gpar(cex = 1.2 * scale, fontfamily = "xkcd", fontface = "bold"))
}


################################
#### Simu ######################
################################
for (scale in 1:2) {
	unlink("temp", recursive = T, force = T)
	dir.create("temp")
	for (i in 1:3) {
		png(paste0("temp/simu", i, ".png"), width = 800*scale, height = 150*scale, bg = "transparent")
		#c4a_plot_cvd(cols = c4a("brewer.set1", 7))
		if (i >= 1) {
			grid.lines(x = c(0.35, 0.75), y = c(0.75, 0.75), arrow = arrow(length = unit(0.1, "npc"), ends = "both"), gp = gpar(lwd = 2 * scale))
			grid.t("Palette colors", 0.55, 0.75, scale)
		}
		if (i >= 2) {
			grid.lines(x = c(0.07, 0.07), y = c(0.08, 0.61), gp = gpar(lwd = 3 * scale))
			grid.lines(x = c(0.07, 0.1), y = c(0.33, 0.33), gp = gpar(lwd = 3 * scale))
			grid.t("Types of color blindness", 0.2, 0.33, scale)
		}
		if (i >= 3) {
			grid.lines(x = c(0.35, 0.75), y = c(0.33, 0.33), arrow = arrow(length = unit(0.1, "npc"), ends = "both"), gp = gpar(lwd = 2 * scale))
			grid.t("Perceived by color blind people !", 0.55, 0.33, scale)
		}
		dev.off()
	}
	gifski::gifski(png_files = list.files(path = "temp", full.names = TRUE), width = 800*scale, height = 150*scale, delay = 1.5, gif_file = paste0("inst/img/simu", scale, "x.gif"), loop = FALSE)

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
			grid.t("No symbol = not similar,\ntherefore easy to distinguish", 0.7, 0.3, scale)
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

for (scale in 1:2) {
	for (X in c("D", "P", "T")) {
		unlink("temp", recursive = T, force = T)
		dir.create("temp")
		for (i in 1:4) {
			png(paste0("temp/dist", i, ".png"), width = 500 * scale, height = 375 * scale, bg = "transparent")
			#c4a_plot_dist_matrix(c4a("brewer.accent", 7))
			if (i >= 1) {
				grid.t("Color 1", x = 0.05, y = 0.92, scale)
				grid.t("Color 2", x = 0.1, y = 0.97, scale)
				grid.lines(x = c(0.05, 0.05), y = c(0.9, 0.82), arrow = arrow(length = unit(0.015, "npc")), gp = gpar(lwd = 2 * scale))
				grid.lines(x = c(0.15, 0.2), y = c(0.97, 0.97), arrow = arrow(length = unit(0.015, "npc")), gp = gpar(lwd = 2 * scale))
			}
			if (i >= 2) {
				grid.t(paste0("How similiar are these 2 colors\n accoriding to\ncolor blind people - ", switch(X, D="deutans", P="protans", T="tritans"), " - ?"), 0.7, 0.55, scale)
				grid.circle(x = 0.45, y = 0.55, r = 0.06, gp = gpar(fill = "#FFFFFF", lwd = 3 * scale))
				grid.t("?", 0.45, 0.55, scale * 1.5, bg = FALSE)
				grid.lines(x = c(0.16, 0.39), y = c(0.55, 0.55), gp = gpar(lwd = 3 * scale))
				grid.lines(x = c(0.45, 0.45), y = c(0.64, 0.82), gp = gpar(lwd = 3 * scale))
				#grid.lines(x = c(0.56, 0.355), y = c(0.88, 0.49), gp = gpar(lwd = 3, lty = "dotted"))
			}

			if (i >= 3) {
				grid.t("Symbol = similar", 0.7, 0.3, scale)
				grid.curve(x1 = 0.8, x2 = 0.94, y1 = 0.3, y2 = 0.7, gp = gpar(lwd = 3 * scale), arrow = arrow(length = unit(0.02, "npc")))
			}
			if (i >= 4) {
				grid.t("No symbol = not similar,\ntherefore each to distinguish", 0.7, 0.2, scale)
			}



			dev.off()
		}
		gifski::gifski(png_files = list.files(path = "temp", full.names = TRUE), width = 500 * scale, height = 375 * scale, delay = 1.5, gif_file = paste0("inst/img/simi", X, scale, "x.gif"), loop = FALSE)
	}
}

################################
#### HUE necklace #################
################################
for (scale in 1:2) {
	unlink("temp", recursive = T, force = T)
	dir.create("temp")
	for (i in 1:3) {
		png(paste0("temp/hue_lines", i, ".png"), width = 375*scale, height = 375*scale, bg = "transparent")
		#c4a_plot_hues(c4a("brewer.set1", 7))
		if (i >= 1) {
			grid.t("Where are the palette colors placed\non the hue necklace?", 0.53, 0.75, scale, bg = FALSE)
		}
		if (i >= 2) {
			grid.t("The distribution can be used for further analysis,", 0.53, 0.65, scale, bg = FALSE)
		}
		if (i >= 3) {
			grid.t("For instance:\n- are the colors of a categorical palette\nequally spread out?\n - do sequential palette colors have\none hue or a spectrum of hues?", 0.53, 0.45, scale, bg = FALSE)
		}

		dev.off()
	}
	gifski::gifski(png_files = list.files(path = "temp", full.names = TRUE), width = 375*scale, height = 375*scale, delay = 1.5, gif_file = paste0("inst/img/hue_neck", scale, "x.gif"), loop = FALSE)
}


################################
#### CL plot #################
################################
for (scale in 1:2) {
	unlink("temp", recursive = T, force = T)
	dir.create("temp")
	for (i in 1:2) {
		png(paste0("temp/cl_plot", i, ".png"), width = 600*scale, height = 600*scale, bg = "transparent")
		#c4a_plot_CL(c4a("brewer.set1", 7))
		if (i >= 1) {
			grid.t("Horizontal axes: how vivid are the colors?", 0.5, 0.07, scale, bg = FALSE)
		}
		if (i >= 2) {
			grid.t("Vertical axes:\nhow bright are the colors?", 0.2, 0.3, scale, bg = FALSE)
		}


		dev.off()
	}
	gifski::gifski(png_files = list.files(path = "temp", full.names = TRUE), width = 600*scale, height = 600*scale, delay = 1.5, gif_file = paste0("inst/img/cl_plot", scale, "x.gif"), loop = FALSE)
}

################################
#### fairness plot #################
################################
for (scale in 1:2) {
	unlink("temp", recursive = T, force = T)
	dir.create("temp")
	for (i in 1:4) {
		png(paste0("temp/fair_plot", i, ".png"), width = 600*scale, height = 600*scale, bg = "transparent")
		#c4a_plot_CL(c4a("brewer.set1", 7))
		if (i >= 1) {
			grid.t("Fairness: do the palette colors stand out about equally?", 0.3, 0.85, scale, bg = FALSE, just = "left")
		}

		if (i >= 2) {
			grid.t("The lower the chroma range -x axis- the more colors are equally saturated: fair", 0.5, 0.07, scale, bg = FALSE)
		}
		if (i >= 3) {
			grid.t("The lower the luminance range (y axis), the more colors are equally bright: fair", 0.05, 0.2, scale, bg = FALSE, just = "left")
		}
		if (i >= 4) {
			grid.t("So the higher the score, the fairer the color palette is!", 0.3, 0.75, scale, bg = FALSE, just = "left")
		}

		dev.off()
	}
	gifski::gifski(png_files = list.files(path = "temp", full.names = TRUE), width = 600*scale, height = 600*scale, delay = 1.5, gif_file = paste0("inst/img/fair_plot", scale, "x.gif"), loop = FALSE)
}

################################
#### CR table #################
################################

for (scale in 1:2) {
	unlink("temp", recursive = T, force = T)
	dir.create("temp")
	for (i in 1:5) {
		png(paste0("temp/table", i, ".png"), width = 400 * scale, height = 300 * scale, bg = "transparent")
		#grid.rect(x = 0.50, y = 0.4, width = 0.7, height = 0.8, gp = gpar(fill = "orange"))
		#c4a_plot_CR_matrix(c4a("brewer.accent", 7))
		if (i >= 1) {
			grid.t("Color 1", x = 0.05, y = 0.97, scale, bg = FALSE)
			grid.t("Color 2", x = 0.2, y = 0.97, scale, bg = FALSE)
			grid.lines(x = c(0.05, 0.05), y = c(0.92, 0.82), arrow = arrow(length = unit(0.015, "npc")), gp = gpar(lwd = 2 * scale))
			grid.lines(x = c(0.28, 0.35), y = c(0.97, 0.97), arrow = arrow(length = unit(0.015, "npc")), gp = gpar(lwd = 2 * scale))
		}
		if (i >= 2) {
			grid.rect(x = 0.45, y = 0.45, width = 0.6, height = 0.8, gp = gpar(col = NA, fill = "white"))
			grid.t("The contract ratio between these two colors\nindicates how distinguishable they are\n- click to select other colors-", 0.5, 0.35, scale, r = 0.1, bg = FALSE)
			grid.circle(x = 0.31, y = 0.55, r = 0.06, gp = gpar(fill = "#FFFFFF", lwd = 3 * scale))
			grid.t("?", 0.31, 0.55, scale * 1.5, bg = FALSE)
			grid.lines(x = c(0.16, 0.25), y = c(0.55, 0.55), gp = gpar(lwd = 3 * scale))
			grid.lines(x = c(0.31, 0.31), y = c(0.64, 0.82), gp = gpar(lwd = 3 * scale))
			#grid.lines(x = c(0.56, 0.355), y = c(0.88, 0.49), gp = gpar(lwd = 3, lty = "dotted"))
		}

		if (i >= 3) {
			grid.t("Hard to distinguigh\n- borders recommended -", 0.56, 0.76, scale, bg = FALSE)
			grid.curve(x1 = 0.74, x2 = 0.815, y1 = 0.71, y2 = 0.76, gp = gpar(lwd = 3 * scale), arrow = arrow(length = unit(0.02, "npc")))
		}
		if (i >= 4) {
			grid.t("Easy to distinguigh\n- suitable for text -", 0.57, 0.56, scale, bg = FALSE)
			grid.curve(x1 = 0.74, x2 = 0.815, y1 = 0.51, y2 = 0.56, gp = gpar(lwd = 3 * scale), arrow = arrow(length = unit(0.02, "npc")))
		}
		if (i >= 5) {
			grid.t("White and black added to analyse\nstandard text readability", 0.4, 0.15, scale, bg = FALSE)
		}

		dev.off()
	}
	gifski::gifski(png_files = list.files(path = "temp", full.names = TRUE), width = 400 * scale, height = 300 * scale, delay = 1.5, gif_file = paste0("inst/img/table", scale, "x.gif"), loop = FALSE)

}

#Do you see a 3D effect? If not check 'Use original colors' below. Chromostereopsis is a visual illusion that happens when a blue color is shown next to a red color with a dark background.



################################
#### 3D Blues ##################
################################

for (scale in 1:2) {
	unlink("temp", recursive = T, force = T)
	dir.create("temp")
	for (i in 1:4) {
		png(paste0("temp/blues", i, ".png"), width = 550 * scale, height = 550 * scale, bg = "transparent")
		#grid.rect(x = 0.50, y = 0.4, width = 0.7, height = 0.8, gp = gpar(fill = "orange"))
		#c4a_plot_CR_matrix(c4a("brewer.accent", 7))
		if (i >= 1) {
			grid.t(" Do you see a 3D effect?", x = 0.05, y = 0.95, scale, bg = TRUE, just = "left")
		}
		if (i >= 2) {
			grid.t(" If not check 'Use optical illusion's original colors' below    ", 0.35, 0.95, scale, bg = TRUE, just = "left")
		}
		if (i >= 3) {
			grid.t(" Chromostereopsis is a visual illusion\n that happens when a blue color is shown\n next to a red color with a dark background", 0.05, 0.1, scale, bg = TRUE, r = 0.1, just = "left")
		}
		if (i >= 4) {
			grid.t(" Check out if the palette may suffer from this illusion   ", 0.05, 0.02, scale, bg = TRUE, r = 0.1, just = "left")
		}

		dev.off()
	}
	gifski::gifski(png_files = list.files(path = "temp", full.names = TRUE), width = 550 * scale, height = 550 * scale, delay = 1.5, gif_file = paste0("inst/img/blues", scale, "x.gif"), loop = FALSE)

}


################################
#### Naming ##################
################################

for (scale in 1:2) {
	unlink("temp", recursive = T, force = T)
	dir.create("temp")
	for (i in 1:5) {
		png(paste0("temp/naming", i, ".png"), width = 1000 * scale, height = 600 * scale, bg = "transparent")
		#grid.rect(x = 0.50, y = 0.4, width = 0.7, height = 0.8, gp = gpar(fill = "orange"))
		#c4a_plot_names2(c4a("brewer.accent"))
		if (i >= 1) {
			grid.lines(x = c(0.35, 0.75), y = c(0.68, 0.68), arrow = arrow(length = unit(0.02, "npc"), ends = "both"), gp = gpar(lwd = 2 * scale))
			grid.t("Most common color names", 0.55, 0.66, scale)
		}
		if (i >= 2) {
			grid.rect(x = 0.29, y = 0.835, width = 0.08, height = 0.325, gp = gpar(fill = NA, col = "#000000", lwd = 3 * scale))
			grid.t(" Multitude of colors named after\neach color - in this example \"purple\" -", 0.245, 0.8, scale, bg = TRUE, just = "right")
		}
		if (i >= 3) {
			grid.t(" How likeliy is it that this palette color is named \"yellow\"?", 0.48, 0.435, scale, bg = TRUE, r = 0.1, just = "left")
			#grid.circle(x = 0.45, y = 0.50, r = 0.06, gp = gpar(fill = NA, col = "#FFFFFF", lwd = 3 * scale))
			grid.circle(x = 0.455, y = 0.435, r = 0.03, gp = gpar(fill = NA, col = "#000000", lwd = 3 * scale, lty = "dotted"))
			#grid.circle(x = 0.55, y = 0.40, r = 0.06, gp = gpar(fill = NA, col = "#FFFFFF", lwd = 3 * scale))
		}
		if (i >= 4) {
			grid.lines(x = c(0.1, 0.95), y = c(0.15, 0.15), gp = gpar(lwd = 3 * scale, lty = "dotted"))
			grid.t(" A horizontal line is a warning: the palette color does not have a unique name ", 0.5, 0.13, scale, bg = TRUE, r = 0.1, just = "left")
		}
		if (i >= 5) {
			grid.lines(x = c(0.37, 0.37), y = c(0.05, 0.65), gp = gpar(lwd = 3 * scale, lty = "dotted"))
			grid.t(" A vertical line is also a warning: multiple palette colors can be called \"pink\" ", 0.38, 0.25, scale, bg = TRUE, r = 0.1, just = "left")
		}

		dev.off()
	}
	gifski::gifski(png_files = list.files(path = "temp", full.names = TRUE), width = 1000 * scale, height = 600 * scale, delay = 1.5, gif_file = paste0("inst/img/naming", scale, "x.gif"), loop = FALSE)

}
