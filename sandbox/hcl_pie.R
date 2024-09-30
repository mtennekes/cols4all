library(grid)





# HCL pie
df = rbind(expand.grid(H = seq(0, 360, by = 1), C = 100, L = seq(20, 80, by = 1)),
		   expand.grid(H = seq(0, 360, by = 1), C = seq(0, 100, by = 1), L = 80))

# # HCL full
# df = rbind(expand.grid(H = seq(0, 360, by = 1), C = NA, L = seq(20, 80, by = 1)))#,
# 		 #  expand.grid(H = seq(0, 360, by = 1), C = seq(0, 160, by = 5), L = 80))
# df$max_croma = pmin(110,  colorspace::max_chroma(df$H, df$L))
# df$C[is.na(df$C)] = df$max_croma[is.na(df$C)]
# df = df[df$C <= df$max_croma, ]
#

add_coords = function(df, height = .6, amp_s = 0.48, amp_c = 0.30) {
	df = within(df, {
		x = 0.5 + sin(H / 180 * pi) * (C / 100) * amp_s
		y = 0.2 + cos(H / 180 * pi) * (C / 100) * amp_c + L / 100 * height
	})
}

add_coords2= function(df, width = 180) {
	df = within(df, {
		x = sin(H / 180 * pi) * (C / 180) * width
		y = cos(H / 180 * pi) * (C / 180) * width
	})
}


df = add_coords(df)

df$col = hcl(df$H, df$C, df$L)

pal = c4a("tol.muted")
pal = c("#FF0000", "#0000FF", "#00FF00")

df2 = as.data.frame(get_hcl_matrix(pal))
df2$L = 80
df2$C = 90
df2 = add_coords(df2)

# annotation data.frame: 3 coords for L, 3 coords for C, x+1 for H (the first ones are for the labels, the other for the arcs)
x = 100
a = data.frame(H = c(205, 240, 240,
					 20, 330, 330,
					 200, seq(0, 300, length.out = 100)),
			   C = c(100, 100, 100,
			   	  45, 0, 97,
			   	  70, rep(90, x)),
			   L = c(65, 22, 76,
			   	  80, 80, 80,
			   	  80, rep(80, x)))
a = add_coords(a)

cex = 1.5
lwd = 2
for (scale in 1:2) {
	png(paste0("inst/img/hcl_spacex", scale, ".png"), width = 200*scale, height = 200*scale, bg = "transparent")
	grid.newpage()
	#grid.rect(gp=gpar(fill="black"))
	grid.points(x = df$x, y = df$y, default.units = "npc", pch = 16, size = grid::unit(5*scale, "points"), gp=gpar(col = df$col))
	#grid.points(x = df2$x, y = df2$y, default.units = "npc", pch = 16, size = grid::unit(5, "points"), gp=gpar(col = "#000000"))


	grid.text("Luminance", a$x[1], a$y[1], gp=gpar(fontfamily = "xkcd", cex = cex*scale))
	grid.lines(a$x[2:3], a$y[2:3], arrow = arrow(length = unit(6 * lwd, "point")), gp = gpar(lwd = lwd*scale))
	grid.text("Chroma", a$x[4], a$y[4], gp=gpar(fontfamily = "xkcd", cex = cex*scale))
	grid.lines(a$x[5:6], a$y[5:6], arrow = arrow(length = unit(6 * lwd, "point")), gp = gpar(lwd = lwd*scale))
	grid.text("Hue", a$x[7], a$y[7], gp=gpar(fontfamily = "xkcd", cex = cex*scale))
	grid.lines(a$x[8:nrow(a)], a$y[8:nrow(a)], arrow = arrow(length = unit(6 * lwd, "point")), gp = gpar(lwd = lwd*scale))
	dev.off()
}




### HCL rock

library(plotly)
# volcano is a numeric matrix that ships with R
fig <- plot_ly(z = ~volcano)
fig <- fig %>% add_surface()

fig



#hdf = hdf[hdf$C <= 50 & hdf$L <= 80 & hdf$L >= 20, ]
