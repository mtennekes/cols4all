



c4a_plot_names2 = function(cols = NULL, a = 2, th = .1) {

	dfs = split(rdata$name_data, rdata$name_data$ids)
	names(dfs) = names(boynton)

	vdf = expand.grid(row = c(1,4,7), col = 1:4)
	vdf$name = c("Green", "Yellow", "Orange", "Blue", "Purple", "Brown", "Pink", "Red", "", "White", "Gray", "Black")

	vdf = vdf[vdf$name != "", ]

	grid::grid.newpage()
	grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow = 9, ncol = 4, heights = c(3,1,1,3,1,1,3,1,1))))



	is_dark = get_hcl_matrix(cols)[, 3] < 50


	mtch = match_colors(cols, a = a, th = th)
	cross = tabulate(unname(unlist(mtch)), nbins = length(cols)) > 1

	for (i in 1:11) {

		dfi = dfs[[match(vdf$name[i], names(boynton))]]

		cellplot(vdf$row[i], vdf$col[i], {
			grid::pushViewport(grid::viewport(width = grid::unit(1, "snpc"), height = grid::unit(1, "snpc"), clip = TRUE))
			g = grid::pointsGrob(x = unit(dfi$x, "npc"), y = unit(dfi$y, "npc"), size = unit(0.06, "npc"), pch = 19, gp = grid::gpar(col = dfi$hex))
			grid.draw(g)
			grid::upViewport()
		})
		cellplot(vdf$row[i]+1, vdf$col[i], {
			grid::grid.text(vdf$name[i], x = 0.1, just = "left")
			ids = mtch[[vdf$name[i]]]
			lgt = length(ids)
			if (lgt>0) {
				for (j in 1:lgt) {
					grid::grid.rect(x= .4+.1*j, y = .5, width = .09, height = 0.75, gp=grid::gpar(fill = cols[ids[j]]))
					cl = ifelse(is_light(cols[ids[j]]), "#000000", "#FFFFFF")

					if (cross[ids[j]]) grid::grid.polyline(x= .4+.1*j + .9*c(-.045, .045, -.045, .045), y = .5 + .9*c(-.375, .375, .375, -.375), id = c(1,1,2,2), gp = grid::gpar(col = cl))

					grid::grid.rect(x= .4+.1*j, y = .5, width = .03, height = 0.3, gp=grid::gpar(col = NA, fill = cols[ids[j]]))

					grid::grid.text(ids[j], x= .4+.1*j, y = .5, gp=gpar(col = cl))
				}
			}
		})
	}
	grid::upViewport()
}
