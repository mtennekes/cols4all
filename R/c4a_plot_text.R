c4a_plot_text = function(cols, dark = FALSE) {
	grid::grid.newpage()

	bc = ifelse(dark, "#000000", "#FFFFFF")

	grid::grid.rect(gp=grid::gpar(fill = bc, col = NA))

	n = length(cols)

	rgb_text = cols4all:::get_rgb_triple(cols)

	nl = grid::convertHeight(grid::unit(1, "npc"), unitTo = "lines")

	cex = min(3, (nl / (n * 1.25)))

	for (i in 1:n) {
		grid::grid.text(x = 0.25, y = (i - 0.5)/n, paste0(cols[i], " (", rgb_text[i], ")"), just = "left", gp = grid::gpar(cex = cex, col = cols[i]))
	}

}
