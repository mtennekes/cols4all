c4a_plot_floating_text = function(words = c("cols", "4", "all"), cols = c("#0000FF", "#FF0000"), size = 10) {
	n1 = length(words)
	n2 = length(cols)

	n = max(n1, n2)

	words = rep(words, length.out = n)
	cols = rep(cols, length.out = n)

	grid::grid.newpage()

	grid::grid.rect(gp=grid::gpar(fill = "#000000"))

	#grid::pushViewport(grid::viewport(width = ))

	d = par("din")
	dasp = d[1] / d[2]

	grid::pushViewport(grid::viewport(width = grid::unit(2, "snpc"), height = grid::unit(1, "snpc")))


	npc_w = function(txt, cex = 1) {
		as.vector(grid::convertWidth(grid::stringWidth(txt), unitTo = "npc")) * cex
	}


	ws = npc_w(words, cex = size * 1.5)


	grid::grid.text(words, x = -sum(ws)/2 + 0.5 + c(0, head(cumsum(ws), -1)) + ws/2, just = "center",
					gp = grid::gpar(cex = size, fontface = "bold", col = cols))


}

