c4a_plot_names = function(cols = NULL) {
	grid::grid.newpage()
	grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow = 3, ncol = 2, widths = c(0.5, 0.5))))

	if (!is.null(cols)) {
		names(cols) = seq_along(cols)
	}

	hex = c(cols, boynton)

	m = as.data.frame(get_hcl_matrix(hex))
	m$hex = hex


	Ls = c(40, 60, 80)

	labels = c("Luminance 20 to 50",
			   "Luminance 50 to 70",
			   "Luminance 70 to 90",
			   "Luminance max")

	m$L2 = sapply(m$L, function(l) Ls[which.min(abs(l - Ls))])

	bs = split(m, f = m$L2)
	df = expand.grid(row = 1:2, col = 1:2)

	for (i in 1:length(bs)) {
		bsi = bs[[i]]
		lm = paste0("L", names(bs)[i])
		h = bsi$hex
		names(h) = rownames(bsi)

		cellplot(df$row[i], df$col[i], {
			plot_rgb(confusion_lines = FALSE, colors = h, L = lm, newpage = FALSE)
			grid::grid.text(labels[i], x = .9, y = 0.05, just = "right")
		})
	}
	cellplot(df$row[4], df$col[4], {
		h = m$hex
		names(h) = rownames(m)

		plot_rgb(confusion_lines = FALSE, colors = h, L = "L100", newpage = FALSE)
		grid::grid.text(labels[4], x = .9, y = 0.05, just = "right")
	})
	grid::upViewport()
}
