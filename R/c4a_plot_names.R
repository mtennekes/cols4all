c4a_plot_names = function(cols = NULL, dark = FALSE, a = NA, th = NA) {

	n = length(cols)
	b = length(.C4A$naming_colors)

	dfs = .C4A$name_data

	vdf = expand.grid(row = 1:(2*n+3), col = 1:(2*b+3))
	#vdf$name = c("Green", "Yellow", "Orange", "Blue", "Purple", "Brown", "Pink", "Red", "", "White", "Gray", "Black")

	#vdf = vdf[vdf$name != "", ]

	col_pal_w = 0.07
	col_marg1 = 0.01

	col_tot = (1 - col_pal_w - 2 * col_marg1) / b

	col_bt_w = col_tot * 0.8
	col_marg2 = col_tot * 0.2


	row_bt_h = 0.3
	row_marg1 = 0.02

	row_tot =  (1 - row_bt_h - 2 * row_marg1) / n

	row_pal_h = row_tot * 0.8
	row_marg2 = row_tot * 0.2


	m = c4a_naming_matrix_softmax(cols, a = a, th = th)

	grid::grid.newpage()
	grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow = (2*n+3), ncol = (2*b+3),
		heights = grid::unit(c(row_marg1, row_bt_h, row_marg1, rep(c(row_pal_h, row_marg2), n)), units = "npc"),
		widths = grid::unit(c(col_marg1, col_pal_w, col_marg1, rep(c(col_bt_w, col_marg2), b)), units = "npc"))))

	bc = if (dark) "#000000" else "#FFFFFF"
	fc = if (dark) "#FFFFFF" else "#000000"

	grid::grid.rect(gp = grid::gpar(fill = bc, col = NA))



	for (i in 1:b) {
		dfi = dfs[[i]]

		cellplot(2, i*2+2, {
			#grid::pushViewport(grid::viewport(width = grid::unit(1, "npc"), height = grid::unit(1, "npc"), clip = TRUE))
			g = grid::pointsGrob(x = grid::unit(dfi$x, "npc"), y = grid::unit(dfi$y * .8 + .2, "npc"), size = grid::unit(0.15, "npc"), pch = 19, gp = grid::gpar(col = dfi$hex))
			#grid::grid.rect()
			grid::grid.draw(g)
			grid::grid.text(names(.C4A$naming_colors)[i], y = .1, gp = grid::gpar(col = fc))
			#grid::upViewport()
		})

		for (j in 1:n) {
			if (i == 1) {
				cellplot(j*2+2, 2, {
					grid::grid.text(j, x = 0.15, gp = grid::gpar(col = fc))
					grid::grid.rect(x = 0.7, width = 0.6, gp = grid::gpar(fill = cols[j]))
				})
			}

			v = sqrt(m[j,i])

			#conflict_vert = sum(m[,i] >0) > 1
			#conflict_hori = sum(m[j,] >0) > 1

			if (i == 1) {
				if (sum(m[j,] >0) > 1) {
					cellplot(j*2+2, 4:(b*2+2), {
						grid::grid.lines(y = grid::unit(c(0.5, 0.5), "npc"), gp = grid::gpar(col = fc, lwd = 2))
					})
				}
			}
			if (j == 1) {
				if (sum(m[,i] >0) > 1) {
					cellplot(4:(n*2+2), i*2+2, {
						grid::grid.lines(x = grid::unit(c(0.5, 0.5), "npc"), gp = grid::gpar(col = fc, lwd = 2))
					})
				}
			}

			if (v > 0) {
				cellplot(j*2+2, i*2+2, {
					grid::grid.points(x = grid::unit(0.5, "npc"), y = grid::unit(0.5, "npc"), pch = 19, size = grid::unit(v, "lines"), gp = grid::gpar(col = fc))
				})
			}
		}
	}
	grid::upViewport()
}
