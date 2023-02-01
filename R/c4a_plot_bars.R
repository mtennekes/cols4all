c4a_plot_bars = function(col1 = "blue", col2 = "red", borders = "black", lwd = 0, dark = FALSE) {

	#set.seed(1234)
	if (lwd == 0) borders = NA
	x = .C4A$rdata.bars.x
	m = matrix(c(x, 100-x), nrow = 2, byrow = TRUE, dimnames = list(c("v1", "v2"), LETTERS[1:5]))

	bc = ifelse(dark, "#000000", "#FFFFFF")

	par(bg = bc)

	barplot(m, col = c(col1, col2), border = borders, xlab = "Group", ylab = "Percentage", space = 0.2)

	for (i in 1:5) {
		cx = (i - 1) + i * 1/5
		lines(c(cx, cx + 1), c(m[1, i], m[1, i]), lwd = lwd, col = borders, lend = "butt")
	}

}
