c4a_plot_bars = function(col1 = "blue", col2 = "red", borders = "black", lwd = 0, dark = FALSE) {

	if (lwd == 0) borders = NA
	x = rdata$bars.x
	m = matrix(c(x, 100-x), nrow = 2, byrow = TRUE, dimnames = list(c("v1", "v2"), LETTERS[1:5]))

	bc = ifelse(dark, "#000000", "#FFFFFF")

	par(bg = bc)

	barplot(m, col = c(col1, col2), border = borders, xlab = "Group", ylab = "Percentage", space = 0.2)

	for (i in 1:5) {
		cx = (i - 1) + i * 1/5
		lines(c(cx, cx + 1), c(m[1, i], m[1, i]), lwd = lwd, col = borders, lend = "butt")
	}

}

c4a_plot_lines = function(col1 = "blue", col2 = "red", borders = "black", lwd = 1, asp = 1) {

	x = rdata$lines.x
	s1 = rdata$lines.s1
	s2 = rdata$lines.s2

	compress = function(x, a) {
		(x - 0.5) * a + 0.5
	}


	mar = par(mar = c(0, 0, 0, 0))

	if (asp >= 1) {
		s1 = compress(s1, 1/asp)
		s2 = compress(s2, 1/asp)
		plot(x, s1, type = "l", col = col1, xaxt = "n", yaxt = "n", xlab = "", ylab = "", main = "", ylim = c(0, 1), lwd = lwd, bty = "n")
		lines(x, s2, col = col2, lwd = lwd)
	} else {
		s1 = compress(s1, asp)
		s2 = compress(s2, asp)

		plot(s1, x, type = "l", col = col1, xaxt = "n", yaxt = "n", xlab = "", ylab = "", main = "", xlim = c(0, 1), lwd = lwd, bty = "n")
		lines(s2, x, col = col2, lwd = lwd)
	}

	par(mar)
}
