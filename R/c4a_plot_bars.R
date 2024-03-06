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

c4a_plot_lines = function(cols = c("blue", "red"), lwd = 1, asp = 1, dark = FALSE, stacked = FALSE, bc = ifelse(dark, "#000000", "#FFFFFF")) {

	par(bg = bc)

	x = rdata$lines.x
	s = rdata$lines.s

	k = min(length(cols),36)
	cols = rep(cols, length.out=k)
	s = s[1:k]

	compress = function(x, a) {
		(x - 0.5) * a + 0.5
	}

	if (stacked) {
		s = mapply(function(si,i) {
			(i-1)/k + (si / k)
		}, s, k:1, SIMPLIFY = FALSE)
	}

	mar = par(mar = c(0, 0, 0, 0))

	if (asp >= 1) {
		s = lapply(s, function(s) {
			compress(s, 1/asp)
		})
		plot(x, s[[1]], type = "l", col = cols[1], xaxt = "n", yaxt = "n", xlab = "", ylab = "", main = "", ylim = c(0, 1), lwd = lwd, bty = "n")
		if (k>1) {
			for (i in 2:k) {
				lines(x, s[[i]], col = cols[i], lwd = lwd)
			}
		}
	} else {
		s = lapply(s, function(s) {
			compress(s, asp)
		})

		plot(s[[1]], x, type = "l", col = cols[1], xaxt = "n", yaxt = "n", xlab = "", ylab = "", main = "", xlim = c(0, 1), lwd = lwd, bty = "n")
		if (k>1) {
			for (i in 2:k) {
				lines(s[[i]], x, col = cols[i], lwd = lwd)
			}
		}
	}
	par(mar)
}
