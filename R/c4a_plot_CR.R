c4a_plot_CR = function(p, dark = FALSE, title = FALSE, sort = FALSE, lines_WCAG = TRUE, lines_equiluminance = TRUE) {
	cr = colorspace::contrast_ratio(p)

	if (sort) {
		id = order(cr)

		cr = cr[id]
		p = p[id]
	}


	if (dark) {
		cr = 21 / cr
		bc = "#000000"
		fc = "#FFFFFF"
		lc1 = "gray70"
		lc2 = "gray80"
	} else {
		bc = "#FFFFFF"
		fc = "#000000"
		lc1 = "gray70"
		lc2 = "gray30"
	}

	# to plot y axis from 1 instead of 0
	cr = cr - 1

	if (lines_WCAG) {
		crWCAG = 7.2
	} else {
		crWCAG = 0
	}

	if (lines_equiluminance) {
		crm = cols4all:::get_CR_matrix(p)
		els = which(crm <= 1.2)
		if (length(els)) {
			eldf = data.frame(c1 = (els-1) %/% length(p) + 1,
							  c2 = (els-1) %% length(p) + 1)
			eldf = eldf[eldf$c1 < eldf$c2, ]
			eldf$cr1 = cr[eldf$c1]
			eldf$cr2 = cr[eldf$c2]

			# find 3rds
			eldf$c3 = lapply(1:nrow(eldf), function(i) NULL)
			eldf$cr3 = lapply(1:nrow(eldf), function(i) 0)
			eldf$red = FALSE
			for (i in 1:nrow(eldf)) {
				partner1 = eldf$c2[which(eldf$c1 == eldf$c1[i])]
				partner2 = eldf$c2[which(eldf$c1 == eldf$c2[i])]

				partners = intersect(partner1, partner2)
				if (length(partners)) {
					eldf$c3[[i]] = partners
					eldf$cr3[[i]] = cr[partners]

					c123 = unique(c(eldf$c1[i], eldf$c2[i], partners))

					eldf$red[setdiff(which(eldf$c1 %in% c123 & eldf$c2 %in% c123), i)] = TRUE
				}
			}
			eldf = eldf[!eldf$red, ]

			eldf$cr_ceiling = 0
			for (i in 1:nrow(eldf)) {
				eldf$cr_ceiling[i] = max(c(eldf$cr1[i], eldf$cr2[i], eldf$cr3[[i]])) + 0.2
			}

			crmax = max(cr, eldf$cr_ceiling, crWCAG)
		} else {
			crmax = max(cr, crWCAG)
			eldf = NULL
		}
	} else {
		crmax = max(cr, crWCAG)
		eldf = NULL
	}


	if (identical(title, TRUE)) {
		title = paste0("Contrast ratio with ", ifelse(dark, "black", "white"))
	} else if (identical(title, FALSE)) {
		title = ""
	}

	opt = par(bg = bc, col.axis = fc, col.lab = fc, col.main = fc, col.sub = fc)
	#mar = par(mar = c(0, 0, 0, 0))

	ylim = c(0, ceiling(crmax))

	co = barplot(cr,
			main = title,
			col = NA,#p,
			xlab = "",
			ylab = "",
			ylim = ylim,
			border = NA,
			names.arg = p,
			yaxt = "n",
			las = 1)

	brks = pretty(c(0, ceiling(crmax)), n = 7)

	if (lines_WCAG) {
		brks = sort(c(brks, 2, 3.5, 6))
	}
	labs = brks + 1

	if (lines_WCAG) {
		labs[labs == 3] = paste0("A - ", labs[labs == 3])
		labs[labs == 4.5] = paste0("AA - ", labs[labs == 4.5])
		labs[labs == 7] = paste0("AAA - ", labs[labs == 7])
	}

	axis(2, at = brks, labels = labs, las = 2)

	if (lines_WCAG) {
		labelX = 0#co[nrow(co), 1] + (co[nrow(co), 1] - co[nrow(co) - 1, 1]) * .5
		buffer = 0.15

		abline(h = 2, col = lc1, lty = "solid", lwd = 2)
		abline(h = 3.5, col = lc1, lty = "solid", lwd = 2)
		abline(h = 6, col = lc1, lty = "solid", lwd = 2)
		#text(x = labelX, y = 2 + buffer, labels = "A", col = "black", adj = 1, cex = 0.8)
		#text(x = labelX, y = 3.5 + buffer, labels = "AA", col = "black", adj = 1, cex = 0.8)
		#text(x = labelX, y = 6 + buffer, labels = "AAA", col = "black", adj = 1, cex = 0.8)
	}

	barplot(cr, col = p, add = TRUE, ylab = "", xlab = "", yaxt = "n")

	if (!is.null(eldf)) {
		for (i in 1:nrow(eldf)) {
			cs = c(eldf$c1[i], eldf$c2[i], eldf$c3[[i]])
			crs = c(eldf$cr1[i], eldf$cr2[i], eldf$cr3[[i]])
			crc = eldf$cr_ceiling[i]

			lines(x = co[c(cs[1], tail(cs, 1)), 1], y = c(crc, crc), lty = "dotted", lwd = 3, col = lc2)
			for (i in 1:length(cs)) {
				lines(x = co[c(cs[i], cs[i]), 1], y = c(crs[i], crc), lty = "dotted", lwd = 3, col = lc2)
			}
		}

	}

	par(opt)
}
