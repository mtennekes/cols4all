library(fitdistrplus)
library(tidyverse)
library(grid)
ABcols = local({
	A = readRDS("build/Dutch_annotations_2024_01_11.rds")
	cols = rownames(A)
	cn_EN = c("Blue", "Brown", "Yellow", "Gray", "Green", "Orange", "Purple", "Red", "Pink", "White", "Black")

	boynton_names = c("Green", "Blue", "Purple", "Pink", "Yellow", "Brown", "Orange",
					  "Red", "White", "Gray", "Black")


	#B = cols4all:::diff_boynton(cols)
	#B2 = cols4all:::diff_boynton_softmax(cols, a = 0.1, th = 0)
	A2 = A[, match(boynton_names, cn_EN)]
	colnames(A2) = boynton_names


	# remove white, gray and black
	#A2 = A2[, 1:8]
	#B2 = B2[, 1:8]

	sel = rowSums(A2) > 0

	A3 = A2[sel, ] / rowSums(A2)[sel]

	#B3 = B2[sel, ]
	cols = cols[sel]

	list(A = A3, cols = cols)
})

A = ABcols$A
cols = ABcols$cols

# function that illustrates how colors were named
plot_col = function(col) {
	grid.newpage()
	grid.rect(gp=gpar(fill = "#000000"))
	grid.rect(width = 0.4, height = 0.4, gp=gpar(fill = col))
}

# rownames should be colors (hex format)
# rows should correspond to annotation labels
fit_color_anno_matrix = function(A, cbuffer = 50, lbuffer = 50) {
	k = ncol(A)
	cols = rownames(A)
	hcl = get_hcl_matrix(cols)

	res = lapply(1:k, function(i) {
		weights = round(A[,i] * 1000)
		res = lapply(1:3, function(x) {
			res = unlist(mapply(rep, hcl[,x], weights))

			# hue: set the median in the center, with -180, 180 range around it
			if (x == 1) {
				tab = table(cut(res, breaks = seq(0, 360, by = 20)))
				hmedian = which.max(tab) * 20 - 10
				if (hmedian < 180) {
					res[res > (hmedian + 180)] = res[res > (hmedian + 180)] - 360
				} else {
					res[res < (hmedian - 180)] = res[res < (hmedian - 180)] + 360
				}
				mn = hmedian - 180
				mx = hmedian + 180
			} else if (x == 2) {
				mn = 0 - cbuffer
				mx = 160 + cbuffer
			} else {
				mn = 0 - lbuffer
				mx = 100 + lbuffer
			}

			res01 = (res - mn) / (mx - mn)

			fit = fitdist(res01, "beta")
			fit = list(estimate = fit$estimate)

			modus = min(mx, max(mn, ((fit$estimate["shape1"] - 1) / (fit$estimate["shape1"] + fit$estimate["shape2"] - 2)) * (mx - mn) + mn))

			list(name = colnames(A)[i], dim = c("H", "C", "L")[x], fit = fit, mn = mn, mx = mx, modus = modus)
		})
		names(res) = c("H", "C", "L")
		proto = do.call(grDevices::hcl, structure(lapply(res, function(x) x$modus), names = c("h", "c", "l")))
		res$proto = proto
		res
	})
	names(res) = colnames(A)
	res
}

names_NL_model = fit_color_anno_matrix(A)
names_NL_colors = structure(sapply(names_NL_model, "[[", "proto"), names = names(names_NL_model))


save(.z, .s, .zbib, .zdes, shp, shp_c, bbx, rgb_data, rdata, names_NL_model, names_NL_colors, file = "R/sysdata.rda", compress = "xz")

