library(pals)
library(rcartocolor)
library(grid)
library(colorblindcheck)


##
# cat
#
# seq
# -single hue
# -twisted hue
# -spectral
#
# div
# - two hue
# - three hue
#
# cyc
#
# biv
#
# tri

#####################################################
######### categorical
#####################################################

dist_to_col = function(pal, col) {
	colorblindcheck::palette_dist(c(col, pal))[1,-1]
}

min_dist_cvd = function(pal) {
	min(sapply(c("deu", "pro", "tri"), function(cvd, x) {
		min(colorblindcheck::palette_dist(x = x, cvd = cvd), na.rm = TRUE)
	}, x = pal))
}

remove_black_white = function(pal, th = 5) {
	blcks = dist_to_col(pal, "#000000") <= th
	whts = dist_to_col(pal, "#FFFFFF") <= th
	pal[!blcks & !whts]
}


cat_grDevices = local({
	grDevices:::.palette_colors_hex |>
	lapply(remove_black_white)
})


cat_pals = local({
	pals_syspals = pals:::syspals
	palsCat = c("alphabet", "alphabet2", "cols25", "glasbey", "kelly", "polychrome", "stepped", "stepped2", "stepped3", "okabe", "tableau20", "tol", "tol.groundcover", "watlington")

	pals = lapply(palsCat, function(p) {
		if (p %in% names(pals_syspals)) {
			pal = pals_syspals[[p]]
			if (is.list(pal)) {
				pal = lapply(pal, remove_black_white)
			} else {
				pal = remove_black_white(pal)
			}
		} else {
			pal = unname(do.call(p, args = list())) |>
				remove_black_white()
		}
		pal
	})
	names(pals) = palsCat
	pals
})




# library(khroma)
#
# khroma:::.schemes
#
# tols = c("bright",
# "contrast",
# "highcontrast",
# "vibrant",
# "muted",
# "mediumcontrast",
# "pale",
# "dark",
# "light")
#
# khroma:::.schemes[tols]

# from https://personal.sron.nl/~pault/data/tol_colors.py
cat_tols = list(tol.bright = c('#4477AA', '#EE6677', '#228833', '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB', '#000000'),
			tol.contrast = c('#004488', '#DDAA33', '#BB5566', '#000000'),
			tol.vibrant = c('#EE7733', '#0077BB', '#33BBEE', '#EE3377', '#CC3311', '#009988', '#BBBBBB', '#000000'),
			tol.muted = c('#CC6677', '#332288', '#DDCC77', '#117733', '#88CCEE', '#882255', '#44AA99', '#999933', '#AA4499', '#DDDDDD','#000000'),
			tol.medium = c('#6699CC', '#004488', '#EECC66', '#994455', '#997700','#EE99AA', '#000000'),
			tol.light = c('#77AADD', '#EE8866', '#EEDD88', '#FFAABB', '#99DDFF','#44BB99', '#BBCC33', '#AAAA00', '#DDDDDD', '#000000'))


z_cat = c(cat_grDevices, cat_pals, cat_tols)


z_cat_score = check_cat_list(z_cat)



save(z_cat, z_cat_score, file="R/sysdata.rda", compress="xz")
