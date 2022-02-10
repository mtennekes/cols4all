library(pals)
library(rcartocolor)
library(grid)
library(colorblindcheck)


#####################################################
######### categorical
#####################################################


cat_grDevices = local({
	grDevices:::.palette_colors_hex |>
	lapply(remove_black_white) |>
	lapply(unname)
})
names(cat_grDevices) = c("R3", "R4", "ggplot2", "okabe", "brewer.accent", "brewer.dark2", "brewer.paired",
						 "brewer.pastel1", "brewer.pastel2", "brewer.set1", "brewer.set2", "brewer.set3", "tableau.10",
						 "tableau.classic", "polychrome", "alphabet")


cat_pals = local({
	pals_syspals = pals:::syspals
	palsCat = c("alphabet", "cols25", "glasbey", "kelly", "tableau20", "watlington")
	palsNew = c("alphabet2", "wright", "glasbey", "kelly", "tableau.20", "watlington") # alphabet2 = base "Alphabet"

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
	pals = lapply(pals, unname)
	names(pals) = palsNew
	pals
})


cat_tol = local({
	# Method:
	# * copy indices from tol_colors.py
	# * replace [ by list( and ] by )
	# * run:
	#     l2 = lapply(l, function(i) {unlist(i)+1L})
	#     names(l2) = sapply(l2, length)
	#     dput(l2)
	rainbow_ids = list(`1` = 10, `2` = c(10, 26), `3` = c(10, 18, 26), `4` = c(10, 15, 18, 26), `5` = c(10, 14, 15, 18, 26), `6` = c(10, 14, 15, 17, 18, 26), `7` = c(9, 10, 14, 15, 17, 18, 26), `8` = c(9, 10, 14, 15, 17, 18, 23, 26), `9` = c(9, 10, 14, 15, 17, 18, 23, 26, 28), `10` = c(9, 10, 14, 15, 17, 18, 21, 24, 26, 28), `11` = c(9, 10, 12, 14, 15, 17, 18, 21, 24, 26, 28), `12` = c(3, 6, 9, 10, 12, 14, 15, 17, 18, 21, 24, 26), `13` = c(3, 6, 9, 10, 12, 14, 15, 16, 17, 18, 21, 24, 26), `14` = c(3, 6, 9, 10, 12, 14, 15, 16, 17, 18, 20, 22, 24, 26), `15` = c(3, 6, 9, 10, 12, 14, 15, 16, 17, 18, 20, 22, 24, 26, 28), `16` = c(3, 5, 7, 9, 10, 12, 14, 15, 16, 17, 18, 20, 22, 24, 26, 28), `17` = c(3, 5, 7, 8, 9, 10, 12, 14, 15, 16, 17, 18, 20, 22, 24, 26, 28), `18` = c(3, 5, 7, 8, 9, 10, 12, 14, 15, 16, 17, 18, 20, 22, 24, 26, 27, 28), `19` = c(2, 4, 5, 7, 8, 9, 10, 12, 14, 15, 16, 17, 18, 20, 22, 24, 26, 27, 28), `20` = c(2, 4, 5, 7, 8, 9, 10, 11, 13, 14, 15, 16, 17, 18, 20, 22, 24, 26, 27, 28), `21` = c(2, 4, 5, 7, 8, 9, 10, 11, 13, 14, 15, 16, 17, 18, 19, 21, 23, 25, 26, 27, 28), `22` = c(2, 4, 5, 7, 8, 9, 10, 11, 13, 14, 15, 16, 17, 18, 19, 21, 23, 25, 26, 27, 28, 29), `23` = c(1, 2, 4, 5, 7, 8, 9, 10, 11, 13, 14, 15, 16, 17, 18, 19, 21, 23, 25, 26, 27, 28, 29))
	# from https://personal.sron.nl/~pault/data/tol_colors.py
	list(tol.bright = c('#4477AA', '#EE6677', '#228833', '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB', '#000000'),
		 tol.contrast = c('#004488', '#DDAA33', '#BB5566', '#000000'),
		 tol.vibrant = c('#EE7733', '#0077BB', '#33BBEE', '#EE3377', '#CC3311', '#009988', '#BBBBBB', '#000000'),
		 tol.muted = c('#CC6677', '#332288', '#DDCC77', '#117733', '#88CCEE', '#882255', '#44AA99', '#999933', '#AA4499', '#DDDDDD','#000000'),
		 tol.medium = c('#6699CC', '#004488', '#EECC66', '#994455', '#997700','#EE99AA', '#000000'),
		 tol.light = c('#77AADD', '#EE8866', '#EEDD88', '#FFAABB', '#99DDFF','#44BB99', '#BBCC33', '#AAAA00', '#DDDDDD', '#000000'),
		 tol.rainbow = structure(c('#E8ECFB', '#D9CCE3', '#D1BBD7', '#CAACCB', '#BA8DB4',
		 						  '#AE76A3', '#AA6F9E', '#994F88', '#882E72', '#1965B0',
		 						  '#437DBF', '#5289C7', '#6195CF', '#7BAFDE', '#4EB265',
		 						  '#90C987', '#CAE0AB', '#F7F056', '#F7CB45', '#F6C141',
		 						  '#F4A736', '#F1932D', '#EE8026', '#E8601C', '#E65518',
		 						  '#DC050C', '#A5170E', '#72190E', '#42150A'), index = rainbow_ids))


})

library(khroma)
# khroma:::.schemes
#
# tols = c("bright",
# "high contrast",
# "vibrant",
# "muted",
# "medium contrast",
# "pale",
# "dark",
# "light")
#
k = khroma:::.schemes[tols]
# cat_khroma_tol = lapply(k, function(ki) ki$colours)
# names(cat_khroma_tol) = paste0("khroma_", names(cat_khroma_tol))



cat_carto = local({
	require(rcartocolor)

	carto_pal(name="Vivid")
	cartoQual = cartocolors[cartocolors$Type == "qualitative",]

	cartoQual$n12

	indices = lapply(1:nrow(cartoQual), function(i) {
		structure(lapply(1:12, function(j) {
			if (j < 3) 1:j else match(cartoQual[[paste0("n", j)]][[i]], cartoQual$n12[[i]])
		}), names = as.character(1:12))
	})

	cat_carto = mapply(function(pal, ind) {
		structure(pal, index = ind)
	}, cartoQual$n12, indices, SIMPLIFY = FALSE)

	names(cat_carto) = paste0("carto.", tolower(cartoQual$Name))
	cat_carto
})

cat_hcl = local({
	hclnames = c("Pastel 1", "Dark 2", "Dark 3", "Set 2", "Set 3", "Warm", "Cold", "Harmonic", "Dynamic")
	hclnewnm = paste0("hcl.", c("pastel1", "dark2", "dark3", "set2", "set3", "warm", "cold", "harmonic", "dynamic"))
	structure(lapply(hclnames, function(h) {
		pals = lapply(1:20, function(i) {
			qualitative_hcl(palette = h, n = i)
		})
		pal = unique(unlist(pals))
		indices = structure(lapply(1:20, function(i) {
			match(pals[[i]], pal)
		}), names = as.character(1:20))
		structure(pal, index = indices)
	}), names = hclnewnm)
})


# cat_paletteer = local({
# 	require(paletteer)
#
# 	cat_paletteer = do.call(c, palettes_d)
# 	cat_paletteer = lapply(cat_paletteer, na.omit)
# 	cat_paletteer = lapply(cat_paletteer, function(pal) {
# 		if (any(nchar(pal) != 7)) {
# 			do.call(rgb, c(as.list(as.data.frame(t(col2rgb(pal)))), maxColorValue = 255))
# 		} else pal
# 	})
# 	names(cat_paletteer) = paste0("X.", names(cat_paletteer))
# 	cat_paletteer
# })

cat_light = list(
	light.martin = unname(colorBlindness::paletteMartin),
	light.paired = unname(colorBlindness::PairedColor12Steps)
)

# TODO
# pals stepped palettes: bivariate (cat x num)
# divering: X.colorBlindness.Blue2Orange12Steps and X.dichromat.BluetoOrange_12
#

z_cat = c(cat_grDevices, cat_pals, cat_tol, cat_carto, cat_hcl, cat_light) #cat_paletteer

series = sub("\\..*", "", names(z_cat))
series[series == names(z_cat)] = "other"

s_cat = get_scores(z_cat, 36)

nmax = sapply(z_cat, function(x) {
	index = attr(x, "index")
	if (is.null(index)) length(x) else length(index[[length(index)]])
})

m = data.frame(name = names(z_cat), series = series, nmax = nmax)

save(z_cat, s_cat, file="R/sysdata.rda", compress="xz")
