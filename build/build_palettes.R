# general color tools
library(colorblindcheck)
library(colorspace)

# packages with colors
library(pals)
library(rcartocolor)
library(RColorBrewer)
library(viridisLite)
library(scico)
library(ggthemes)

## more:

# library(paletteer)
# https://github.com/EmilHvitfeldt/r-color-palettes


###################################
### package grDevices: cat
###################################
local({
	pals = grDevices:::.palette_colors_hex

	p1 = pals[c("R3", "R4", "ggplot2", "Okabe-Ito")]
	names(p1) = c("R3", "R4", "ggplot2", "okabe")
	s1 = "misc"

	# p2 = pals[c("Accent", "Dark 2", "Paired", "Pastel 1", "Pastel 2", "Set 1", "Set 2", "Set 3")]
	# s2 = "brewer"

	p3 = pals[c("Alphabet", "Polychrome 36")]
	s3 = "misc"

	c4a_submit_series(p1, types = "cat", series = s1, from.scratch = TRUE)
	#c4a_submit_series(p2, types = "cat", series = s2, from.scratch = FALSE)
	c4a_submit_series(p3, types = "cat", series = s3, from.scratch = FALSE, take_grey_for_NA = FALSE, remove_other_greys = FALSE, remove_blacks = FALSE)
	invisible(NULL)
})

###################################
### package grDevices: seq and div
###################################
local({

	brewer_seq = c("YlOrRd", "YlOrBr", "OrRd", "Oranges", "YlGn", "YlGnBu", "Reds", "RdPu", "PuRd", "Purples","PuBuGn", "PuBu", "Greens", "BuGn", "GnBu", "BuPu", "Blues")
	brewer_div <- c("Spectral", "RdYlGn", "RdYlBu", "RdGy", "RdBu", "PiYG", "PRGn", "PuOr", "BrBG")

	carto_seq = c("DarkMint", "Mint", "BluGrn", "Teal", "TealGrn", "Emrld", "BluYl", "ag_GrnYl", "Peach", "PinkYl", "Burg", "BurgYl", "RedOr", "OrYel", "Purp", "PurpOr", "Sunset", "Magenta", "SunsetDark", "ag_Sunset", "BrwnYl")
	carto_div <- c("ArmyRose", "Earth", "Fall", "Geyser", "TealRose", "Temps", "Tropic")

	viridis_seq = c("Viridis", "Plasma", "Inferno", "Rocket", "Mako")
	viridis_div = "Cividis"

	scico_seq = c("Oslo", "Lajolla", "Turku", "Hawaii", "Batlow")
	scico_div = c("Broc", "Cork", "Vik", "Berlin", "Lisbon", "Tofino", "Roma")

	wes_div = "Zissou 1"

	seq_hcl_approx = c(brewer_seq, carto_seq, viridis_seq, scico_seq)
	div_hcl_approx = c(brewer_div, carto_div, viridis_div, scico_div, wes_div)

	hcl_lst = lapply(c("sequential", "diverging", "divergingx"), hcl.pals)

	seq = setdiff(hcl_lst[[1]], seq_hcl_approx)
	div = setdiff(c(hcl_lst[[2]],hcl_lst[[3]]), div_hcl_approx)


	spals = lapply(seq, function(s) hcl.colors(11, s))
	names(spals) = seq

	dpals = lapply(div, function(s) hcl.colors(11, s))
	names(dpals) = div

	type = c(rep("seq", length(spals)), rep("div", length(dpals)))

	c4a_submit_series(c(spals, dpals), types = type, series = "hcl", from.scratch = FALSE)
})



###################################
### package RColorBrewer
###################################
local({
	inf = RColorBrewer::brewer.pal.info

	pals = lapply(1:nrow(inf), function(i) {
		RColorBrewer::brewer.pal(n = inf$maxcolors[i], name = rownames(inf)[i])
	})
	names(pals) = rownames(inf)
	types = ifelse(inf$category == "qual", "cat", inf$category)

	c4a_submit_series(pals, types = types, series = "brewer", from.scratch = FALSE)
})




###################################
### Tol: from python script (https://personal.sron.nl/~pault/) 2022-02-10
###################################


local({
	# Method:
	# * copy indices from tol_colors.py
	# * replace [ by list( and ] by )
	# * run:
	#     l2 = lapply(l, function(i) {unlist(i)+1L})
	#     names(l2) = sapply(l2, length)
	#     dput(l2)
	rainbow_ids = list(`1` = 10, `2` = c(10, 26), `3` = c(10, 18, 26), `4` = c(10, 15, 18, 26), `5` = c(10, 14, 15, 18, 26), `6` = c(10, 14, 15, 17, 18, 26), `7` = c(9, 10, 14, 15, 17, 18, 26), `8` = c(9, 10, 14, 15, 17, 18, 23, 26), `9` = c(9, 10, 14, 15, 17, 18, 23, 26, 28), `10` = c(9, 10, 14, 15, 17, 18, 21, 24, 26, 28), `11` = c(9, 10, 12, 14, 15, 17, 18, 21, 24, 26, 28), `12` = c(3, 6, 9, 10, 12, 14, 15, 17, 18, 21, 24, 26), `13` = c(3, 6, 9, 10, 12, 14, 15, 16, 17, 18, 21, 24, 26), `14` = c(3, 6, 9, 10, 12, 14, 15, 16, 17, 18, 20, 22, 24, 26), `15` = c(3, 6, 9, 10, 12, 14, 15, 16, 17, 18, 20, 22, 24, 26, 28), `16` = c(3, 5, 7, 9, 10, 12, 14, 15, 16, 17, 18, 20, 22, 24, 26, 28), `17` = c(3, 5, 7, 8, 9, 10, 12, 14, 15, 16, 17, 18, 20, 22, 24, 26, 28), `18` = c(3, 5, 7, 8, 9, 10, 12, 14, 15, 16, 17, 18, 20, 22, 24, 26, 27, 28), `19` = c(2, 4, 5, 7, 8, 9, 10, 12, 14, 15, 16, 17, 18, 20, 22, 24, 26, 27, 28), `20` = c(2, 4, 5, 7, 8, 9, 10, 11, 13, 14, 15, 16, 17, 18, 20, 22, 24, 26, 27, 28), `21` = c(2, 4, 5, 7, 8, 9, 10, 11, 13, 14, 15, 16, 17, 18, 19, 21, 23, 25, 26, 27, 28), `22` = c(2, 4, 5, 7, 8, 9, 10, 11, 13, 14, 15, 16, 17, 18, 19, 21, 23, 25, 26, 27, 28, 29), `23` = c(1, 2, 4, 5, 7, 8, 9, 10, 11, 13, 14, 15, 16, 17, 18, 19, 21, 23, 25, 26, 27, 28, 29))
	# from https://personal.sron.nl/~pault/data/tol_colors.py
	p1 = list(bright = c('#4477AA', '#EE6677', '#228833', '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB', '#000000'),
		 contrast = c('#004488', '#DDAA33', '#BB5566', '#000000'),
		 vibrant = c('#EE7733', '#0077BB', '#33BBEE', '#EE3377', '#CC3311', '#009988', '#BBBBBB', '#000000'),
		 muted = c('#CC6677', '#332288', '#DDCC77', '#117733', '#88CCEE', '#882255', '#44AA99', '#999933', '#AA4499', '#DDDDDD','#000000'),
		 medium = c('#6699CC', '#004488', '#EECC66', '#994455', '#997700','#EE99AA', '#000000'),
		 light = c('#77AADD', '#EE8866', '#EEDD88', '#FFAABB', '#99DDFF','#44BB99', '#BBCC33', '#AAAA00', '#DDDDDD', '#000000'))

	p2 = list(rainbow = structure(c('#E8ECFB', '#D9CCE3', '#D1BBD7', '#CAACCB', '#BA8DB4',
										 '#AE76A3', '#AA6F9E', '#994F88', '#882E72', '#1965B0',
										 '#437DBF', '#5289C7', '#6195CF', '#7BAFDE', '#4EB265',
										 '#90C987', '#CAE0AB', '#F7F056', '#F7CB45', '#F6C141',
										 '#F4A736', '#F1932D', '#EE8026', '#E8601C', '#E65518',
										 '#DC050C', '#A5170E', '#72190E', '#42150A'),
								index = rainbow_ids))


	p3 = list(
		sunset = c('#364B9A', '#4A7BB7', '#6EA6CD', '#98CAE1', '#C2E4EF',
					   '#EAECCC', '#FEDA8B', '#FDB366', '#F67E4B', '#DD3D2D',
					   '#A50026'),
		bu_rd = c('#2166AC', '#4393C3', '#92C5DE', '#D1E5F0', '#F7F7F7',
					  '#FDDBC7', '#F4A582', '#D6604D', '#B2182B'),
		pr_gn = c('#762A83', '#9970AB', '#C2A5CF', '#E7D4E8', '#F7F7F7',
					  '#D9F0D3', '#ACD39E', '#5AAE61', '#1B7837'),
		yl_or_br = c('#FFFFE5', '#FFF7BC', '#FEE391', '#FEC44F', '#FB9A29',
						 '#EC7014', '#CC4C02', '#993404', '#662506'),
		wh_or_br = c('#FFFFFF', '#FFF7BC', '#FEE391', '#FEC44F', '#FB9A29',
						 '#EC7014', '#CC4C02', '#993404', '#662506'),
		iridescent = c('#FEFBE9', '#FCF7D5', '#F5F3C1', '#EAF0B5', '#DDECBF',
						   '#D0E7CA', '#C2E3D2', '#B5DDD8', '#A8D8DC', '#9BD2E1',
						   '#8DCBE4', '#81C4E7', '#7BBCE7', '#7EB2E4', '#88A5DD',
						   '#9398D2', '#9B8AC4', '#9D7DB2', '#9A709E', '#906388',
						   '#805770', '#684957', '#46353A'),
		rainbow_pu_rd = c('#6F4C9B', '#6059A9', '#5568B8', '#4E79C5', '#4D8AC6',
							  '#4E96BC', '#549EB3', '#59A5A9', '#60AB9E', '#69B190',
							  '#77B77D', '#8CBC68', '#A6BE54', '#BEBC48', '#D1B541',
							  '#DDAA3C', '#E49C39', '#E78C35', '#E67932', '#E4632D',
							  '#DF4828', '#DA2222'),
		rainbow_pu_br = c('#6F4C9B', '#6059A9', '#5568B8', '#4E79C5', '#4D8AC6',
							  '#4E96BC', '#549EB3', '#59A5A9', '#60AB9E', '#69B190',
							  '#77B77D', '#8CBC68', '#A6BE54', '#BEBC48', '#D1B541',
							  '#DDAA3C', '#E49C39', '#E78C35', '#E67932', '#E4632D',
							  '#DF4828', '#DA2222', '#B8221E', '#95211B', '#721E17',
							  '#521A13'),
		rainbow_wh_rd = c('#E8ECFB', '#DDD8EF', '#D1C1E1', '#C3A8D1', '#B58FC2',
							  '#A778B4', '#9B62A7', '#8C4E99', '#6F4C9B', '#6059A9',
							  '#5568B8', '#4E79C5', '#4D8AC6', '#4E96BC', '#549EB3',
							  '#59A5A9', '#60AB9E', '#69B190', '#77B77D', '#8CBC68',
							  '#A6BE54', '#BEBC48', '#D1B541', '#DDAA3C', '#E49C39',
							  '#E78C35', '#E67932', '#E4632D', '#DF4828', '#DA2222'),
		rainbow_wh_br = c('#E8ECFB', '#DDD8EF', '#D1C1E1', '#C3A8D1', '#B58FC2',
							  '#A778B4', '#9B62A7', '#8C4E99', '#6F4C9B', '#6059A9',
							  '#5568B8', '#4E79C5', '#4D8AC6', '#4E96BC', '#549EB3',
							  '#59A5A9', '#60AB9E', '#69B190', '#77B77D', '#8CBC68',
							  '#A6BE54', '#BEBC48', '#D1B541', '#DDAA3C', '#E49C39',
							  '#E78C35', '#E67932', '#E4632D', '#DF4828', '#DA2222',
							  '#B8221E', '#95211B', '#721E17', '#521A13'))

	p3_na = c("#FFFFFF", "#FFEE99", "#FFEE99", "#888888", "#888888", "#999999", "#FFFFFF", "#FFFFFF", "#666666", "#666666")

	p3_types = ifelse(names(p3) %in% c("bu_rd", "pr_gn", "sunset"), "div", "seq")

	c4a_submit_series(p1, types = "cat", series = "tol", from.scratch = FALSE)
	c4a_submit_series(p2, types = "cat", series = "tol", from.scratch = FALSE, take_grey_for_NA = FALSE, remove_other_greys = FALSE, remove_blacks = FALSE)
	c4a_submit_series(p3, types = p3_types, xNA = p3_na, series = "tol", from.scratch = FALSE)
})






###################################
### package viridisLite
###################################

local({
	nms = c("magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo")
	types = ifelse(nms == "cividis", "div", "seq")

	pals = lapply(nms, function(nm) {
		viridisLite::viridis(11, option = nm)
	})
	names(pals) = nms

	c4a_submit_series(pals, types = types, series = "viridis", from.scratch = FALSE)
})






###################################
### package pals
###################################

local({
	syspals = pals:::syspals
	palsCat = c("kelly", "watlington")
	palsNew = c("kelly", "watlington")
	series = "misc"

	pals = syspals[palsCat]
	names(pals) = palsNew

	pals3 = list(cols25 = pals::cols25(),
				 glasbey = pals::glasbey(32),
				 alphabet2 = pals::alphabet()) # pals::alphabet2 = base "Alphabet"

	pals4 = syspals[substr(names(syspals), 1, 6) == "kovesi" & substr(names(syspals), 1, 13) != "kovesi.cyclic"]

	isdiv = substr(names(pals4), 1, 16) == "kovesi.diverging"
	iscyc = substr(names(pals4), 1, 13) == "kovesi.cyclic"

	pals4_type = ifelse(isdiv, "div", ifelse(iscyc, "cyc", "seq"))

	names(pals4) = substr(names(pals4), 8, nchar(names(pals4)))
	c4a_submit_series(pals, types = "cat", series = series, from.scratch = FALSE)
	c4a_submit_series_as_is(pals3, types = "cat", series = "misc", from.scratch = FALSE)
	c4a_submit_series(pals4, types = pals4_type, series = "kovesi", from.scratch = FALSE, format.palette.name = FALSE)


	# pals = lapply(palsCat, function(p) {
	# 	if (p %in% names(pals_syspals)) {
	# 		pal = pals_syspals[[p]]
	# 		if (p != "alphabet") {
	# 			if (is.list(pal)) {
	# 				pal = lapply(pal, remove_black_white)
	# 			} else {
	# 				pal = remove_black_white(pal)
	# 			}
	# 		}
	# 	} else {
	# 		pal = unname(do.call(p, args = list()))
	# 		if (p != "alphabet") pal = remove_black_white(pal)
	# 	}
	# 	pal
	# })
	# pals = lapply(pals, unname)
	# names(pals) = palsNew
	# pals$kelly = c(pals$kelly[-1], pals$kelly[1]) # put black at the end
	# pals
})



###################################
### package wesanderson
###################################


local({
	pals = wesanderson::wes_palettes

	names(pals)[c(18,19)] = c("isle_of_dogs1", "isle_of_dogs2")

	type = ifelse(names(pals) == "Zissou1", "div", "cat")

	c4a_submit_series(pals, types = type, series = "wes", from.scratch = FALSE)

})



###################################
### package rcartocolor
###################################

local({
	cartoQual = cartocolors[cartocolors$Type == "qualitative",]

	indices = lapply(1:nrow(cartoQual), function(i) {
		structure(lapply(1:12, function(j) {
			if (j < 3) 1:j else match(cartoQual[[paste0("n", j)]][[i]], cartoQual$n12[[i]])
		}), names = as.character(1:12))
	})

	pals = mapply(function(pal, ind) {
		structure(pal, index = ind)
	}, cartoQual$n12, indices, SIMPLIFY = FALSE)

	names(pals) = cartoQual$Name

	cartoNum = cartocolors[cartocolors$Type %in% c("quantitative", "aggregation", "diverging"), c("Name", "Type", "n7")]
	pals2 = cartoNum$n7
	names(pals2) = cartoNum$Name
	type = ifelse(cartoNum$Type == "diverging", "div", "seq")

	c4a_submit_series(pals, types = "cat", series = "carto", from.scratch = FALSE)
	c4a_submit_series(pals2, types = type, series = "carto", from.scratch = FALSE)

})




###################################
### package colorspace
###################################

local({
	hclnames = c("Pastel 1", "Dark 2", "Dark 3", "Set 2", "Set 3", "Warm", "Cold", "Harmonic", "Dynamic")
	pals = structure(lapply(hclnames, function(h) {
		pals = lapply(1:36, function(i) {
			qualitative_hcl(palette = h, n = i)
		})
		pal = unique(unlist(pals))
		indices = structure(lapply(1:36, function(i) {
			match(pals[[i]], pal)
		}), names = as.character(1:36))
		structure(pal, index = indices)
	}), names = hclnames)

	c4a_submit_series(pals, types = "cat", series = "hcl", from.scratch = FALSE)
})

###################################
### package scico
###################################

local({
	ids = seq(1,256, length.out=16)
	d = scico:::palettes
	pals = lapply(d, function(x) {
		colorRampPalette(rgb(x$r, x$g, x$b, maxColorValue = 1), space = "Lab")(15)
	})
	div = c("broc", "brocO", "cork", "corkO", "vik", "vikO", "lisbon", "tofino", "berlin", "roma", "romaO", "bam", "bamO", "vanimo")
	mseq = c("oleron", "bukavu", "fes")

	pals_div = pals[div]
	pals_seq = pals[setdiff(names(pals), c(div, mseq))]

	c4a_submit_series(pals_div, types = "div", series = "scico", from.scratch = FALSE)
	c4a_submit_series(pals_seq, types = "seq", series = "scico", from.scratch = FALSE)
})


###################################
### package ggthemes (tableau)
###################################

local({
	palettes = ggthemes_data[["tableau"]][["color-palettes"]][["regular"]]
	tab_cat = lapply(palettes, function(pal) {
		pal$value
	})
	names(tab_cat)[1:2] = c("10", "20")


	palettes2 = ggthemes_data[["tableau"]][["color-palettes"]][["ordered-sequential"]]
	tab_seq = lapply(palettes2, function(pal) {
		pal$value
	})

	palettes3 = ggthemes_data[["tableau"]][["color-palettes"]][["ordered-diverging"]]
	tab_div = lapply(palettes3, function(pal) {
		pal$value
	})

	c4a_submit_series(tab_cat, types = "cat", series = "tableau", from.scratch = FALSE)
	c4a_submit_series(tab_seq, types = "seq", series = "tableau", from.scratch = FALSE)
	c4a_submit_series(tab_div, types = "div", series = "tableau", from.scratch = FALSE)
})



## meta package: 2000 palettes,
# library(paletteer)
# str(palettes_d)

# library(paletteer)
# cat_paletteer = local({
#
# 	cat_paletteer = do.call(c, palettes_d)
# })



.z = get(".z", .C4A_CACHE)
.s = get(".s", .C4A_CACHE)

# saveRDS(.z, "z.rds")
# saveRDS(.z, "s.rds")

#
# .z = readRDS("z.rds")
# .s = readRDS("s.rds")

save(.z, .s, file="R/sysdata.rda", compress="xz")

