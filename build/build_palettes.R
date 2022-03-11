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
library(reticulate) # to get seaborn
library(Polychrome)

## more:

# library(paletteer)
# https://github.com/EmilHvitfeldt/r-color-palettes

c4a_series_remove(are.you.sure = TRUE)

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

	#p3 = pals[c("Alphabet", "Polychrome 36")]
	#s3 = "misc"

	c4a_series_add(p1, types = "cat", series = s1)
	#c4a_series_add(p2, types = "cat", series = s2c4a_series_add)
	#c4a_series_add(p3, types = "cat", series = s3, take.gray.for.NA = FALSE, remove.other.grays = FALSE, remove.blacks = FALSE)
	invisible(NULL)
})

###################################
### package grDevices: seq and div
###################################
local({

	# select palettes that are not approximations (those are added later on from the original sources)

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

	terrain = c("Terrain", "Terrain 2")

	seq = setdiff(hcl_lst[[1]], c(seq_hcl_approx, terrain))
	div = setdiff(c(hcl_lst[[2]],hcl_lst[[3]]), div_hcl_approx)


	spals = lapply(seq, function(s) hcl.colors(11, s))
	names(spals) = seq

	dpals = lapply(div, function(s) hcl.colors(11, s))
	names(dpals) = div

	names(dpals)[1] = "Blue-Red 1" #to prevent conflict with reversed "Red-Blue"

	tpals = lapply(terrain, function(s) hcl.colors(11, s))
	names(tpals) = c("terrain", "terrain2")

	type = c(rep("seq", length(spals)), rep("div", length(dpals)))

	c4a_series_add(c(spals, dpals), types = type, series = "hcl")
	c4a_series_add_as_is(tpals, types = "seq", series = "hcl")
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

	c4a_series_add(pals, types = types, series = "brewer")
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
		pu_gn = c('#762A83', '#9970AB', '#C2A5CF', '#E7D4E8', '#F7F7F7',
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

	p3_types = ifelse(names(p3) %in% c("bu_rd", "pu_gn", "sunset"), "div", "seq")

	c4a_series_add(p1, types = "cat", series = "tol")
	c4a_series_add(p2, types = "cat", series = "tol", take.gray.for.NA = FALSE, remove.other.grays = FALSE, remove.blacks = FALSE)
	c4a_series_add(p3, types = p3_types, xNA = p3_na, series = "tol")
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

	c4a_series_add(pals, types = types, series = "viridis")
})






###################################
### package pals
###################################

local({
	syspals = pals:::syspals
	palsCat = "watlington"
	palsNew = "watlington"
	series = "misc"

	pals = syspals[palsCat]
	names(pals) = palsNew

	pals4 = syspals[substr(names(syspals), 1, 6) == "kovesi" & substr(names(syspals), 1, 13) != "kovesi.cyclic"]

	isdiv = substr(names(pals4), 1, 16) == "kovesi.diverging"
	iscyc = substr(names(pals4), 1, 13) == "kovesi.cyclic"

	pals4_type = ifelse(isdiv, "div", ifelse(iscyc, "cyc", "seq"))

	names(pals4) = substr(names(pals4), 8, nchar(names(pals4)))

	orig = c("linear_grey_10_95_c0",
			 "rainbow_bgyr_35_85_c72",
			 "rainbow_bgyrm_35_85_c69",
			 "linear_ternary_blue_0_44_c57",
			 "linear_ternary_green_0_46_c42",
			 "linear_ternary_red_0_50_c52",
			 "linear_kry_5_95_c72",
			 "linear_kryw_5_100_c64",
			 "linear_green_5_95_c69",
			 "linear_bmy_10_95_c71",
			 "linear_bmw_5_95_c86",
			 "linear_blue_95_50_c20",
			 "linear_blue_5_95_c73",
			 "linear_bgyw_15_100_c67",
			 "linear_bgy_10_95_c74",
			 "isoluminant_cgo_70_c39")

	new = c("linear_grey",
			"rainbow_bu_rd",
			"rainbow_bu_pk",
			"linear_ternary_blue",
			"linear_ternary_green",
			"linear_ternary_red",
			"linear_yl_rd_bk",
			"linear_wh_rd_bk",
			"linear_green",
			"linear_yl_mg_bu",
			"linear_wh_mg_bu",
			"linear_blue",
			"linear_tq_bu",
			"linear_wh_yl_gn_bu",
			"linear_yl_gn_bu",
			"isoluminant_tq_or")

	ids = match(orig, names(pals4))
	pals4_sel = pals4[ids]
	pals4_type_sel = pals4_type[ids]
	names(pals4_sel) = new


	pals_ter = pals4["linear_gow_65_90_c35"]
	names(pals_ter) = "linear_terrain"



	orig_div = c("diverging_gwv_55_95_c39",
				 "diverging_bky_60_10_c30",
				 "diverging_bwr_40_95_c42",
				 "diverging_bwr_55_98_c37",
				 "diverging_linear_bjy_30_90_c45",
				 "diverging_bkr_55_10_c35",
				 "diverging_linear_bjr_30_55_c53",
				 "diverging_isoluminant_cjo_70_c25",
				 "diverging_rainbow_bgymr_45_85_c67",
				 "diverging_cwm_80_100_c22",
				 "diverging_isoluminant_cjm_75_c24",
				 "diverging_gwr_55_95_c38",
				 "diverging_gkr_60_10_c40")


	new_div = c("div_gn_wh_pu",
				"div_bu_bk_br",
				"div_bu_wh_rd",
				"div_bu_wh_rd2",
				"div_bu_gy_yl",
				"div_bu_bk_rd",
				"div_bu_gy_rd",
				"div_isoluminant_tq_or",
				"div_rainbow",
				"div_tq_wh_pk",
				"div_tq_gy_pk",
				"div_gn_wh_rd",
				"div_gn_bk_rd")

	ids = match(orig_div, names(pals4))
	pals5 = pals4[ids]
	pals5_type = pals4_type[ids]
	names(pals5) = new_div


	c4a_series_add(pals, types = "cat", series = series)
	c4a_series_add(pals4_sel, types = pals4_type_sel, series = "kovesi", format.palette.name = FALSE)
	c4a_series_add_as_is(pals_ter, types = "seq", series = "kovesi", format.palette.name = FALSE)
	c4a_series_add(pals5, types = pals5_type, series = "kovesi", format.palette.name = FALSE)

})



###################################
### package wesanderson
###################################


local({
	pals = wesanderson::wes_palettes

	names(pals)[c(18,19)] = c("isle_of_dogs1", "isle_of_dogs2")

	type = ifelse(names(pals) == "Zissou1", "div", "cat")

	c4a_series_add(pals, types = type, series = "wes")

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

	cartoNum = cartocolors[cartocolors$Type %in% c("quantitative", "diverging"), c("Name", "Type", "n7")]
	pals2 = cartoNum$n7
	names(pals2) = cartoNum$Name
	type = ifelse(cartoNum$Type == "diverging", "div", "seq")

	pals2rev = lapply(pals2, rev) # trick to reverse names
	pals2rev["SunsetDark"] = rev(pals2rev["SunsetDark"]) # another reverse to undo

	cartoAgg = cartocolors[cartocolors$Type %in% c("aggregation"), c("Name", "Type", "n7")]
	pals3 = cartoAgg$n7
	names(pals3) = tolower(cartoAgg$Name)
	names(pals3)[2] = "ag_grn_yl"

	c4a_series_add(pals, types = "cat", series = "carto")
	c4a_series_add(pals2rev, types = type, series = "carto")
	c4a_series_add(pals3, types = "seq", series = "carto", format.palette.name = FALSE)

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

	c4a_series_add(pals, types = "cat", series = "hcl")
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


	names(pals_seq)[match(c("batlowK", "batlowW"), names(pals_seq))] = c("k_batlow", "w_batlow") # reverse names (because palettes will be reversed)

	c4a_series_add(pals_div, types = "div", series = "scico")
	c4a_series_add(pals_seq, types = "seq", series = "scico")
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

	c4a_series_add(tab_cat, types = "cat", series = "tableau")
	c4a_series_add(tab_seq, types = "seq", series = "tableau")
	c4a_series_add(tab_div, types = "div", series = "tableau")
})



local({
	sns = import("seaborn")
	mpc = import("matplotlib.colors")

	sb_cat_names = c("deep", "muted", "pastel", "bright", "dark", "colorblind")
	sb_cat = lapply(sb_cat_names, sns$color_palette, as_cmap = TRUE)
	names(sb_cat) = sb_cat_names

	sb_seq_names = c("rocket", "mako", "flare", "crest")
	sb_seq = lapply(sb_seq_names, function(nm) {
		pal = sns$color_palette(nm)
		m = as.data.frame(t(sapply(0:(length(pal)-1), function(i) pal[[i]])))
		names(m) = c("red", "green", "blue")
		do.call(rgb, as.list(m))
	})
	names(sb_seq) = sb_seq_names

	sb_div_names = c("vlag", "icefire")
	sb_div = lapply(sb_div_names, function(nm) {
		pal = sns$color_palette(nm)
		m = as.data.frame(t(sapply(0:(length(pal)-1), function(i) pal[[i]])))
		names(m) = c("red", "green", "blue")
		do.call(rgb, as.list(m))
	})
	names(sb_div) = sb_div_names

	c4a_series_add(sb_cat, types = "cat", series = "seaborn")
	c4a_series_add(sb_seq, types = "seq", series = "seaborn")
	c4a_series_add(sb_div, types = "div", series = "seaborn")
})

local({
	p = list(kelly = Polychrome::kelly.colors(n = 22),
			 glasbey = Polychrome::glasbey.colors(n = 32),
			 alphabet2 = Polychrome::green.armytage.colors(n = 26),
			 wright25 = pals::cols25(),
			 palette36 = Polychrome::palette36.colors(n = 36),
			 alphabet = Polychrome::alphabet.colors(n = 26),
			 light24 = Polychrome::light.colors(n = 24),
			 dark24 = Polychrome::dark.colors(n = 24),
			 sky24 = Polychrome::sky.colors(n = 24))
	c4a_series_add(p, types = "cat", series = "poly", remove.blacks = FALSE, take.gray.for.NA = FALSE, remove.other.grays = FALSE)
})






## meta package: 2000 palettes,
# library(paletteer)
# str(palettes_d)

# library(paletteer)
# cat_paletteer = local({
#
# 	cat_paletteer = do.call(c, palettes_d)
# })
# paletteer::palettes_c_names





########################################################################################
######################################## BIVARIATE #####################################
########################################################################################

local({
	#c4a_palettes(type = "div")
	pals = c("hcl.purple_brown", "hcl.purple_green", "hcl.blue_red3", "hcl.red_green", "hcl.green_brown")

	b = .C4A$z[match(pals, .C4A$z$fullname), ]
	p = b$palette
	names(p) = paste(b$name, "_x")

	p = lapply(p, function(pal)pal[3:9])

	c4a_series_add(p, types = "biv", series = "hcl", biv.method = "fromdiv")

	pals2 = list(pinkgreen = pals::stevens.pinkgreen(n = 9),
				 bluered = pals::stevens.bluered(n = 9),
				 pinkblue = pals::stevens.pinkblue(n = 9),
				 greenblue = pals::stevens.greenblue(n = 9),
				 purplegold = pals::stevens.purplegold(n = 9))

	c4a_series_add(pals2, types = "biv", series = "stevens", biv.method = "bycol")

	pals3 = list(divseq = brewer.divseq(n = 9),
				 qualseq = brewer.qualseq(n = 9),
				 divdiv = brewer.divdiv(n = 9),
				 seqseq1 = brewer.seqseq1(n = 9),
				 seqseq2 = brewer.seqseq2(n = 9))
	c4a_series_add(pals3, types = "biv", series = "brewer", biv.method = "bycol")

})

.z = get("z", .C4A)
.s = get("s", .C4A)


save(.z, .s, file="R/sysdata.rda", compress="xz")

