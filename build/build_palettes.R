# general color tools
library(colorblindcheck)
library(colorspace)

# packages with colors
library(pals)
library(rcartocolor)
library(RColorBrewer)
library(viridisLite)
library(khroma) # library(scico)
library(ggthemes)
library(reticulate) # to get seaborn
library(Polychrome)
library(MetBrewer)

sessioninfo::session_info(pkgs = "attached")
# colorblindcheck * 1.0.2   2023-05-13 [1] CRAN (R 4.4.0)
# colorspace      * 2.1-1   2024-07-26 [1] CRAN (R 4.4.0)
# P cols4all        * 0.7-2   2024-08-23 [?] load_all()
# ggthemes        * 5.1.0   2024-02-10 [1] CRAN (R 4.4.0)
# khroma          * 1.14.0  2024-08-26 [1] CRAN (R 4.4.1)
# MetBrewer       * 0.2.0   2022-03-21 [1] CRAN (R 4.4.0)
# pals            * 1.9     2024-07-16 [1] CRAN (R 4.4.0)
# Polychrome      * 1.5.1   2022-05-03 [1] CRAN (R 4.4.0)
# rcartocolor     * 2.1.1   2023-05-13 [1] CRAN (R 4.4.0)
# RColorBrewer    * 1.1-3   2022-04-03 [1] CRAN (R 4.4.0)
# reticulate      * 1.38.0  2024-06-19 [1] CRAN (R 4.4.0)
# shiny           * 1.8.1.1 2024-04-02 [1] CRAN (R 4.4.0)
# viridisLite     * 0.4.2   2023-05-02 [1] CRAN (R 4.4.0)


c4a_sysdata_remove(are.you.sure = TRUE)

###################################
### package grDevices: cat
###################################
local({
	pals = grDevices:::.palette_colors_hex

	p1 = pals[c("R3", "R4", "ggplot2", "Okabe-Ito")]
	names(p1) = c("R3", "R4", "ggplot2", "okabe")

	c4a_load(c4a_data(p1, types = "cat", series = "miscs"))
})

###################################
### package grDevices: seq and div
###################################
local({
	seq = c("Grays", "Light Grays", "Blues 2", "Blues 3", "Purples 2",
			"Purples 3", "Reds 2", "Reds 3", "Greens 2", "Greens 3", "Purple-Blue",
			"Red-Purple", "Red-Blue", "Purple-Orange", "Purple-Yellow", "Blue-Yellow",
			"Green-Yellow", "Red-Yellow", "Heat", "Heat 2", "Dark Mint")

	div = c("Blue-Red", "Blue-Red 2", "Blue-Red 3", "Red-Green", "Purple-Green", "Purple-Brown",
			"Green-Brown", "Blue-Yellow 2", "Blue-Yellow 3", "Green-Orange", "Cyan-Magenta")

	terrain = c("Terrain", "Terrain 2")

	spals = lapply(seq, function(s) hcl.colors(11, s))
	names(spals) = seq

	dpals = lapply(div, function(s) hcl.colors(11, s))
	names(dpals) = div

	names(dpals)[1] = "Blue-Red 1" #to prevent conflict with reversed "Red-Blue"

	tpals = lapply(terrain, function(s) hcl.colors(11, s))
	names(tpals) = c("terrain", "terrain2")

	type = c(rep("seq", length(spals)), rep("div", length(dpals)))

	c4a_load(c4a_data(c(spals, dpals), types = type, series = "hcl", space = "Lab"))
	c4a_load(c4a_data_as_is(tpals, types = "seq", series = "hcl", space = "Lab"))
})



###################################
### package RColorBrewer
###################################
local({
	inf = RColorBrewer::brewer.pal.info

	pals = lapply(1:nrow(inf), function(i) {
		if (inf$category[i] != "qual") {
			xs = lapply(3:inf$maxcolors[i], function(j) {
				RColorBrewer::brewer.pal(n = j, name = rownames(inf)[i])
			})
			x = unique(unlist(rev(xs)))

			index = lapply(xs, function(xi) {
				match(xi, x)
			})
			names(index) = 3:inf$maxcolors[i]
			attr(x, "index") = 	index
		} else {
			x = RColorBrewer::brewer.pal(n = inf$maxcolors[i], name = rownames(inf)[i])

		}
		x
	})
	names(pals) = rownames(inf)
	types = ifelse(inf$category == "qual", "cat", inf$category)

	c4a_load(c4a_data(pals, types = types, series = "brewer"))
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
	p1 = list(bright = c('#4477AA', '#EE6677', '#228833', '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB'),
		 contrast = c('#004488', '#DDAA33', '#BB5566'),
		 vibrant = c('#EE7733', '#0077BB', '#33BBEE', '#EE3377', '#CC3311', '#009988', '#BBBBBB'),
		 muted = c('#CC6677', '#332288', '#DDCC77', '#117733', '#88CCEE', '#882255', '#44AA99', '#999933', '#AA4499', '#DDDDDD'),
		 medium = c('#6699CC', '#004488', '#EECC66', '#994455', '#997700','#EE99AA'),
		 light = c('#77AADD', '#EE8866', '#EEDD88', '#FFAABB', '#99DDFF','#44BB99', '#BBCC33', '#AAAA00', '#DDDDDD'),
		 dark = c("#222255", "#225555", "#225522", "#666633", "#663333", "#555555"
		 ))

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
		nightfall = c("#125A56", "#00767B", "#238F9D", "#42A7C6", "#60BCE9",
					  "#9DCCEF", "#C6DBED", "#DEE6E7", "#ECEADA", "#F0E6B2", "#F9D576",
					  "#FFB954", "#FD9A44", "#F57634", "#E94C1F", "#D11807", "#A01813"
		),
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
		incandescent = c("#CEFFFF", "#C6F7D6", "#A2F49B", "#BBE453", "#D5CE04",
		  "#E7B503", "#F19903", "#F6790B", "#F94902", "#E40515", "#A80003"
		),
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

	p3_na = c("#FFFFFF", "#FFEE99", "#FFEE99", "#888888", "#888888", "#999999", "#888888", "#FFFFFF", "#FFFFFF", "#666666", "#666666")

	p3_types = ifelse(names(p3) %in% c("bu_rd", "pu_gn", "sunset", "nightfall"), "div", "seq")

	c4a_load(c4a_data(p1, types = "cat", series = "tol"))
	c4a_load(c4a_data(p2, types = "cat", series = "tol", xNA = "#777777", take.gray.for.NA = FALSE, remove.other.grays = FALSE, remove.blacks = FALSE))
	c4a_load(c4a_data(p3, types = p3_types, xNA = p3_na, series = "tol"))
})


###################################
### Just checking: tol via khroma
###################################

if (FALSE) {
local({
	library(khroma)
	tol_cat = c("bright", "high contrast", "vibrant", "muted", "medium contrast", "pale", "dark", "light")
	tol_cat2 = c("bright", "contrast", "vibrant", "muted", "medium", "pale", "dark", "light")
	tol_cat_n = c(7, 3, 7, 9, 6, 6, 6, 9)

	c1 = mapply(function(m, n) {
		unname(khroma::color(m)(n))
	}, tol_cat, tol_cat_n, SIMPLIFY = FALSE)
	names(c1) = tol_cat2

	p1 = lapply(c1, function(ci) {
		v = unclass(ci)
		attributes(v) = NULL
		v
	})
	na1 = lapply(c1, function(ci) {
		attr(ci, "missing")
	})

	rainbow = lapply(1:23, function(i) {
		x = khroma::color("discreterainbow")(i)
		v = unclass(x)
		attributes(v) = NULL
		v
	})

	rainbow2 = lapply(1:34, function(i) {
		x = khroma::color("smoothrainbow")(i)
		v = unclass(x)
		attributes(v) = NULL
		v
	})

	generate_indices = function(palette_to_n) {
		k = length(palette_to_n)
		ks = sapply(palette_to_n, length)
		pal = unique(unlist(rev(palette_to_n)))
		indices = structure(lapply(1:k, function(i) {
			match(palette_to_n[[i]], pal)
		}), names = as.character(ks))
		structure(pal, index = indices)

	}

	rainbowI = generate_indices(rainbow)
	rainbowI2 = generate_indices(rainbow2)

	div = list(sunset = khroma::colour("sunset")(11),
			   nightfall = khroma::colour("nightfall")(17),
			   bu_rd = khroma::colour("BuRd")(9),
			   pu_gn = khroma::colour("PRGn")(9))

	div_na = sapply(div, function(x) {
		attr(x, "missing")
	})

	div_lst = lapply(div, function(d) {
		v = unclass(d)
		attributes(v) = NULL
		v
	})

	seq = list(yl_or_br = khroma::colour("YlOrBr")(9),
			   iridescent = khroma::colour("iridescent")(23),
			   incandescent = khroma::colour("incandescent")(11)
	)

	seq_na = sapply(seq, function(x) {
		attr(x, "missing")
	})

	seq_lst = lapply(seq, function(d) {
		v = unclass(d)
		attributes(v) = NULL
		v
	})



	c4a_load(c4a_data(p1, types = "cat", series = "tol2", xNA = unlist(na1)))
	c4a_load(c4a_data(list(rainbow = rainbowI, rainbow_smooth = rainbowI2), types = c("cat", "seq"), series = "tol2", xNA = c("#777777", "#666666")))
	c4a_load(c4a_data(seq_lst, types = "seq", series = "tol2", xNA = seq_na))
	c4a_load(c4a_data(div_lst, types = "div", series = "tol2", xNA = div_na))
})
}

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

	c4a_load(c4a_data(pals, types = types, series = "viridis"))
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


	c4a_load(c4a_data(pals, types = "cat", series = series))
	c4a_load(c4a_data(pals4_sel, types = pals4_type_sel, series = "kovesi", format.palette.name = FALSE))
	c4a_load(c4a_data_as_is(pals_ter, types = "seq", series = "kovesi", format.palette.name = FALSE))
	c4a_load(c4a_data(pals5, types = pals5_type, series = "kovesi", format.palette.name = FALSE))

})



###################################
### package wesanderson
###################################


local({
	pals = wesanderson::wes_palettes

	names(pals)[c(18,19)] = c("isle_of_dogs1", "isle_of_dogs2")

	type = ifelse(names(pals) == "Zissou1", "div", "cat")

	c4a_load(c4a_data(pals, types = type, series = "wes"))

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

	c4a_load(c4a_data(pals, types = "cat", series = "carto"))
	c4a_load(c4a_data(pals2rev, types = type, series = "carto"))
	c4a_load(c4a_data(pals3, types = "seq", series = "carto", format.palette.name = FALSE))

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
		pal = unique(unlist(rev(pals)))
		indices = structure(lapply(1:36, function(i) {
			match(pals[[i]], pal)
		}), names = as.character(1:36))
		structure(pal, index = indices)
	}), names = hclnames)

	c4a_load(c4a_data(pals, types = "cat", series = "hcl"))
})

###################################
### package scico
###################################

local({
	ids = seq(1,256, length.out=16)
	d = scico:::palettes

	d$.categorical = NULL

	div = c("broc","cork", "vik", "lisbon", "tofino", "berlin", "bam", "roma", "vanimo", "managua")
	mseq = c("oleron", "bukavu", "fes")
	cyc = c( "brocO", "corkO",  "vikO", "romaO", "bamO")

	sq = setdiff(names(d), c(div, mseq, cyc))

	pals = mapply(function(x, nm) {
		if (nm %in% mseq) {
			c(rampPal(rgb(x$r[128:1], x$g[128:1], x$b[128:1], maxColorValue = 1), 7),
			rampPal(rgb(x$r[256:129], x$g[256:129], x$b[256:129], maxColorValue = 1), 7))
		} else {
			rampPal(rgb(x$r, x$g, x$b, maxColorValue = 1), 15)
		}
	}, d, names(d))

	pals_cat = lapply(scico:::palettes$.categorical, head, 10)
	names(pals_cat) = paste0(names(pals_cat), "_cat")

	pals_div = pals[div]
	pals_seq = pals[sq]
	pals_biv = pals[mseq]
	pals_biv = lapply(pals_biv, function(p) {
		matrix(p[c(1:7,8:14)], ncol = 2)
	})
	pals_biv[["fes"]] = pals_biv[["fes"]][,2:1]

	names(pals_seq)[match(c("batlowK", "batlowW", "grayC"), names(pals_seq))] = c("k_batlow", "w_batlow", "c_gray") # reverse names (because palettes will be reversed)

	c4a_load(c4a_data_as_is(pals_cat, types = "cat", series = "scico"), overwrite = T)
	c4a_load(c4a_data(pals_div, types = "div", series = "scico"))
	c4a_load(c4a_data(pals_seq, types = "seq", series = "scico"))
	c4a_load(c4a_data(pals_biv, types = c("bivc", "bivc", "bivg"), series = "scico", biv.method = "bycol2"))
})

###################################
### package crameri
###################################
# local({
# 	inf = khroma::info()[1:35, ]
#
# 	scms = khroma:::.schemes
#
# 	names = names(scms)
# 	types = unname(sapply(scms, "[[", "type"))
# 	pals = unname(lapply(scms, "[[", "colours"))
# 	nas = sapply(scms, "[[", missing)
# 	schemes = unname(lapply(scms, "[[", "scheme"))
#
# 	df = data.table::rbindlist(scms)
#
#
# 	cols = mapply(function(name, mx) {
# 		khroma:::.schemes[[name]] r(name, mx)
# 	}, inf$palette, inf$max, SIMPLIFY = FALSE, USE.NAMES = FALSE)
# })

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

	# 7th color is off (magenta instead of gray) do to typo:-) #colorRampPalette(tab_seq[["Gray Warm"]][c(6,8)], space = "rgb")(3)
	tab_seq[["Gray Warm"]][7] = "#b0a7a4"
	palettes3 = ggthemes_data[["tableau"]][["color-palettes"]][["ordered-diverging"]]
	tab_div = lapply(palettes3, function(pal) {
		pal$value
	})

	c4a_load(c4a_data(tab_cat, types = "cat", series = "tableau"))
	c4a_load(c4a_data(tab_seq, types = "seq", series = "tableau"))
	c4a_load(c4a_data(tab_div, types = "div", series = "tableau"))
})



local({
	library(reticulate)

	sns = import("seaborn")
	mpc = import("matplotlib.colors")

	sb_cat_names = c("deep", "muted", "pastel", "bright", "dark", "colorblind")
	sb_cat = lapply(sb_cat_names, sns$color_palette, as_cmap = TRUE)
	names(sb_cat) = sb_cat_names

	sb_seq_names = c("rocket", "mako", "flare", "crest")
	sb_seq = lapply(sb_seq_names, function(nm) {
		pal = sns$color_palette(nm)
		m = as.data.frame(t(sapply(1:(length(pal)), function(i) pal[[i-1]])))
		names(m) = c("red", "green", "blue")
		do.call(rgb, as.list(m))
	})
	names(sb_seq) = sb_seq_names

	sb_div_names = c("vlag", "icefire")
	sb_div = lapply(sb_div_names, function(nm) {
		pal = sns$color_palette(nm)
		m = as.data.frame(t(sapply(1:(length(pal)), function(i) pal[[i-1]])))
		names(m) = c("red", "green", "blue")
		do.call(rgb, as.list(m))
	})
	names(sb_div) = sb_div_names

	c4a_load(c4a_data(sb_cat, types = "cat", series = "seaborn"))
	c4a_load(c4a_data(sb_seq, types = "seq", series = "seaborn"))
	c4a_load(c4a_data(sb_div, types = "div", series = "seaborn"))
})

local({
	p = list(kelly = Polychrome::kelly.colors(n = 22),
			 glasbey = Polychrome::glasbey.colors(n = 32),
			 alphabet2 = Polychrome::green.armytage.colors(n = 26),
			 palette36 = Polychrome::palette36.colors(n = 36),
			 alphabet = Polychrome::alphabet.colors(n = 26),
			 light24 = Polychrome::light.colors(n = 24),
			 dark24 = Polychrome::dark.colors(n = 24),
			 sky24 = Polychrome::sky.colors(n = 24),
			 wright25 = pals::cols25())
	c4a_load(c4a_data(p, types = "cat", series = "poly", remove.blacks = FALSE, take.gray.for.NA = FALSE, remove.other.grays = FALSE))
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

## See also:

# https://github.com/EmilHvitfeldt/r-color-palettes



########################################################################################
######################################## BIVARIATE #####################################
########################################################################################
#c4a_palettes_remove(series = "c4a")
local({
	bu2 = c4a("hcl.blues3", n = 5, range = c(0.3, 0.8))
	yl_rd = c4a("hcl.yellow_red", n = 5, range = c(0.3, 0.8))
	pg = hcl.colors(11, "Purple-Green")
	bu = hcl.colors(9, "Blues 3")[7:3]
	gn = hcl.colors(9, "Greens 3")[7:3]
	pu = hcl.colors(9, "Purples 3")[7:3]

	# extract average c and l values for both wings
	mat = get_hcl_matrix(pg)
	cs = (mat[1:6, 2] + mat[11:6, 2]) / 2
	cs[6] = 0
	cs[5] = 25
	ls = (mat[1:6, 3] + mat[11:6, 3]) / 2

	ls2 = ls
	ls2[6] = 92


	# candidate hues
	hs = seq(0, 359, by = 0.2)

	# find hues for which colors have most chroma (the higher the better to distinguisch with grey)
	res = lapply(1:5, function(i) {
		sapply(hs, function(h) hcl(h=h,c=cs[i], l =ls[i]))
	})
	get_chroma = function(x) attr(as(colorspace::hex2RGB(x), "polarLAB"), "coords")[, "C"]
	max_chroma_cvd = function(x) {
		cr1 = deutan(x) |> get_chroma()
		cr2 = protan(x) |> get_chroma()
		cr3 = tritan(x) |> get_chroma()
		y = pmin(cr1, cr2, cr3)
		y = y / max(y)
	}
	res2 = lapply(res, max_chroma_cvd)
	res3 = rowSums(do.call(cbind, res2))
	plot(res3, pch=16)

	hue_br = hs[1:500][which.max(res3[1:500])]
	hue_gn = hs[500:800][which.max(res3[500:800])]
	hue_bu = hs[1000:1300][which.max(res3[1000:1300])]
	hue_pu = hs[1300:1800][which.max(res3[1300:1800])]

	bu_br_div = c(hcl(hue_bu, c = cs, l = ls2),
				  rev(hcl(hue_br, c = cs, l = ls2))[-1])

	pu_gn_div = c(hcl(hue_pu, c = cs, l = ls2),
				  rev(hcl(hue_gn, c = cs, l = ls2))[-1])

	bu_br_biv = c(hcl(hue_bu, c = cs, l = ls),
				  rev(hcl(hue_br, c = cs, l = ls))[-1])

	pu_gn_biv = c(hcl(hue_pu, c = cs, l = ls),
				  rev(hcl(hue_gn, c = cs, l = ls))[-1])
	# pu_gn_div |> specplot()
	# pg |> specplot()
	#pu_gn_div = pg # very similar but still a bit better

	pals_div = list(bu_br_div = bu_br_div, pu_gn_div = pu_gn_div)
	pals_bivs = list(bu_br_bivs = bu_br_biv[3:9], pu_gn_bivs = pu_gn_biv[3:9])
	pals_bivd = list(bu_br_bivd = bu_br_biv[2:10], pu_gn_bivd = pu_gn_biv[2:10])
	pals_bivg = list(bu_bivg = bu2, yl_rd_bivg = yl_rd, br_bivg = bu_br_biv[8:10], pu_bivg = pu_gn_biv[4:2], gn_bivg = pu_gn_biv[8:10])


	c4a_load(c4a_data(pals_div, types = "div", series = "c4a", space = "rgb"))
	c4a_load(c4a_data(pals_bivs, types = "bivs", series = "c4a", biv.method = "div2seqseq", space = "rgb"))
	c4a_load(c4a_data(pals_bivd, types = "bivd", series = "c4a", biv.method = "div2catseq", space = "rgb"))
	c4a_load(c4a_data(pals_bivg, types = "bivg", series = "c4a", biv.method = "seq2uncseq", space = "rgb"))

	pals2 = list(pinkgreen = pals::stevens.pinkgreen(n = 9),
				 bluered = pals::stevens.bluered(n = 9),
				 pinkblue = pals::stevens.pinkblue(n = 9),
				 greenblue = pals::stevens.greenblue(n = 9),
				 purplegold = pals::stevens.purplegold(n = 9))



	c4a_load(c4a_data(pals2, types = "bivs", series = "stevens", biv.method = "byrow"))

	pals3 = list(divseq = brewer.divseq(n = 9),
				 qualseq = brewer.qualseq(n = 9),
				 seqseq1 = brewer.seqseq1(n = 9),
				 seqseq2 = brewer.seqseq2(n = 9))
	c4a_load(c4a_data(pals3, types = c("bivd", "bivc", "bivs", "bivs"), series = "brewer", biv.method = "byrow"))


	pals4 = list(stepped = do.call(c, lapply(1:6, function(i) pals::stepped()[(i*4):(i*4-3)])),
				 stepped2 = do.call(c, lapply(1:5, function(i) pals::stepped2()[(i*4):(i*4-3)])),
				 stepped3 = do.call(c, lapply(1:5, function(i) pals::stepped3()[(i*4):(i*4-3)])))
	c4a_load(c4a_data(pals4[1], types = "bivc", series = "misc", biv.method = "bycol6"))
	c4a_load(c4a_data(pals4[2:3], types = "bivc", series = "misc", biv.method = "bycol5"))
})


local({
	pals = structure(lapply(MetBrewer::MetPalettes, "[[", 1), names = names(MetBrewer::MetPalettes))
	cbf = which(sapply(MetBrewer::MetPalettes, "[[", 3))
	seq = c("Greek", "Hokusai1", "Hokusai2", "Hokusai3", "Manet", "OKeeffe2", "Tam", "VanGogh3")
	div = c("Benedictus", "Cassatt1", "Cassatt2", "Demuth", "Hiroshige", "Homer1", "Homer2", "Ingres", "Isfahan1", "Johnson", "Morgenstern", "OKeeffe1", "Paquin", "Troy")

	bivc = "Monet"

	pals$Monet = pals$Monet[c(3:1, 4:6, 9:7)]

	types = ifelse(names(pals) %in% seq, "seq", ifelse(names(pals) %in% div, "div", ifelse(names(pals) %in% bivc, "bivc", "cat")))

	#pals_cbf = pals[cbf]
	#types_cbf = types[cbf]

	c4a_load(c4a_data(pals, types = types, series = "met", biv.method = "bycol3"))
})

local({
	library(NatParksPalettes)

	seq = c("Arches2", "CapitolReef", "Denali", "Glacier", "WindCave")
	div = c("Acadia", "Arches", "Olympic")


	pals = structure(lapply(NatParksPalettes::NatParksPalettes, "[[", 1), names = names(NatParksPalettes::NatParksPalettes))

	types = ifelse(names(pals) %in% seq, "seq", ifelse(names(pals) %in% div, "div", "cat"))

	c4a_load(c4a_data(pals, types = types, series = "parks"))

})



#### c4a cat
local({
	pals = list(area7 = c("#FF9D9A", "#77AADD", "#F1CE63", "#2CA02C", "#B07AA1", "#9EDAE5", "#CC6677"),
				area8 = c("#CC6677", "#AEC7E8", "#44BB99", "#B07AA1", "#BBCC33", "#FFAABB", "#B6992D", "#98DF8A"),
				area9 = c("#EE8866", "#88CCEE", "#2CA02C", "#B07AA1", "#F1CE63", "#FFAABB", "#6699CC", "#44BB99", "#CC6677"),
				area7d = c("#72190E", "#332288", "#225555", "#997700", "#437DBF", "#994F88", "#666633"),
				area8d = c("#663333", "#1F77B4", "#225555", "#994F88", "#997700", "#332288", "#666633", "#661100"),
				area9d = c("#72190E", "#1965B0", "#225555", "#994F88", "#997700", "#332288", "#666633", "#663333", "#437DBF"),
				line7 = c("#1F77B4", "#2CA02C", "#E73F74", "#6699CC", "#994F88", "#117733", "#D37295"),
				line8 = c("#DC050C", "#1F77B4", "#117733", "#994F88", "#999933", "#D37295", "#6699CC", "#E73F74"),
				line9 = c("#EE3377", "#1F77B4", "#117733", "#CF1C90", "#999933", "#994455", "#6699CC", "#D37295", "#DC050C"))
	c4a_load(c4a_data(pals, types = "cat", series = "c4a"))
})



.z = get("z", .C4A)
.s = get("s", .C4A)

.z$cit = NULL
.z$bib = NULL

.zbib = bibtex::read.bib("build/references.bib")
.zdes = local({
	df = read.csv("build/description.csv")
	structure(df$description, names = df$series)
})

save(.z, .s, .zbib, .zdes, file="R/sysdata.rda", compress="xz")
source("build/build_data.R")

