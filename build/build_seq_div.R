library(pals)
library(rcartocolor)
library(grid)
library(colorblindcheck)
library(rcartocolor)


#####################################################
######### sequential
#####################################################

c4a_name_compress = function(x) {
	x2 = gsub("[-, \\,, (, ), \\ ]",  "", x)
	y <- tolower(gsub("([a-z])([A-Z])", "\\1_\\L\\2", x2, perl = TRUE))
	#substr(tolower(gsub("([A-Z])","\\_\\1", x2)), 2,.Machine$integer.max)
	y
}


# merge diverging and divergingx


hcl = hcl_palettes()

seq_sh = rownames(hcl)[hcl$type == "Sequential (single-hue)"]
seq_mh = rownames(hcl)[hcl$type == "Sequential (multi-hue)"]

seq = c(seq_sh, seq_mh) |> c4a_name_compress()


# see ?hcl_palettes for explanation
div_lst = lapply(c("diverging", "divergingx"), hcl.pals)
div_th = div_lst[[1]]
div_mh = div_lst[[2]]

div = c(div_th, div_mh)


spals = lapply(seq, function(s) {
	pal = hcl.colors(9, s)
	il = is_light(pal[c(1,9)])
	if (il[2] && !il[1]) {
		rev(pal)
	} else {
		pal
	}
})
names(spals) = paste0("hcl.", seq)

dpals = lapply(div, function(s) {
	hcl.colors(9, s)
})

div_names = c4a_name_compress(div)
div_names[div_names == "prgn"] = "pr_gn"

names(dpals) = paste0("hcl.", div_names)

type = c(rep("seq", length(spals)), rep("div", length(dpals)))

z_hcl = data.frame(name = c(names(spals), names(dpals)), type = type, series = "hcl", palette = I(c(spals, dpals)), nmax = Inf)




###############
### Tol
###############

pals_tol = list(
	tol.sunset = c('#364B9A', '#4A7BB7', '#6EA6CD', '#98CAE1', '#C2E4EF',
				   '#EAECCC', '#FEDA8B', '#FDB366', '#F67E4B', '#DD3D2D',
				   '#A50026'),
	tol.bu_rd = c('#2166AC', '#4393C3', '#92C5DE', '#D1E5F0', '#F7F7F7',
				  '#FDDBC7', '#F4A582', '#D6604D', '#B2182B'),
	tol.pr_gn = c('#762A83', '#9970AB', '#C2A5CF', '#E7D4E8', '#F7F7F7',
				  '#D9F0D3', '#ACD39E', '#5AAE61', '#1B7837'),
	tol.yl_or_br = c('#FFFFE5', '#FFF7BC', '#FEE391', '#FEC44F', '#FB9A29',
					 '#EC7014', '#CC4C02', '#993404', '#662506'),
	tol.wh_or_br = c('#FFFFFF', '#FFF7BC', '#FEE391', '#FEC44F', '#FB9A29',
					 '#EC7014', '#CC4C02', '#993404', '#662506'),
	tol.iridescent = c('#FEFBE9', '#FCF7D5', '#F5F3C1', '#EAF0B5', '#DDECBF',
					   '#D0E7CA', '#C2E3D2', '#B5DDD8', '#A8D8DC', '#9BD2E1',
					   '#8DCBE4', '#81C4E7', '#7BBCE7', '#7EB2E4', '#88A5DD',
					   '#9398D2', '#9B8AC4', '#9D7DB2', '#9A709E', '#906388',
					   '#805770', '#684957', '#46353A'),
	tol.rainbow_pu_rd = c('#6F4C9B', '#6059A9', '#5568B8', '#4E79C5', '#4D8AC6',
						  '#4E96BC', '#549EB3', '#59A5A9', '#60AB9E', '#69B190',
						  '#77B77D', '#8CBC68', '#A6BE54', '#BEBC48', '#D1B541',
						  '#DDAA3C', '#E49C39', '#E78C35', '#E67932', '#E4632D',
						  '#DF4828', '#DA2222'),
	tol.rainbow_pu_br = c('#6F4C9B', '#6059A9', '#5568B8', '#4E79C5', '#4D8AC6',
						  '#4E96BC', '#549EB3', '#59A5A9', '#60AB9E', '#69B190',
						  '#77B77D', '#8CBC68', '#A6BE54', '#BEBC48', '#D1B541',
						  '#DDAA3C', '#E49C39', '#E78C35', '#E67932', '#E4632D',
						  '#DF4828', '#DA2222', '#B8221E', '#95211B', '#721E17',
						  '#521A13'),
	tol.rainbow_wh_rd = c('#E8ECFB', '#DDD8EF', '#D1C1E1', '#C3A8D1', '#B58FC2',
						  '#A778B4', '#9B62A7', '#8C4E99', '#6F4C9B', '#6059A9',
						  '#5568B8', '#4E79C5', '#4D8AC6', '#4E96BC', '#549EB3',
						  '#59A5A9', '#60AB9E', '#69B190', '#77B77D', '#8CBC68',
						  '#A6BE54', '#BEBC48', '#D1B541', '#DDAA3C', '#E49C39',
						  '#E78C35', '#E67932', '#E4632D', '#DF4828', '#DA2222'),
	tol.rainbow_wh_br = c('#E8ECFB', '#DDD8EF', '#D1C1E1', '#C3A8D1', '#B58FC2',
						  '#A778B4', '#9B62A7', '#8C4E99', '#6F4C9B', '#6059A9',
						  '#5568B8', '#4E79C5', '#4D8AC6', '#4E96BC', '#549EB3',
						  '#59A5A9', '#60AB9E', '#69B190', '#77B77D', '#8CBC68',
						  '#A6BE54', '#BEBC48', '#D1B541', '#DDAA3C', '#E49C39',
						  '#E78C35', '#E67932', '#E4632D', '#DF4828', '#DA2222',
						  '#B8221E', '#95211B', '#721E17', '#521A13'))


type = ifelse(names(pals_tol) %in% c("tol.bu_rd", "tol.pr_gn", "tol.sunset"), "div", "seq")

pals_tol[type == "seq"] = lapply(pals_tol[type == "seq"], function(p) {
	il = is_light(p[c(1,length(p))])
	if (il[2] && !il[1]) {
		rev(p)
	} else {
		p
	}
})


z_tol = data.frame(name = names(pals_tol), type = type, series = "tol", palette = I(pals_tol), nmax = Inf)


pals_lb = dichromat::colorschemes
names(pals_lb) = paste0("lb.", c4a_name_compress(names(pals_lb)))

z_lb = data.frame(name = names(pals_lb), type = "div", series = "lb", palette = I(pals_lb), nmax = Inf)


z_seq_div = rbind(z_hcl, z_tol, z_lb)



