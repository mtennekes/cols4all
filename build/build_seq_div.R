library(pals)
library(rcartocolor)
library(grid)
library(colorblindcheck)
library(rcartocolor)
library(wesanderson)

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

light_to_dark = function(p) {
	il = is_light(p[c(1,length(p))])
	if (il[2] && !il[1]) {
		rev(p)
	} else {
		p
	}
}


pals_tol[type == "seq"] = lapply(pals_tol[type == "seq"], light_to_dark)


z_tol = data.frame(name = names(pals_tol), type = type, series = "tol", palette = I(pals_tol), nmax = Inf)


remove_after_dot = function(x) sub("*\\.[0-9]+", "", x)

###############
### lb
###############



pals_lb = dichromat::colorschemes[c(2,4,6,7:9,11,13,15:17)]
names(pals_lb) = remove_after_dot(paste0("lb.", c4a_name_compress(names(pals_lb))))

pals_lb = pals_lb[!(names(pals_lb) %in% c("lb.categorical", "lb.stepped_sequential"))] # identical to lb.paired # stepped to be added later

lb_seq = c("lb.light_blueto_dark_blue")
lb_cat = c("lb.categorical", "lb.stepped_sequential")

z_lb = data.frame(name = names(pals_lb),
				  type = ifelse(names(pals_lb) %in% lb_seq, "seq",
				  			  ifelse(names(pals_lb) %in% lb_cat, "cat", "div")),
				  series = "lb",
				  palette = I(pals_lb),
				  nmax = ifelse(names(pals_lb) %in% lb_cat, unname(sapply(pals_lb, length)), Inf))


###############
### kovesi
###############

z_kovesi = local({
	pals_syspals = pals:::syspals

	kov = pals_syspals[substr(names(pals_syspals), 1, 6) == "kovesi"]

	isdiv = substr(names(kov), 1, 16) == "kovesi.diverging"
	iscyc = substr(names(kov), 1, 13) == "kovesi.cyclic"

	type = ifelse(isdiv, "div", ifelse(iscyc, "cyc", "seq"))


	kov[type == "seq"] = lapply(kov[type == "seq"], light_to_dark)


	z_lb = data.frame(name = names(kov),
					  type = type,
					  series = "kovesi",
					  palette = I(kov),
					  nmax = Inf)
	z_lb[z_lb$type!="cyc", ] # cyclic to be added later
})


z_wes = local({
	pals = wesanderson::wes_palettes
	names(pals) = c4a_name_compress(names(pals))

	names(pals)[c(18,19)] = c("isle_of_dogs1", "isle_of_dogs2")

	type = ifelse(names(pals) == "zissou1", "div", "cat")
	nmax = unname(sapply(pals, length))
	nmax[names(pals) == "zissou1"] = Inf

	data.frame(name = names(pals),
					  type = type,
					  series = "wes",
					  palette = I(pals),
					  nmax = nmax)

})

z_seq_div = rbind(z_hcl, z_tol, z_lb, z_kovesi, z_wes)



