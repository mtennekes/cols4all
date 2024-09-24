

c_cat_names = c("carto.safe", "tableau.classic20", "tableau.20", "tol.muted", "tol.light") #, "okabe", c4a_palettes(type = "cat", series = "brewer"))
c_cat_list = lapply(c_cat_names, c4a)

pool = unlist(c_cat_list)

#######################################
############# area - light ############
#######################################

# pastel 7
# CR min 1.5

c4a_plot(pool)

sel = pool |>
	unique() |>
	colors_filter(CRWmin = 1.5, CRWmax = 4, Cmin = 30, Lmin = 40, Cmax = 110) |>
	colors_sort("H")
	#colors_sort("L") |>
	#colors_name("CR")

c4a_plot(sel)


#### 7
res7 = colors_cbf_set(sel, k = 7, dE_min = 10)

dput(res7$palettes[[15]])
area7 = c("#CC6677", "#FF9D9A", "#F1CE63", "#2CA02C", "#9EDAE5", "#77AADD",
  "#B07AA1") |>
	colors_order(weight_normal = 0.5, head = c(2,6,3))

c4a_plot(area7, include.cvd = T)


for (i in 2:7) {
	c4a_plot_map(area7[1:i], lwd = 1)
}



#### 8
system.time({
	# 456 sec
	res8 = colors_cbf_set(sel, k = 8, dE_min = 10)
})

dput(res8$palettes[[3]])
area8 = c("#CC6677", "#B6992D", "#BBCC33", "#98DF8A", "#44BB99", "#AEC7E8", "#B07AA1", "#FFAABB") |>
	colors_order(weight_normal = 0.5, head = c(1,6,5,7,3))

c4a_plot(area8, include.cvd = T)

for (i in 2:8) {
	c4a_plot_map(area8[1:i], lwd = 1)
}


#### 9
sel9 = unique(unlist(res8$palettes, res7$palettes)) |>
	colors_sort("H")

c4a_plot(sel9)

system.time({
	res9 = colors_cbf_set(sel9, k = 9, dE_min = 10)
})

dput(res9$palettes[c(5,6,7)])

list(c("#CC6677", "#EE8866", "#F1CE63", "#2CA02C", "#44BB99", "#9EDAE5", "#6699CC", "#B07AA1", "#FFAABB"),
	 c("#CC6677", "#EE8866", "#F1CE63", "#2CA02C", "#44BB99", "#88CCEE", "#6699CC", "#B07AA1", "#FFAABB"),
	 c("#CC6677", "#EE8866", "#F1CE63", "#2CA02C", "#44BB99", "#A0CBE8", "#6699CC", "#B07AA1", "#FFAABB"))

area9 = c("#CC6677", "#EE8866", "#F1CE63", "#2CA02C", "#44BB99", "#88CCEE", "#6699CC", "#B07AA1", "#FFAABB") |>
	colors_order(weight_normal = 0.5, head = c(2,6,4,8,3,9,7))

c4a_plot(area9, include.cvd = T)

for (i in 2:9) {
	c4a_plot_map(area9[1:i], lwd = 1)
}




#######################################
############# area - dark #############
#######################################


c_cat_names = c("carto.safe", "carto.bold", "tableau.classic20", "tableau.color_blind", "tableau.classic_color_blind", "tableau.20", "tol.muted", "tol.light", "tol.vibrant", "tol.medium", "tol.dark", "tol.rainbow")
c_cat_list = lapply(c_cat_names, c4a)

pool = unlist(c_cat_list)

############# dark


# pastel 7
# CR min 1.5

c4a_plot(pool)

sel = pool |>
	unique() |>
	colors_filter(CRBmin = 1.5, CRBmax = 5, Cmin = 20, Cmax = 100) |> #, Cmin = 30, Lmin = 40, Cmax = 110) |>
	colors_sort("H")
	#colors_sort("C") |>
	#colors_name("C")

c4a_plot(sel)


res7 = colors_cbf_set(sel, k = 7, dE_min = 10)

c4a_load(c4a_data(structure(res7$palettes, names = paste0("pal", 1:length(res7$palettes))), series = "c4a_test"), overwrite = TRUE)
c4a_gui()

dput(res7$palettes[[2]])
area7d = c("#72190E", "#997700", "#666633", "#225555", "#437DBF", "#332288", "#994F88") |>
	colors_order(weight_normal = 0.5, head = c(1,6,4))

c4a_plot(area7d, include.cvd = T)


#### 8
system.time({
	res8 = colors_cbf_set(sel, k = 8, dE_min = 10)
})



dput(res8$palettes[[1]])
area8d = c("#663333", "#661100", "#997700", "#666633", "#225555", "#1F77B4", "#332288", "#994F88") |>
	colors_order(weight_normal = 1, head = c(1, 6, 5, 8))

c4a_plot(area8d, include.cvd = T)

for (i in 2:8) {
	c4a_plot_map(area8d[1:i], lwd = 1)
}

#### 9

sel9 = unique(unlist(res8$palettes, res7$palettes)) |>
	colors_sort("H")

c4a_plot(sel9)

system.time({
	res9 = colors_cbf_set(sel9, k = 9, dE_min = 10)
})

dput(res9$palettes[[13]])

area9d = c("#663333", "#72190E", "#997700", "#666633", "#225555", "#437DBF", "#1965B0", "#332288", "#994F88") |>
	colors_order(weight_normal = 1, head = c(2, 7, 5, 9))

c4a_plot(area9d, include.cvd = T)

for (i in 2:9) {
	c4a_plot_map(area9d[1:i], lwd = 1, dark = T, borders = "white")
}

#######################################
############# line - light #############
#######################################




c_cat_names = c("carto.safe", "carto.bold", "tableau.classic20", "tableau.color_blind", "tableau.classic_color_blind", "tableau.20", "tol.muted", "tol.light", "tol.vibrant", "tol.medium", "tol.dark", "tol.rainbow")
c_cat_list = lapply(c_cat_names, c4a)

pool = unlist(c_cat_list)


sel = pool |>
	unique() |>
	colors_filter(CRWmin = 3, CRWmax = 7, Cmin = 40) |> #, Cmin = 30, Lmin = 40, Cmax = 110
	colors_sort("H")
#	colors_sort("C") |>
#	colors_name("C")

c4a_plot(sel)

#### 7
res7 = colors_cbf_set(sel, k = 7, dE_min = 10)

dput(res7$palettes[[18]])
line7 = c("#2CA02C", "#117733", "#6699CC", "#1F77B4", "#994F88", "#D37295", "#E73F74") |>
	colors_order(weight_normal = 0.5, head = c(4,1,7,3,5))

c4a_plot(line7, include.cvd = T)


for (i in 2:7) {
	c4a_plot_lines(line7[1:i], lwd = 3)
}

### 8
res8 = colors_cbf_set(sel, k = 8, dE_min = 10)
dput(res8$palettes[[1]])

line8 = c("#DC050C", "#999933", "#117733", "#6699CC", "#1F77B4", "#994F88", "#D37295", "#E73F74") |>
	colors_order(weight_normal = 0.5, head = c(1,5,3,6))

c4a_plot(line8, include.cvd = T)
c4a_plot_lines(line8, lwd = 3)

### 9

sel9 = unlist(c(res7$palettes, res8$palettes)) |>
	unique() |>
	colors_sort("H")

c4a_plot(sel9)

res9 = colors_cbf_set(sel9, k = 9, dE_min = 10)
res9 = colors_cbf_set(sel9, k = 9, dE_min = 10)

dput(res9$palettes[[1]])

line9 = c("#994455", "#DC050C", "#999933", "#117733", "#6699CC", "#1F77B4", "#CF1C90", "#D37295", "#EE3377") |>
	colors_order(head = c(9,6,4,7,3,1))

c4a_plot(line9, include.cvd = T)
c4a_plot_lines(line9, lwd = 3)



#######################################
############# cbf #############
#######################################

######## old pool for twilight
dput(pool_twilight)
pool_cvd = c("#CC6677", "#A5170E", "#FF9D9A", "#E65518", "#8C564B", "#997700",
			 "#F1CE63", "#DDCC77", "#AAAA00", "#999933", "#59A14F", "#225522",
			 "#009988", "#99DDFF", "#77AADD", "#3969AC", "#4B4B8F", "#9467BD",
			 "#B07AA1", "#882255", "#FABFD2", "#D37295", "#E73F74")
c4a_twilight7 = list(twilight7a = c("#DDCC77", "#999933", "#225522", "#77AADD", "#3969AC", "#882255", "#D37295"),
					 twilight7b = c("#F1CE63", "#59A14F", "#225522", "#99DDFF", "#9467BD", "#882255", "#D37295"),
					 twilight7c = c("#FF9D9A", "#AAAA00", "#225522", "#99DDFF", "#3969AC", "#882255", "#E73F74"),
					 twilight7d = c("#E65518", "#F1CE63", "#225522", "#009988", "#99DDFF", "#9467BD", "#882255"))
c4a_twilight9 = list(twilight9a = c("#FF9D9A", "#997700", "#AAAA00", "#225522", "#77AADD", "#4B4B8F", "#9467BD", "#882255", "#E73F74"),
					 twilight9b = c("#A5170E", "#997700", "#AAAA00", "#009988", "#4B4B8F", "#9467BD", "#882255", "#FABFD2", "#E73F74"),
					 twilight9c = c("#F1CE63", "#999933", "#225522", "#009988", "#99DDFF", "#4B4B8F", "#9467BD", "#882255", "#E73F74"),
					 twilight9d = c("#A5170E", "#997700", "#F1CE63", "#009988", "#99DDFF", "#4B4B8F", "#9467BD", "#882255", "#E73F74"))
c4a_twilight5 = list(twilight5a = c("#A5170E", "#F1CE63", "#009988", "#99DDFF", "#4B4B8F"),
					 twilight5b = c("#CC6677", "#F1CE63", "#225522", "#99DDFF", "#9467BD"),
					 twilight5c = c("#CC6677", "#A5170E", "#AAAA00", "#77AADD", "#3969AC"))
c4a_twilight11 = list(twilight11a = c("#CC6677", "#FF9D9A", "#997700", "#F1CE63", "#AAAA00", "#225522", "#99DDFF", "#77AADD", "#4B4B8F", "#9467BD", "#882255"),
					  twilight11b = c("#FF9D9A", "#F1CE63", "#AAAA00", "#225522", "#99DDFF", "#77AADD", "#4B4B8F", "#9467BD", "#B07AA1", "#882255", "#E73F74"),
					  twilight11c = c("#A5170E", "#FF9D9A", "#8C564B", "#F1CE63", "#59A14F", "#99DDFF", "#77AADD", "#4B4B8F", "#9467BD", "#B07AA1", "#882255"))
c4a_twilight13 = list(twilight13a = c("#A5170E", "#FF9D9A", "#8C564B", "#997700", "#F1CE63", "#AAAA00", "#009988", "#99DDFF", "#77AADD", "#4B4B8F", "#9467BD", "#882255", "#E73F74"),
					  twilight13b = c("#A5170E", "#FF9D9A", "#8C564B", "#997700", "#AAAA00", "#009988", "#99DDFF", "#77AADD", "#4B4B8F", "#9467BD", "#882255", "#FABFD2", "#E73F74"),
					  twilight13c = c("#FF9D9A", "#8C564B", "#997700", "#F1CE63", "#AAAA00", "#225522", "#009988", "#99DDFF", "#77AADD", "#4B4B8F", "#9467BD", "#882255", "#E73F74"),
					  twilight13d = c("#FF9D9A", "#8C564B", "#997700", "#AAAA00", "#225522", "#009988", "#99DDFF", "#77AADD", "#4B4B8F", "#9467BD", "#882255", "#FABFD2", "#E73F74"),
					  twilight13e = c("#A5170E", "#FF9D9A", "#8C564B", "#997700", "#DDCC77", "#AAAA00", "#009988", "#99DDFF", "#77AADD", "#4B4B8F", "#9467BD", "#882255", "#E73F74"),
					  twilight13f = c("#FF9D9A", "#8C564B", "#997700", "#DDCC77", "#AAAA00", "#225522", "#009988", "#99DDFF", "#77AADD", "#4B4B8F", "#9467BD", "#882255", "#E73F74"))

m = matrix(0L, nrow = 20, ncol = 23)
tpals = c(c4a_twilight5, c4a_twilight7, c4a_twilight9, c4a_twilight11, c4a_twilight13)
tlst = list(c4a_twilight5, c4a_twilight7, c4a_twilight9, c4a_twilight11, c4a_twilight13)


for (i in 1:length(tpals)) m[i, match(tpals[[i]], pool_cvd)] = 1L

find_matches = function(x, y) {
	nx = length(x)
	ny = length(y)

	sapply(1:nx, function(i) {
		sapply(1:ny, function(j) {
			length(intersect(x[[i]], y[[j]]))
		})
	})

}

tpals = c(c4a_twilight5, c4a_twilight7, c4a_twilight9, c4a_twilight11, c4a_twilight13)

tn = sapply(tlst, length)

tcomb = do.call(expand.grid, lapply(tn, seq, from = 1))

score = apply(tcomb, MARGIN = 1, function(ti) {
	res = lapply(1:length(ti), function(i) {
		tlst[[i]][[ti[i]]]
	})
	length(unique(unlist(res)))

	# y = combn(length(ti), 2)
	# z = apply(y, MARGIN = 2, function(yi) {
	# 	length(intersect(res[[yi[1]]], res[[yi[2]]]))
	# })
	# sum(z)
})

mx = which(score == min(score))


tcomb[mx, ]


ids = c(296, 299, 323, 371)
ids2 = which(score <= 16)
ids3 = c(296, 55)



score[c(323, 371)]

get_pals = function(i) {
	ind = as.integer(tcomb[i, ])
	lapply(1:length(ind), function(j) {
		tlst[[j]][[ind[[j]]]]
	})
}

get_pool = function(i) {
	pals = get_pals(i)
	pool = pals |>
		unlist() |>
		unique() |>
		colors_sort("H")
}

pools = lapply(ids2, get_pool)
names(pools) = paste0("pool", ids2)


setdiff(pools$pool55, pools$pool296)
setdiff(pools$pool296, pools$pool55)
setdiff(pools$pool296, pools$pool7)
setdiff(pools$pool7, pools$pool296)

c4a_plot(pools$pool7, nrows = 1)
c4a_plot(pools$pool296, nrows = 1)
c4a_plot(pools$pool55, nrows = 1)


c4a_load(c4a_data(pools, series = "cpools"), overwrite = TRUE)

c4a_gui()

create_c4a_data = function(i) {
	pool = get_pool(i)

	ps = lapply(2:13, function(k) {
		res = suppressMessages(colors_cbf_set(pool, k = k, step = 0, plot = FALSE))
		l = min(length(res$palettes), 15)
		p = res$palettes[1:l]
		names(p) = paste0("k", k, "_n", 1:l)
		p
	})
	pss = do.call(c, ps)
	c4a_data(pss, series = paste0("c", i))
}



create_c4a_data_orig = function(i) {
	pals = get_pals(i)
	names(pals) = paste0("k", sapply(pals, length), "_orig")
	c4a_data(pals, series = paste0("c", i, "_orig"))
}

create_c4a_data_sel = function(i, sel) {
	pool = get_pool(i)

	ps = lapply(2:13, function(k) {
		res = suppressMessages(colors_cbf_set(pool, k = k, step = 0, plot = FALSE))
		res$palettes[[sel[k-1]]]
	})
	pals = c(list(ps[[1]][1]), ps)
	names(pals) = 1:13

	ids = lapply(pals, function(p) {
		match(p, pool)
	})

	#ids2 = c(ids, list(index14 = 2:15, index15 = 1:15))


	attr(pool, "index") = ids

	c4a_data(list(cbf = pool), series = paste0("c", i))
}

p = lapply(ids3, create_c4a_data)

po = lapply(ids3, create_c4a_data_orig)

ps = create_c4a_data_sel(296, c(14,3,12,5,1,1,2,3,1,2,3,1))


lapply(po, c4a_load, overwrite = TRUE)

lapply(p, c4a_load, overwrite = TRUE)
c4a_load(ps)
c4a_gui()

str(p,1)



create_index = function(i) {
	pals = get_pals(i)
	pool = pals |>
		unlist() |>
		unique() |>
		colors_sort("H")

	colors_cbf_set(pool, k = 5, step = 0, dE_min = 20)
	colors_cbf_set(pool, k = 7, step = 0, dE_min = 20)
	colors_cbf_set(pool, k = 9, step = 0, dE_min = 20)
	colors_cbf_set(pool, k = 11, step = 0, dE_min = 20)
	colors_cbf_set(pool, k = 13, step = 0, dE_min = 20)


	for (j in 1:5) c4a_plot(pals[[j]], nrows = 1)

	lapply(1:13, function(k) {
		p = if (k < 5) {
			pals[[1]][1:k]
		} else
	})
}



get_pals(296) |>
	unlist() |>
	unique() |>
	colors_sort("H") |>
	c4a_plot()






find_matches(c4a_twilight7, c4a_twilight13)







res11 = colors_cbf_set(pool_cvd, k = 11)
res13 = colors_cbf_set(pool_cvd, k = 13)







c_cat_names = c("carto.safe", "carto.bold", "tableau.classic20", "tableau.color_blind", "tableau.classic_color_blind", "tableau.20", "tol.muted", "tol.light", "tol.vibrant", "tol.medium", "tol.dark", "tol.rainbow")
c_cat_list = lapply(c_cat_names, c4a)

pool5 = unlist(c_cat_list) |>
	unique() |>
	colors_filter(CRWmin = 1.5, Cmin = 34, CRWmax = 9) |> #, Cmin = 30, Lmin = 40, Cmax = 110
	colors_sort("H") |>
	colors_remove_twins(th = 3, include.cvd = F)




init5 = colors_cbf_set(pool5, k = 5, dE_min = 18, parallelize = FALSE, step = -1)
res5 = colors_cbf_set(init = init5, step = 1)

saveRDS(res5, file = "build/res5.rds")

pals=structure(res5$palettes, names = paste0("pal", 1:length(res5$palettes)))



pool7 = unlist(pals) |>
	unique() |>
	colors_sort("H")|>
	colors_remove_twins(th = 6)
c4a_plot(pool7)

# attempt 1: 9: 12.42, 11: 9.94

saveRDS(pool7, file = "temp/pool7.rds")

pool7 = readRDS("temp/pool7.rds")

init7 = colors_cbf_set(pool7, k = 7, dE_min = 10, parallelize = FALSE, step = -1)

res7 = colors_cbf_set(init = init7, step = 1)
saveRDS(res7, file = "build/res7.rds")


pool9 = unlist(res7$palettes[1:250]) |>
	unique() |>
	colors_sort("H")
saveRDS(pool9, file = "temp/pool9.rds")

init9 = colors_cbf_set(pool9, k = 9, dE_min = 10)
res9 = colors_cbf_set(init = init9, step = 1)
saveRDS(res9, file = "build/res9b.rds") # 3 hours # pool9 = 41


pool11 = unlist(res9$palettes[1:29]) |>
	unique() |>
	colors_sort("H")

c4a_plot(pool11)
c4a_plot(pool_twilight)

init11 = colors_cbf_set(pool11, k = 11, dE_min = 10)
res11 = colors_cbf_set(init = init11, step = 1)

res11 = colors_cbf_set(init = init9, step = 1)




identical(pool11, pool_twilight)

res9$dE[1]

res11 = colors_cbf_set(pool9, k = 11, dE_min = 10)
saveRDS(res9, file = "build/res9.rds")
saveRDS(res11, file = "build/res11.rds")

# attempt 2: 9: , 11:

res7 = colors_cbf_set(pool7, k = 7, dE_min = 10)
saveRDS(res7, file = "build/res7.rds")

pals7=structure(res7$palettes, names = paste0("pal", 1:length(res7$palettes)))


pool9 = unlist(pals7) |>
	unique() |>
	colors_sort("H")

res9 = colors_cbf_set(pool9, k = 9, dE_min = 15)
res11 = colors_cbf_set(pool9, k = 11, dE_min = 10)
saveRDS(res9, file = "build/res9.rds")
saveRDS(res11, file = "build/res11.rds")


pals3=structure(res$palettes, names = paste0("pal", 1:length(res$palettes)))


res = colors_cbf_set(sel2, k = 9, dE_min = 15)
pals3=structure(res$palettes, names = paste0("pal", 1:length(res$palettes)))

c4a_sysdata_remove(series = "c4b")
c4a_load(c4a_data(pals3, series = "c4b"))


c4a_sysdata_remove(series = "c4b")
c4a_load(c4a_data(pals7, series = "c4b"))

c4a_gui()




########## old attempt

c4a_day = list(day5 = c("#FF9D9A", "#F1CE63", "#59A14F", "#99DDFF", "#B07AA1"),
			   day7 = c("#CC6677", "#FF9D9A", "#F1CE63", "#AAAA00", "#99DDFF", "#77AADD", "#B07AA1"),
			   day9 = c("#CC6677", "#FF9D9A", "#DDCC77", "#AAAA00", "#59A14F", "#99DDFF", "#77AADD", "#B07AA1", "#FABFD2"))
c4a_night7 = list(night7a = c("#A5170E", "#8C564B", "#999933", "#009988", "#4B4B8F", "#9467BD", "#E73F74"),
				  night7b = c("#A5170E", "#E65518", "#9D7660", "#6195CF", "#3969AC", "#882255", "#D37295"),
				  night7c = c("#A5170E", "#997700", "#009988", "#4B4B8F", "#9467BD", "#882255", "#E73F74"),
				  night7d = c("#E65518", "#9D7660", "#225522", "#6195CF", "#4B4B8F", "#882255", "#D37295"))
c4a_night9 = list(night9a = c("#A5170E", "#E65518", "#8C564B", "#9D7660", "#009988","#4B4B8F", "#9467BD", "#882255", "#D37295"),
				  night9b = c("#A5170E", "#8C564B", "#9D7660", "#999933", "#009988", "#4B4B8F", "#9467BD","#882255", "#D37295"),
				  night9c = c("#A5170E", "#8C564B", "#997700", "#009988", "#4B4B8F", "#9467BD", "#882255", "#D37295", "#E73F74"),
				  night9d = c("#A5170E", "#8C564B", "#999933", "#009988", "#4B4B8F", "#9467BD", "#882255", "#D37295", "#E73F74"),
				  night9e = c("#E65518", "#8C564B", "#9D7660", "#225522", "#009988", "#4B4B8F", "#9467BD", "#882255", "#D37295"),
				  night9f = c("#8C564B", "#9D7660", "#999933", "#225522", "#009988", "#4B4B8F", "#9467BD", "#882255", "#D37295"),
				  night9g = c("#8C564B", "#997700", "#225522", "#009988", "#4B4B8F", "#9467BD", "#882255", "#D37295", "#E73F74"),
				  night9h = c("#8C564B", "#999933", "#225522", "#009988", "#4B4B8F", "#9467BD", "#882255", "#D37295", "#E73F74"))
c4a_night5 = list(night5a = c("#997700", "#6195CF", "#3969AC", "#882255", "#E73F74"),
				  night5b = c("#999933", "#225522", "#3969AC", "#882255", "#D37295"),
				  night5c = c("#E65518", "#225522", "#6195CF", "#3969AC", "#882255"))
c4a_twilight7 = list(twilight7a = c("#DDCC77", "#999933", "#225522", "#77AADD", "#3969AC", "#882255", "#D37295"),
					 twilight7b = c("#F1CE63", "#59A14F", "#225522", "#99DDFF", "#9467BD", "#882255", "#D37295"),
					 twilight7c = c("#FF9D9A", "#AAAA00", "#225522", "#99DDFF", "#3969AC", "#882255", "#E73F74"),
					 twilight7d = c("#E65518", "#F1CE63", "#225522", "#009988", "#99DDFF", "#9467BD", "#882255"))
c4a_twilight9 = list(twilight9a = c("#FF9D9A", "#997700", "#AAAA00", "#225522", "#77AADD", "#4B4B8F", "#9467BD", "#882255", "#E73F74"),
					 twilight9b = c("#A5170E", "#997700", "#AAAA00", "#009988", "#4B4B8F", "#9467BD", "#882255", "#FABFD2", "#E73F74"),
					 twilight9c = c("#F1CE63", "#999933", "#225522", "#009988", "#99DDFF", "#4B4B8F", "#9467BD", "#882255", "#E73F74"),
					 twilight9d = c("#A5170E", "#997700", "#F1CE63", "#009988", "#99DDFF", "#4B4B8F", "#9467BD", "#882255", "#E73F74"))
c4a_twilight5 = list(twilight5a = c("#A5170E", "#F1CE63", "#009988", "#99DDFF", "#4B4B8F"),
					 twilight5b = c("#CC6677", "#F1CE63", "#225522", "#99DDFF", "#9467BD"),
					 twilight5c = c("#CC6677", "#A5170E", "#AAAA00", "#77AADD", "#3969AC"))
c4a_twilight11 = list(twilight11a = c("#CC6677", "#FF9D9A", "#997700", "#F1CE63", "#AAAA00", "#225522", "#99DDFF", "#77AADD", "#4B4B8F", "#9467BD", "#882255"),
					  twilight11b = c("#FF9D9A", "#F1CE63", "#AAAA00", "#225522", "#99DDFF", "#77AADD", "#4B4B8F", "#9467BD", "#B07AA1", "#882255", "#E73F74"),
					  twilight11c = c("#A5170E", "#FF9D9A", "#8C564B", "#F1CE63", "#59A14F", "#99DDFF", "#77AADD", "#4B4B8F", "#9467BD", "#B07AA1", "#882255"))
c4a_twilight13 = list(twilight13a = c("#A5170E", "#FF9D9A", "#8C564B", "#997700", "#F1CE63", "#AAAA00", "#009988", "#99DDFF", "#77AADD", "#4B4B8F", "#9467BD", "#882255", "#E73F74"),
					  twilight13b = c("#A5170E", "#FF9D9A", "#8C564B", "#997700", "#AAAA00", "#009988", "#99DDFF", "#77AADD", "#4B4B8F", "#9467BD", "#882255", "#FABFD2", "#E73F74"),
					  twilight13c = c("#FF9D9A", "#8C564B", "#997700", "#F1CE63", "#AAAA00", "#225522", "#009988", "#99DDFF", "#77AADD", "#4B4B8F", "#9467BD", "#882255", "#E73F74"),
					  twilight13d = c("#FF9D9A", "#8C564B", "#997700", "#AAAA00", "#225522", "#009988", "#99DDFF", "#77AADD", "#4B4B8F", "#9467BD", "#882255", "#FABFD2", "#E73F74"),
					  twilight13e = c("#A5170E", "#FF9D9A", "#8C564B", "#997700", "#DDCC77", "#AAAA00", "#009988", "#99DDFF", "#77AADD", "#4B4B8F", "#9467BD", "#882255", "#E73F74"),
					  twilight13f = c("#FF9D9A", "#8C564B", "#997700", "#DDCC77", "#AAAA00", "#225522", "#009988", "#99DDFF", "#77AADD", "#4B4B8F", "#9467BD", "#882255", "#E73F74"))


### old pool used to make twilight palettes
pool_twilight = unlist(c(c4a_twilight5,c4a_twilight7,c4a_twilight9,c4a_twilight11,c4a_twilight13)) |>
	unique() |>
	colors_sort("H")




sapply(c4a_twilight7, function(x) {
	sapply(c4a_twilight13, function(y) {
		all(x %in% y)
	})

})



c4 = c(c4a_day, c4a_night5, c4a_night7, c4a_night9, c4a_twilight5, c4a_twilight7, c4a_twilight9, c4a_twilight11, c4a_twilight13)

# example HCL and CR
# pool = unlist(c4) |>
# 	unique() |>
# 	colors_sort("H")
#
# pool |>
# 	colors_name("C") |>
# 	c4a_plot()


c4a_sysdata_remove(series = "c4c")
c4a_load(c4a_data(c4, series = "c4c"), overwrite = TRUE)


################################
c4a = list(area7 = c("#FF9D9A", "#77AADD", "#F1CE63", "#2CA02C", "#B07AA1", "#9EDAE5", "#CC6677"),
			area8 = c("#CC6677", "#AEC7E8", "#44BB99", "#B07AA1", "#BBCC33", "#FFAABB", "#B6992D", "#98DF8A"),
			area9 = c("#EE8866", "#88CCEE", "#2CA02C", "#B07AA1", "#F1CE63", "#FFAABB", "#6699CC", "#44BB99", "#CC6677"),
			area7d = c("#72190E", "#332288", "#225555", "#997700", "#437DBF", "#994F88", "#666633"),
			area8d = c("#663333", "#1F77B4", "#225555", "#994F88", "#997700", "#332288", "#666633", "#661100"),
			area9d = c("#72190E", "#1965B0", "#225555", "#994F88", "#997700", "#332288", "#666633", "#663333", "#437DBF"),
			line7 = c("#1F77B4", "#2CA02C", "#E73F74", "#6699CC", "#994F88", "#117733", "#D37295"),
			line8 = c("#DC050C", "#1F77B4", "#117733", "#994F88", "#999933", "#D37295", "#6699CC", "#E73F74"),
			line9 = c("#EE3377", "#1F77B4", "#117733", "#CF1C90", "#999933", "#994455", "#6699CC", "#D37295", "#DC050C"))




#
# c4a = list(area7 = area7,
# 		   area8 = area8,
# 		   area9 = area9,
# 		   area7d = area7d,
# 		   area8d = area8d,
# 		   area9d = area9d,
# 		   line7 = line7,
# 		   line8 = line8,
# 		   line9 = line9)

c4a_load(c4a_data(c4a2, series = "c4a"))
c4a_gui()
