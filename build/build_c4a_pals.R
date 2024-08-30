

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


##########################
c4a = list(area7 = area7,
			area8 = area8,
			area9 = area9,
			area7d = area7d,
			area8d = area8d,
			area9d = area9d,
			line7 = line7,
			line8 = line8,
			line9 = line9)


c4a_load(c4a_data(c4a2, series = "c4a2"))
c4a_gui()
