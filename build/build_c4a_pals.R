c_cat_names = c("carto.safe", "tableau.classic20", "tableau.20", "tol.muted", "tol.light", "okabe")
c_cat_list = lapply(c_cat_names, c4a)

pool = unlist(c_cat_list)

c4a_plot(pool)

sel = pool |>
	unique() |>
	colors_filter(CRmin = 3, Cmin = 30) |>
	colors_sort("C") |>
	colors_name("C")

c4a_plot(sel)

res = colors_cbf_set(sel, k = 7, dE_min = 11)

c4a_load(c4a_data(structure(res$palettes, names = paste0("pal", 1:length(res$palettes))), series = "c4anew"))

c4a_gui()
