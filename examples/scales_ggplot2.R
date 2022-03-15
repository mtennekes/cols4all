library("ggplot2")

data("diamonds")
diam_exp = diamonds[diamonds$price >= 15000, ]
diam_exp$clarity[1:500] = NA

# discrete categorical scale
ggplot(diam_exp, aes(x = carat, y = price, color = color)) +
	geom_point(size = 2) +
	scale_color_discrete_c4a_cat("carto.safe") +
	theme_light()

# missing values
ggplot(diam_exp, aes(x = carat, y = price, fill = clarity)) +
	geom_point(size = 2, shape = 21) +
	scale_fill_discrete_c4a_cat("tol.muted") +
	theme_light()

# discrete sequential scale
ggplot(diam_exp, aes(x = carat, y = price, color = cut)) +
	geom_point(size = 2) +
	scale_color_discrete_c4a_seq("hcl.blues2") +
	theme_light()

# continuous sequential scale
ggplot(diam_exp, aes(x = carat, y = price, color = depth)) +
	geom_point(size = 2) +
	scale_color_continuous_c4a_seq("hcl.blues2", range = c(0.4, 1)) +
	theme_light()

# continuous diverging scale
ggplot(diam_exp, aes(x = carat, y = depth, color = price)) +
	geom_point(size = 2) +
	scale_color_continuous_c4a_div("wes.zissou1", mid = mean(diam_exp$price)) +
	theme_light()

# binned sequential scale
ggplot(diam_exp, aes(x = carat, y = price, color = depth)) +
	geom_point(size = 2) +
	scale_color_binned_c4a_seq("scico.batlow", range = c(0.4, 1)) +
	theme_light()
