library("ggplot2")

data("diamonds")
diam_exp = diamonds[diamonds$price >= 15000, ]
diam_exp$clarity[1:500] = NA

# discrete categorical scale
ggplot(diam_exp, aes(x = carat, y = price, color = color)) +
	geom_point(size = 2) +
	scale_color_c4a_cat("carto.safe") +
	theme_light()


# missing values
ggplot(diam_exp, aes(x = carat, y = price, fill = clarity)) +
	geom_point(size = 2, shape = 21) +
	scale_fill_c4a_cat("carto.safe") +
	theme_light()

# discrete sequential scale
ggplot(diam_exp, aes(x = carat, y = price, color = cut)) +
	geom_point(size = 2) +
	scale_color_c4a_seq("scico.hawaii") +
	theme_light()
