library("ggplot2")

data("diamonds")

# find suitable palette
c4a_show(n = nlevels(diamonds$color), sort = "rank")

# select "carto.safe"
ggplot(diamonds[diamonds$price >= 15000, ], aes(x = carat, y = price, color = color)) +
	geom_point(size = 2) +
	scale_color_c4a_cat("carto.safe") +
	theme_light()
