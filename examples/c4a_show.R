if (interactive()) {
	c4a_gui()
}

# categorical palettes with maximum number of colors
c4a_show(type = "cat")

# sort sequential palettes by rank (based on color blind-friendliness, and color balance)
c4a_show(type = "seq", n = 7, sort = "rank")

# sort by hue type, which is indication how many hues are used.
c4a_show(type = "seq", n = 5, sort = "hueType")
