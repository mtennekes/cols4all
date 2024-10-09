# get the colors from brewer.set3 and plot them
set3 <- c4a("brewer.set3")
c4a_plot_hex(set3, nrows = 1)

c4a("hcl.set2", n = 36) |> c4a_plot_hex()
c4a("-hcl.set2", n = 12) |> c4a_plot_hex()

# how to know which palettes are avaiable?
# 1) Via the interactive tool:
\dontrun{
	c4a_gui()
}

# 2) Via the overview function:
c4a_palettes(type = "cat")
c4a_palettes(series = "brewer")
c4a_palettes(type = "cat", series = "brewer")
# Run c4a_overview() to see which are available

# 3) Via .P
.P$brewer$cat$set3

# each palette contains a color for missing values

c4a("carto.safe", 7)
c4a_na("carto.safe")

c4a_plot_hex("carto.safe", n = 7, include.na = TRUE)


c4a_plot_hex("carto.safe", n = 7, include.na = TRUE)
# same (but shorter) as
# c4a_plot_hex(c(c4a("carto.safe", 7), c4a_na("carto.safe")), include.na = TRUE)


# color ramp
c4a("viridis", 100) |> c4a_plot()
c4a_ramp("viridis")(100) |> c4a_plot()
