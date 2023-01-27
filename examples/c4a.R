c4a_palettes("div")

c4a(type = "cat")

(pal = c4a("tol.sunset", n = 7, range = c(0, .6)))

c4a_plot(pal)

c4a("set2")

c4a("hcl.set2")

c4a("hcl.set2", n = 8)

# reversed palette
c4a("hcl.set2", reverse = TRUE, n = 8)

# handy shortcut
c4a("-hcl.set2", n = 8)

# the color for missing values is white:
c4a_na("hcl.set2")
