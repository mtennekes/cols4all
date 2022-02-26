c4a_palettes("div")

c4a(type = "cat")

(pal = c4a("tol.sunset", n = 7, contrast = c(0, .6)))
colorspace::specplot(pal)

c4a("set2")

c4a("hcl.set2")

c4a("hcl.set2", n = 8)

# reversed palette
c4a("hcl.set2", reverse = TRUE, n = 8)

# handy shortcut
c4a("-hcl.set2", n = 8)
