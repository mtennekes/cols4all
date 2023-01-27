if (requireNamespace("shiny") &&
  requireNamespace("shinyjs") &&
  requireNamespace("kableExtra") &&
  requireNamespace("colorblindcheck") &&
  interactive()) {

c4a_gui()

# categorical palettes with maximum number of colors
c4a_table(type = "cat")

# sort sequential palettes by hue
c4a_table(type = "seq", n = 7, sort = "H")

# sort sequential palettes by hue type (how many hues are used)
c4a_table(type = "seq", n = 5, sort = "hueType")
}
