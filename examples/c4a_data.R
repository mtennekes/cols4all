# palettes extracted Pink Floyd albums
pf = list(piper = c("#391C1C", "#C6C6AA", "#713939", "#C6391C",
    "#C6E3C6", "#AA7155", "#AA8E71", "#C68E71"),
		  saucerful = c("#000000", "#1C1C1C", "#393939", "#FFFFFF",
    "#555555", "#8E8E71", "#E3C6AA", "#715539"),
		  atom = c("#C6E3FF", "#397139", "#557139", "#E3E3C6",
    "#1C1C1C", "#1C551C", "#AAAA8E", "#8EC6E3"),
		  meddle = c("#715539", "#553939", "#8E7155", "#71AAAA",
    "#8E8E71", "#1CAAE3", "#55C6E3", "#AA7155"),
		  obscured = c("#000000", "#1C1C1C", "#393939", "#717155",
    "#8E8E71", "#715539", "#C6AA8E", "#E3C6AA"),
		  moon = c("#000000", "#FF0000", "#FF9224", "#FFFF00",
    "#71C600", "#00C6FF", "#8E398E", "#FFFFFF"),
		  wish = c("#FFFFFF", "#AAC6E3", "#8E8E8E", "#717155",
    "#555539", "#8E8E71", "#555555", "#8E7155"),
		  animals = c("#391C39", "#393955", "#E3C671", "#718E8E",
    "#AAAA8E", "#C67139", "#AA5539", "#E3AA39"),
		  wall = c("#FFFFFF", "#E3E3E3", "#C6C6C6", "#AAAAC6",
    "#1C1C1C", "#000000", "#8E8E8E", "#E3C6E3"),
		  cut = c("#000000", "#E30000", "#AA0000", "#391C55",
    "#FFE3E3", "#1C1C00", "#FFAA55", "#8E8E55"),
		  lapse = c("#000000", "#8E8EC6", "#8E8E71", "#7171AA",
    "#39391C", "#717171", "#AAAAAA", "#E3E3E3"),
		  division = c("#000000", "#FFFFC6", "#00398E", "#AA8E55",
    "#39558E", "#C6AA71", "#39391C", "#555571"),
		  more = c("#0055AA", "#FFAA1C", "#1C71AA", "#003971",
    "#E38E55", "#E3AAAA", "#718EAA", "#71718E"),
		  umma = c("#AA8E71", "#555539", "#39391C", "#1C1C1C",
    "#E3E3C6", "#715539", "#391C1C", "#8E7155"),
		  relics = c("#3955AA", "#1C3971", "#5571C6", "#715555",
    "#8E7155", "#E3AA71", "#8E8EAA", "#E3FFFF"),
		  river = c("#393939", "#555555", "#39558E", "#C6C6C6",
    "#718EAA", "#1C1C1C", "#717171", "#E3C68E"))

if (requireNamespace("colorblindcheck", quietly = TRUE)) {
	pfdata = c4a_data_as_is(pf, series = "pinkfloyd",
		description = "Palettes extracted from Pink Floyd album covers")
	c4a_load(pfdata)

	c4a_series()
	c4a_overview()

	if (requireNamespace("shiny") &&
		requireNamespace("shinyjs") &&
		requireNamespace("kableExtra") &&
		requireNamespace("colorblindcheck") &&
		requireNamespace("plotly") &&
		interactive()) {
		c4a_gui(series = "pinkfloyd", n = 8)
	}
}
