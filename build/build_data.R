
library(sf)

nc <- st_read(system.file("shape/nc.shp", package="sf")) |> st_transform(crs = 2264)
nc = nc[, "NAME"]
nc$geometry =  st_sfc(lapply(nc$geometry, function(nci) {
	nci = st_multipolygon(lapply(nci, function(ncii) lapply(ncii, round)))
}))

bbx = tmaptools::bb(st_bbox(nc), -1.1)

cols = c("white", "black")
ind = as.integer(substr(nc$NAME, 1, 1) %in% LETTERS[1:13]) + 1

shp = sf::st_as_grob(nc$geometry, gp = gpar(fill = cols[ind], col = NA))

attr(shp, "bbx") = bbx

vars = load("R/sysdata.rda")
save(.z, .s, .zbib, shp, bbx, file = "R/sysdata.rda", compress = "xz")






