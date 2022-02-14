source("build/build_cat.R")
source("build/build_seq_div.R")
source("build/get_scores.R")
.z = rbind(z_cat, z_seq_div)
.s = get_scores(.z, nmax = 36)

.maxC = sapply(0:360, function(h) {
	max(colorspace::max_chroma(h = h, l = 0:100))
})


save(.z, .s, .maxC, file="R/sysdata.rda", compress="xz")


# TODO
# pals stepped palettes: bivariate (cat x num)
# divering: X.colorBlindness.Blue2Orange12Steps and X.dichromat.BluetoOrange_12
#
