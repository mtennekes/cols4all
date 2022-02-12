source("build/build_cat.R")
source("build/build_seq_div.R")
z = rbind(z_cat, z_seq_div)

z = attach_scores(z, nmax.cat = 36, nmax.oth = 36)

save(z, file="R/sysdata.rda", compress="xz")


# TODO
# pals stepped palettes: bivariate (cat x num)
# divering: X.colorBlindness.Blue2Orange12Steps and X.dichromat.BluetoOrange_12
#
