#reprex::reprex({
sessionInfo(package = "paletteer")
library(paletteer)

table(palettes_c_names$type)
# typo: sequantial

table(palettes_dynamic_names$type)
table(palettes_d_names$type)
# divergent instead of diverging

palettes_d$ggthemes$stata_economist
# first value is NA

# palettes_d is not consistent with palettes_d_names:
pals_d = do.call(c, palettes_d)
setdiff(names(pals_d), paste0(palettes_d_names$package, ".", palettes_d_names$palette))
setdiff(paste0(palettes_d_names$package, ".", palettes_d_names$palette), names(pals_d))
#})


# discrete (fw)		discrete (dyn)			continuous
# palettes_d_names	palettes_dynamic_names  palettes_c_names

df_d = palettes_d_names
df_d$type = ifelse(df_d$type %in% c("sequantial", "sequential"), "seq", ifelse(df_d$type == "divergent", "div", "cat"))
df_d$n = df_d$length

setdiff(names(pals_d), paste0(df_d$package, ".", df_d$palette))
setdiff(paste0(df_d$package, ".", df_d$palette), names(pals_d))


df_d = df_d[df_d$palette != "contrast", ]

pals_d = do.call(c, palettes_d)

pals_d = pals_d[match(paste0(df_d$package, ".", df_d$palette), names(pals_d))]

pals_d = lapply(pals_d, function(x) x[!is.na(x)]) # ggthemes$stata_economist contains NA
names(pals_d) = df_d$palette



df_dyn = palettes_dynamic_names
df_dyn$type = ifelse(df_dyn$type %in% c("sequantial", "sequential"), "seq", ifelse(df_dyn$type == "divergent", "div", "cat"))


pals_dyn = lapply(do.call(c, palettes_dynamic), function(xs) {
	nmin = length(xs[[1]])
	nmax = length(xs[[length(xs)]])
	x = unique(unlist(rev(xs)))
	index = lapply(xs, function(xi) {
		match(xi, x)
	})
	names(index) = nmin:nmax
	attr(x, "index") = 	index
	x
})
names(pals_dyn) = df_dyn$palette



df_c = palettes_c_names
df_c$type = ifelse(df_c$type %in% c("sequantial", "sequential"), "seq", ifelse(df_c$type == "diverging", "div", "cat"))
df_c$n = ifelse(df_c$type == "cat", 36, ifelse(df_c$type == "seq", 9, 11))
pals_c = unname(do.call(mapply, c(as.list(df_c), list(SIMPLIFY = FALSE, FUN = function(package, palette, type, n) {
	if (n == 36) {
		xs = lapply(3:n, function(i) {
			substr(as.character(paletteer_c(paste0(package, "::", palette), n = i)), 1, 7)
		})
		x = unique(unlist(rev(xs)))
		index = lapply(xs, function(xi) {
			match(xi, x)
		})
		names(index) = 3:36
		attr(x, "index") = 	index
		x
	} else {
		substr(as.character(paletteer_c(paste0(package, "::", palette), n = n)), 1, 7)
	}
}))))
names(pals_c) = df_c$palette


pals = c(pals_c, pals_d, pals_dyn)
types = c(df_c$type, df_d$type, df_dyn$type)
series = c(df_c$package, df_d$package, df_dyn$package)
nms = paste(names(pals), series, sep = "_")
dat = c4a_data_as_is(pals,
					 types = types,
					 series = series)
saveRDS(dat, file = "build/paletteer.rds")

c4a_sysdata_remove(are.you.sure = T)

dat = readRDS("build/paletteer.rds")

series = sort(unique(dat$data$series))
description = structure(paste0("Palettes from the R package ", series), names = series)
dat$description = description

saveRDS(dat, file = "build/paletteer.rds")

c4a_sysdata_import(dat)

# https://github.com/EmilHvitfeldt/r-color-palettes

