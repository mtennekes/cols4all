library(cranly)
cran_db <- clean_CRAN_db()
package_network <- build_network(cran_db)


imports = c("methods", "grDevices", "stats", "abind", "png", "stringdist", "colorspace")
suggs = c("colorblindcheck", "kableExtra", "knitr", "shiny", "shinyjs", "ggplot2", "scales", "rmarkdown")


ilist = lapply(imports, function(x) compute_dependence_tree(package_network, x))
ilist = mapply(function(x,y) {
	x$source = y
	x
}, ilist, imports, SIMPLIFY = FALSE)

slist = lapply(suggs, function(x) compute_dependence_tree(package_network, x))
slist = mapply(function(x,y) {
	x$source = y
	x
}, slist, suggs, SIMPLIFY = FALSE)

x = data.table::rbindlist(c(ilist, slist))
x


cat(paste(paste0("  - name: ", sort(unique(x$package))), collapse = "\n"))

View(x)
