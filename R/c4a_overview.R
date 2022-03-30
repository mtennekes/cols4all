c4a_overview = function(type) {
	z = .C4A$z

	zt = z[z$type == type, ]

	zt$series = as.factor(zt$series)

	np = tapply(zt$name, INDEX = zt$series, FUN = length)
	nmin = tapply(zt$nmin, INDEX = zt$series, FUN = min)
	nmax = tapply(zt$nmax, INDEX = zt$series, FUN = max)

	df = data.frame(Series = levels(zt$series), np = np, nmin = nmin, nmax = nmax)
	rownames(df) = NULL

	kableExtra::kbl(df, col.names = c("Series", "Number of palettes", "Min. number of colors", "Max. number of colors"))
}
