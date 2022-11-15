attach_bib = function() {
	z = .C4A$z
	zbib = .C4A$zbib

	if (is.null(zbib)) return(NULL)

	bnames = names(zbib)
	split_names = lapply(bnames, function(bn) {
		strsplit(bn, split = ".", fixed = TRUE)[[1]][-1]
	})
	split_series = sapply(bnames, function(bn) {
		strsplit(bn, split = ".", fixed = TRUE)[[1]][1]
	})


	nms = mapply(function(x,y) {
		if (length(x)) paste(y, x, sep = ".") else y
	}, split_names, split_series, SIMPLIFY = FALSE)

	match(z$series, split_series)

	ids = mapply(function(fn, s) {
		id = which(sapply(nms, function(nm) {
			any(fn %in% nm | s %in% nm)
		}))
		if (!length(id)) id = 0
		id
	}, z$fullname, z$series, SIMPLIFY = TRUE, USE.NAMES = FALSE)

	zbib_cit = sapply(zbib, format, style = "text", USE.NAMES = FALSE)
	zbib_bib = sapply(zbib, format, style = "Bibtex", USE.NAMES = FALSE)

	ids[ids==0] = NA

	z$cit = zbib_cit[ids]
	z$bib = zbib_bib[ids]
	.C4A$z = z
	NULL
}
