#' List all available cols4all color palettes
c4a_palettes = function(type = c("all", "cat", "seq", "div", "biv", "cyc")) {
	type = match.arg(type)
	z$name
}

c4a_defaults = c(cat = "tol.muted", seq = "hcl.blues", div = "hcl.purple-green")

#' Select a cols4all color palette
#'
#' Select a cols4all color palette
#'
#' @param palette name of the palette. See \code{\link{c4a_palettes}} for options
#' @param n
#'
c4a = function(palette = NULL, n = NULL, type = c("cat", "seq", "div", "biv", "cyc"), reverse = FALSE, order = NULL) {
	type = match.arg(type)

	if (is.null(palette)) palette = c4a_defaults[type]

	palid = which(palette == .z$name)
	if (!length(palid)) stop("Unknown palette. See c4a_palettes() for options, and c4a_show / c4a_gui to see them.")

	if (n > .z$nmax[palid]) stop("Palette ", name, " only supports ", .z$nmax[palid], " colors.")

	zl = as.list(.z[palid,])
	zl$palette = zl$palette[[1]]
	pal = do.call(get_pal_n, c(list(n = n), zl))

	pal = if (!is.null(order)) {
		if (!all(order %in% 1L:n)) stop("order should consist of numbers 1 to ", n)
		pal[order]
	} else pal

	if (reverse) rev(pal) else pal
}

