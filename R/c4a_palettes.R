#' Get available palette names and series
#'
#' `c4a_palettes` lists all available cols4all color palettes. Palettes are organized by series. The available series are listed with `c4a_series`. Palettes are also organized per functional type, where we currently support: categorical `"cat"`, sequential `"seq"`, and diverging `"div"`" palette types. The function `c4a_types` lists all available types. The function `c4a_overview` gives an overview table of the number of palette per series and type. In an IDE with auto-completion (such as RStudio) it is possible to browse through the palette names with `.P` (using `$` like in lists).
#'
#' @param type type of color palette: one of `"all"` (all palettes), `"cat"` (categorical/qualitative palettes), `"seq"` (sequential palettes) and `"div"` (diverging palettes).
#' @param series series to list the palettes from. Run `c4a_series` to see the options.
#' @param full.names should full names, i.e. with the prefix "series."? By default `TRUE`.
#' @return names of the loaded color palettes
#' @example ./examples/c4a_palettes.R
#' @seealso References of the palettes: \code{\link{cols4all-package}}.
#' @rdname c4a_palettes
#' @name c4a_palettes
#' @export
c4a_palettes = function(type = c("all", "cat", "seq", "div"), series = NULL, full.names = TRUE) {
	type = match.arg(type)
	z = .C4A$z
	nms = if (full.names) z$fullname else z$name
	sel_type = if (type != "all") z$type == type else TRUE
	sel_series = if (is.null(series)) TRUE else (z$series %in% series)
	nms[sel_type & sel_series]
}

#' @rdname c4a_palettes
#' @name c4a_series
#' @export
c4a_series = function(type = c("all", "cat", "seq", "div")) {
	type = match.arg(type)
	z = .C4A$z
	series = z$series
	unique({if (type != "all") series[z$type == type] else series})
}

c4a_default_palette = function(type) {
	if (!(type %in% .C4A$types)) stop("Unknown palette type. These are supported: ", paste(.C4A$types, collapse = ", "))
	unname(.C4A$defaults[type])
}

#' @rdname c4a_palettes
#' @name c4a_series
#' @export
c4a_types = function(series = NULL) {

	tps = unname(.C4A$types)

	z = .C4A$z

	if (!is.null(series)) {
		intersect(tps, z$type[z$series %in% series])
	} else {
		tps
	}
}

#' @rdname c4a_palettes
#' @name c4a_series
#' @export
c4a_overview = function() {
	z = .C4A$z
	z = z[order(z$fullname), ]

	tps = unname(.C4A$types)

	tab_k = as.data.frame(tapply(z$nmin, INDEX = list(z$series, factor(z$type, levels = tps)), FUN = length))
	tab_k$series = rownames(tab_k)
	rownames(tab_k) = NULL
	tab_k[, c("series", tps)]
}


#' @rdname c4a_palettes
#' @name .P
#' @export
.P <- new.env(FALSE, parent=globalenv())
