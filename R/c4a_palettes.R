#' Get available palette names and series
#'
#' `c4a_palettes` lists all available cols4all color palettes. Palettes are organized by series. The available series are listed with `c4a_series`. Palettes are also organized per functional type, where we currently support: categorical `"cat"`, sequential `"seq"`, diverging `"div"`", cyclic `"cyc"`, and bivariate (seq x seq `"bivs"`, seq x cat `"bivc"`, seq x div `"bivd"`, seq x desaturated `"bivg"`) palette types. The function `c4a_types` lists all available types. The function `c4a_overview` gives an overview table of the number of palette per series and type. In an IDE with auto-completion (such as RStudio) it is possible to browse through the palette names with `.P` (using `$` like in lists).
#'
#' @param type type of color palette: one of `"all"` (all palettes), `"cat"`, `"seq"`, `"div"`, `"cyc"`, `"bivs"`, `"bivc"`, `"bivd"`, or `"bivg"`. See \code{\link{c4a_types}} for descriptions.
#' @param series series to list the palettes from. Run `c4a_series` to see the options.
#' @param full.names should full names, i.e. with the prefix "series."? By default `TRUE`.
#' @param as.data.frame should `c4a_series` and `c4a_types` return the result as a data.frame, with description included as a column?
#' @return names of the loaded color palettes
#' @example ./examples/c4a_palettes.R
#' @seealso References of the palettes: \code{\link{cols4all-package}}.
#' @rdname c4a_palettes
#' @name c4a_palettes
#' @export
c4a_palettes = function(type = c("all", "cat", "seq", "div", "cyc", "bivs", "bivc", "bivd", "bivg"), series = NULL, full.names = TRUE) {
	type = match.arg(type)
	z = .C4A$z
	if (is.null(z)) {
		message("No palettes loaded")
		return(invisible(NULL))
	}

	nms = if (full.names) z$fullname else z$name
	sel_type = if (type != "all") z$type == type else TRUE
	sel_series = if (is.null(series)) TRUE else (z$series %in% series)
	nms[sel_type & sel_series]
}

#' @rdname c4a_palettes
#' @name c4a_series
#' @export
c4a_series = function(type = c("all", "cat", "seq", "div", "cyc"), as.data.frame = TRUE) {
	type = match.arg(type)
	z = .C4A$z
	if (is.null(z)) {
		message("No palettes loaded")
		return(invisible(NULL))
	}
	series = z$series
	x = sort(unique({if (type != "all") series[z$type == type] else series}))

	if (as.data.frame) {
		data.frame(series = x, description = unname(.C4A$zdes[x]))
	} else {
		x
	}
}

c4a_default_palette = function(type) {
	if (!(type %in% .C4A$types)) stop("Unknown palette type. These are supported: ", paste(.C4A$types, collapse = ", "))
	unname(.C4A$defaults[type])
}

#' @rdname c4a_palettes
#' @name c4a_series
#' @export
c4a_types = function(series = NULL, as.data.frame = TRUE) {

	tps = unname(.C4A$types)

	z = .C4A$z
	if (is.null(z)) {
		message("No palettes loaded")
		return(invisible(NULL))
	}

	x = if (!is.null(series)) {
		intersect(tps, z$type[z$series %in% series])
	} else {
		tps
	}

	if (as.data.frame) {
		.C4A$type_info[match(x, .C4A$type_info$type),]
	} else {
		x
	}

}

#' @rdname c4a_palettes
#' @param return.matrix should only a matrix be returned with numbers per palette and type? If `FALSE` a data.frame is returned with addional information
#' @param zero.count.as.NA should zeros counted in the table be returned as 0 (`FALSE`, default) or as `NA` (`TRUE`)?
#' @name c4a_series
#' @export
c4a_overview = function(return.matrix = FALSE, zero.count.as.NA = FALSE) {
	z = .C4A$z

	if (is.null(z)) {
		message("No palettes loaded")
		return(invisible(NULL))
	}

	z = z[order(z$fullname), ]

	tps = unname(.C4A$types)

	tab_k = tapply(z$nmin, INDEX = list(z$series, factor(z$type, levels = tps)), FUN = length)
	if (!zero.count.as.NA) tab_k[is.na(tab_k)] = 0L

	if (return.matrix) return(tab_k)

	df_k = as.data.frame(tab_k)

	df_k$series = rownames(df_k)
	df_k$description = ""

	if (!is.null(.C4A$zdes)) {
		mtch = intersect(df_k$series, names(.C4A$zdes))
		df_k$description[match(mtch, df_k$series)] = unname(.C4A$zdes[mtch])
	}
	rownames(df_k) = NULL

	df_k[, c("series", "description", tps)]
}


#' @rdname c4a_palettes
#' @name .P
#' @export
.P <- new.env(FALSE, parent=globalenv())
