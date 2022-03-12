#' Get a cols4all color palette
#'
#' Get a cols4all color palette: `c4a` returns the colors of the specified palette, and `c4a_na` returns the color for missing value that is associated with the specified palette. Run \code{\link{c4a_gui}} to see all available palettes, which are also listed with \code{\link{c4a_palettes}}.
#'
#' @param palette name of the palette. See \code{\link{c4a_palettes}} for options. If omitted, the default palette is provided by `c4a_default_palette`. The palette name can be prefixed with a `"-"` symbol, which will reverse the palette (this can also be done with the `reverse` argument).
#' @param n number of colors. If omitted then: for type `"cat"` the maximum number of colors is returned, for types `"seq"` and `"div"`, 9 colors.
#' @param m number of rows in case type is `"bivs"` or `"bivc"`.
#' @param type type of color palette, in case `palette` is not specified: one of `"cat"` (categorical/qualitative palette), `"seq"` (sequential palette), `"div"` (diverging palette), and `"bivs"`/`"bivc"` (bivariate, where the former is seq-seq and the latter cat-seq).
#' @param reverse should the palette be reversed?
#' @param order order of colors. Only applicable for `"cat"` palettes
#' @param range a vector of two numbers between 0 and 1 that determine the range that is used for sequential and diverging palettes. The first number determines where the palette begins, and the second number where it ends. For sequential `"seq"` palettes, 0 means the leftmost (normally lightest) color, and 1 the rightmost (often darkest) color. For diverging `"seq"` palettes, 0 means the middle color, and 1 both extremes. If only one number is provided, this number is interpreted as the endpoint (with 0 taken as the start). The default values (that depend on the `n`n and `type`) are provided by \code{\link{c4a_default_range}}.
#' @param format format of the colors. One of: `"hex"` character vector of hex color values, `"RGB"` 3 column matrix of RGB values, or `"HCL"` 3-column matrix of HCL values
#' @param n_too_large what should be done in case `n` is larger than the maximum amount of colors? Options are `"error"` (an error is returned), `"repeat"`, the palette is repeated, `"interpolate"` colors are interpolated. For categorical `"cat"` palettes only.
#' @param verbose should messages be printed?
#' @return A vector of colors
#' @importFrom grDevices col2rgb colorRampPalette colors gray.colors rgb
#' @importFrom methods as
#' @importFrom stats na.omit
#' @example ./examples/c4a.R
#' @rdname c4a
#' @name c4a
#' @export
c4a = function(palette = NULL, n = NULL, m = NULL, type = c("cat", "seq", "div", "bivs", "bivc"), reverse = FALSE, order = NULL, range = NA, format = c("hex", "RGB", "HCL"), n_too_large = c("error", "repeat", "interpolate"), verbose = TRUE) {
	calls = names(match.call(expand.dots = TRUE)[-1])

	type = match.arg(type)
	format = match.arg(format)

	n_too_large = match.arg(n_too_large)

	if (is.null(palette)) {
		palette = c4a_default_palette(type)
		if ("palette" %in% calls) message("Argument palette is specified as NULL, therefore returning default palette: \"", palette, "\"")
		mes = paste0("These are the colors from palette \"", palette, "\", the default for type \"", type, "\":")
	} else {
		mes = NULL
	}

	x = c4a_meta(palette)

	reverse = xor(reverse, x$reverse)


	if (!is.null(n) && n > x$nmax && n_too_large == "error") stop("Palette ", palette, " only supports ", x$nmax, " colors.")

	x$range = range
	x$n_too_large = n_too_large
	if (is.null(n)) n = ifelse(x$type == "cat", x$nmax, ifelse(x$type %in% c("bivs", "bivc"), 3, 11))
	if (type %in% c("bivs", "bivc") && is.null(m)) m = n


	pal = do.call(get_pal_n, c(list(n = n, m = m), x))

	pal = if (!is.null(order)) {
		if (!all(order %in% 1L:n)) stop("order should consist of numbers 1 to ", n)
		pal[order]
	} else pal

	if (!is.null(mes) && verbose) message(mes)
	pal2 = if (reverse) rev(pal) else pal

	if (format == "hex") {
		pal2
	} else if (format == "RGB") {
		colorspace::hex2RGB(pal)@coords * 255
	} else if (format == "HCL") {
		get_hcl_matrix(pal)
	}
}

#' Get meta information from a cols4all palette
#'
#' Get meta information from a cols4all palette
#'
#' @param palette name of the palette
#' @param no.match what happens is no match is found? Options: `"error"`: an error is thrown, `"null"`: `NULL` is returned
#' @return list with the following items: name, series, fullname, type, palette (colors), na (color), nmax, and reverse. The latter is `TRUE` when there is a `"-"` prefix before the palette name.
#' @export
c4a_meta = function(palette, no.match = c("error", "null")) {
	isrev = (substr(palette, 1, 1) == "-")
	if (isrev) palette = substr(palette, 2, nchar(palette))

	z = .C4A$z

	fullname = c4a_name_convert(palette, no.match = no.match)

	if (is.null(fullname)) return(NULL)

	palid = which(fullname == z$fullname)

	x = as.list(z[palid, ])
	x$reverse = isrev
	x$palette = x$palette[[1]]
	x
}




#' @rdname c4a
#' @name c4a_na
#' @export
c4a_na = function(palette = NULL, type = c("cat", "seq", "div"), verbose = TRUE) {
	type = match.arg(type)
	if (is.null(palette)) {
		palette = c4a_default_palette(type)
		mes = paste0("This is the color for missing values associated with palette \"", palette, "\", the default for type \"", type, "\":")
	} else {
		mes = NULL
	}

	z = .C4A$z

	palid = which(palette == z$fullname)
	if (!length(palid)) {
		palid = which(palette == z$name)
		if (length(palid) > 1) {
			stop(paste0("Multiple palettes called \"", palette, " found. Please use the full name \"<series>.<name>\""))
		}
		if (!length(palid)) stop("Unknown palette. See c4a_palettes() for options, and c4a_table / c4a_gui to see them.")
	}

	if (!is.null(mes) && verbose) message(mes)

	z$na[palid]
}
