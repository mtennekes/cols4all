#' Get a cols4all color palette
#'
#' Get a cols4all color palette: `c4a` returns the colors of the specified palette, and `c4a_na` returns the color for missing value that is associated with the specified palette. Run \code{\link{c4a_gui}} to see all available palettes, which are also listed with \code{\link{c4a_palettes}}.
#'
#' @param palette name of the palette. See \code{\link{c4a_palettes}} for options. If omitted, the default palette is provided by `c4a_default_palette`. The palette name can be prefixed with a `"-"` symbol, which will reverse the palette (this can also be done with the `reverse` argument).
#' @param n number of colors. If omitted then: for type `"cat"` the maximum number of colors is returned, for types `"seq"` and `"div"`, 9 colors.
#' @param m number of rows in case type is `"bivs"`, `"bivc"`, `"bivd"` or  `"bivg"` (which stand for respectively sequential, categorical, diverging and desaturated (`g` for 'grayscale')).
#' @param type type of color palette, in case `palette` is not specified: one of `"cat"` (categorical/qualitative palette), `"seq"` (sequential palette), `"div"` (diverging palette), and `"bivs"`/`"bivc"`/`"bivd"`/`"bivg"` (bivariate: respectively seq-seq seq-cat, seq-div, and seq-desaturated).
#' @param reverse should the palette be reversed?
#' @param order order of colors. Only applicable for `"cat"` palettes
#' @param range a vector of two numbers between 0 and 1 that determine the range that is used for sequential and diverging palettes. The first number determines where the palette begins, and the second number where it ends. For sequential `"seq"` palettes, 0 means the leftmost (normally lightest) color, and 1 the rightmost (often darkest) color. For diverging `"seq"` palettes, 0 means the middle color, and 1 both extremes. If only one number is provided, this number is interpreted as the endpoint (with 0 taken as the start).
#' @param format format of the colors. One of: `"hex"` character vector of hex color values, `"RGB"` 3 column matrix of RGB values, or `"HCL"` 3-column matrix of HCL values
#' @param nm_invalid what should be done in case `n` or `m` is larger than the maximum number of colors or smaller than the minimum number? Options are `"error"` (an error is returned), `"repeat"`, the palette is repeated, `"interpolate"` colors are interpolated. For categorical `"cat"` palettes only.
#' @param verbose should messages be printed?
#' @return A vector of colors
#' @importFrom grDevices col2rgb colorRampPalette colors gray.colors rgb
#' @importFrom methods as
#' @importFrom stats na.omit
#' @example ./examples/c4a.R
#' @rdname c4a
#' @name c4a
#' @export
c4a = function(palette = NULL, n = NA, m = NA, type = c("cat", "seq", "div", "bivs", "bivc", "bivd", "bivg"), reverse = FALSE, order = NULL, range = NA, format = c("hex", "RGB", "HCL"), nm_invalid = c("error", "repeat", "interpolate"), verbose = TRUE) {
	calls = names(match.call(expand.dots = TRUE)[-1])

	type = match.arg(type)
	format = match.arg(format)

	nm_invalid = match.arg(nm_invalid)

	if (is.null(palette)) {
		palette = c4a_default_palette(type)
		if ("palette" %in% calls && verbose) message("Argument palette is specified as NULL, therefore returning default palette: \"", palette, "\"")
		mes = paste0("These are the colors from palette \"", palette, "\", the default for type \"", type, "\":")
	} else {
		mes = NULL
	}

	x = c4a_info(palette, verbose = verbose)

	if (is.null(x)) return(invisible(NULL))

	reverse = xor(reverse, x$reverse)

	if (is.na(n)) n = x$ndef
	if (is.na(m)) m = if (is.na(x$mdef)) n else x$mdef

	if (nm_invalid == "error") {
		tail_str = if (substr(type, 1, 3) == "biv") " columns of colors" else " colors"
		if (n > x$nmax) {
			if (x$nmax == x$nmin) {
				stop("Palette ", palette, " only supports ", x$nmax, tail_str)
			} else {
				stop("Palette ", palette, " only supports maximally ", x$nmax, tail_str)
			}
		} else if (n < x$nmin) {
			stop("Palette ", palette, " should only be used with a minimum of ", x$nmin, tail_str)
		}

		if (m > x$mmax) {
			if (x$mmax == x$mmin) {
				stop("Palette ", palette, " only supports ", x$mmax, " rows of colors.")
			} else {
				stop("Palette ", palette, " only supports maximally ", x$mmax, " rows of colors.")
			}
		} else if (m < x$mmin) {
			stop("Palette ", palette, " should only be used with a minimum of ", x$mmin, " rows of colors.")
		}
	}

	x$range = range
	if (is.na(n)) n = x$ndef
	if (is.na(m)) m = n
	#if (substr(type, 1, 3) == "biv" && is.na(m)) m = n
	x$nm_invalid = nm_invalid


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

#' Get information from a cols4all palette
#'
#' Get information from a cols4all palette
#'
#' @param palette name of the palette
#' @param no.match what happens is no match is found? Options: `"message"`: a message is thrown with suggestions, `"error"`: an error is thrown, `"null"`: `NULL` is returned
#' @param verbose should messages be printed?
#' @return list with the following items: name, series, fullname, type, palette (colors), na (color), nmax, and reverse. The latter is `TRUE` when there is a `"-"` prefix before the palette name.
#' @export
c4a_info = function(palette, no.match = c("message", "error", "null"), verbose = TRUE) {
	no.match = match.arg(no.match)
	isrev = (substr(palette, 1, 1) == "-")
	if (isrev) palette = substr(palette, 2, nchar(palette))

	z = .C4A$z

	fullname = c4a_name_convert(palette, no.match = no.match, verbose = verbose)

	if (is.null(fullname)) return(invisible(NULL))

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


	x = c4a_info(palette, verbose = verbose)

	if (!is.null(mes) && verbose) message(mes)

	x$na
}
