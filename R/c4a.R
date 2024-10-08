#' Get a cols4all color palette
#'
#' Get a cols4all color palette: `c4a` returns the colors of the specified palette, and `c4a_na` returns the color for missing value that is associated with the specified palette. Run \code{\link{c4a_gui}} to see all available palettes, which are also listed with \code{\link{c4a_palettes}}.
#'
#' @param palette name of the palette. See \code{\link{c4a_palettes}} for options. If omitted, the default palette is provided by `c4a_default_palette`. The palette name can be prefixed with a `"-"` symbol, which will reverse the palette (this can also be done with the `reverse` argument).
#' @param n number of colors. If omitted then: for type `"cat"` the maximum number of colors is returned, for types `"seq"`, `"div"`, and `"cyc"`, 7 , 9, and 9 colors respectively.
#' @param m number of rows in case type is `"bivs"`, `"bivc"`, `"bivd"` or  `"bivg"` (which stand for respectively sequential, categorical, diverging and desaturated (`g` for 'grayscale')).
#' @param type type of color palette, in case `palette` is not specified: one of `"cat"` (categorical/qualitative palette), `"seq"` (sequential palette), `"div"` (diverging palette), `"cyc"` (cyclic palette), and `"bivs"`/`"bivc"`/`"bivd"`/`"bivg"` (bivariate: respectively seq-seq seq-cat, seq-div, and seq-desaturated).
#' @param reverse should the palette be reversed?
#' @param order order of colors. Only applicable for `"cat"` palettes
#' @param range a vector of two numbers between 0 and 1 that determine the range that is used for sequential and diverging palettes. The first number determines where the palette begins, and the second number where it ends. For sequential `"seq"` palettes, 0 means the leftmost (normally lightest) color, and 1 the rightmost (often darkest) color. For diverging `"seq"` palettes, 0 means the middle color, and 1 both extremes. If only one number is provided, this number is interpreted as the endpoint (with 0 taken as the start).
#' @param colorsort Sort the colors (`"cat"` only). Options: `"orig"` (original order), `"Hx"` (hue, where x is a starting number from 0 to 360), `"C"` (chroma), `"L"` (luminance)
#' @param format format of the colors. One of: `"hex"` character vector of hex color values, `"rgb"` 3 column matrix of RGB values, `"hcl"` 3-column matrix of HCL values, or one of the color classes from \code{\link[colorspace:color-class]{colorspace}}
#' @param nm_invalid what should be done in case `n` or `m` is larger than the maximum number of colors or smaller than the minimum number? Options are `"error"` (an error is returned), `"repeat"`, the palette is repeated, `"interpolate"` colors are interpolated. For categorical `"cat"` palettes only.
#' @param verbose should messages be printed?
#' @return A vector of colors (`c4a`) and a color (`c4a_na`)
#' @importFrom grDevices col2rgb colorRampPalette colors gray.colors rgb grey
#' @importFrom methods as
#' @importFrom spacesXYZ DeltaE
#' @importFrom stats na.omit
#' @example ./examples/c4a.R
#' @rdname c4a
#' @name c4a
#' @export
c4a = function(palette = NULL, n = NA, m = NA, type = c("cat", "seq", "div", "cyc", "bivs", "bivc", "bivd", "bivg"), reverse = FALSE, order = NULL, range = NA, colorsort = "orig", format = c("hex", "rgb", "hcl", "RGB", "XYZ", "HSV", "HLS", "LAB", "polarLAB", "LUV", "polarLUV"), nm_invalid = c("error", "repeat", "interpolate"), verbose = TRUE) {
	calls = names(match.call(expand.dots = TRUE)[-1])

	type = match.arg(type)

	if (identical(format, "HCL") && verbose) {
		message("As of cols4all 0.8, the formats \"RGB\" and \"HCL\" have been renamed to lower case (to prevent conflicts with the newly supported colorspace classes)")
		format = "hcl"
	}
	if (identical(format, "RGB") && verbose) {
		message("As of cols4all 0.8, the formats \"RGB\" and \"HCL\" have been renamed to lower case (to prevent conflicts with the newly supported colorspace classes)")
	}

	format = match.arg(format)

	nm_invalid = match.arg(nm_invalid)

	if (is.null(palette)) {
		palette = c4a_default_palette(type)
		if ("palette" %in% calls && verbose) message("Argument palette is specified as NULL, therefore returning default palette: \"", palette, "\"")
		mes = paste0("These are the colors from palette \"", palette, "\", the default for type \"", type, "\":")
	} else {
		mes = NULL
	}

	x = c4a_info(palette, verbose = verbose, no.match = {if (verbose) "message" else "null"})

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
				stop("Palette ", palette, " only supports up to ", x$nmax, tail_str)
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


	pal = do.call(get_pal_n, c(list(n = n, m = m, colorsort = colorsort), x))

	pal = if (!is.null(order)) {
		if (!all(order %in% 1L:n)) stop("order should consist of numbers 1 to ", n)
		pal[order]
	} else pal

	if (!is.null(mes) && verbose) message(mes)
	pal2 = if (reverse) rev(pal) else pal

	if (format == "hex") {
		pal2
	} else {
		cols = colorspace::hex2RGB(pal2)

		if (format == "rgb") {
			cols@coords * 255
		} else if (format == "hcl") {
			as(cols, "polarLUV")@coords[,c("H", "C", "L"), drop = FALSE]
		} else {
			as(cols, format)
		}
	}
}

#' @param space a character string; interpolation in RGB or CIE Lab color spaces
#' @param interpolate use spline or linear interpolation
#' @param ... passed on to `c4a`.
#' @rdname c4a
#' @name c4a_ramp
#' @export
c4a_ramp = function(..., space = c("rgb", "Lab"),
					interpolate = c("linear", "spline")) {
	space = match.arg(space)
	interpolate = match.arg(interpolate)
	args = list(...)
	pal = do.call(c4a, args)
	if (is.null(pal)) return(invisible(NULL))
	colorRampPalette(pal, space = space, interpolate = interpolate)
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
	if (!is.character(palette)) stop("palette should be a character value", call. = FALSE)
	if (length(palette) != 1L) stop("palette should be a character value (so length 1)", call. = FALSE)

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

	structure(x, class = c("c4a_info", "list"))
}




#' @rdname c4a
#' @name c4a_na
#' @export
c4a_na = function(palette = NULL, type = c("cat", "seq", "div", "cyc", "bivs", "bivc", "bivd", "bivg"), verbose = TRUE) {
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


get_zp = function(p, n = NA) {
	x = c4a_info(p, no.match, verbose)
	if (is.na(n)) {
		n = x$ndef
	}
	z = data.frame(name = x$name, series = x$series, fullname = x$fullname, type = x$type, n = n)
}

#' Get information from a cols4all palette
#'
#' Get information from a cols4all palette
#'
#' @param palette name of the palette
#' @param n number of colors
#' @param no.match what happens is no match is found? Options: `"message"`: a message is thrown with suggestions, `"error"`: an error is thrown, `"null"`: `NULL` is returned
#' @param verbose should messages be printed?
#' @return list with the following items: name, series, fullname, type, palette (colors), na (color), nmax, and reverse. The latter is `TRUE` when there is a `"-"` prefix before the palette name.
#' @export
#' @example examples/c4a_scores.R
c4a_scores = function(palette = NULL, type = NULL, series = NULL, n = NA, no.match = c("message", "error", "null"), verbose = TRUE) {
	if (!is.null(palette)) {
		z = get_zp(palette, n)
	} else {
		if (is.null(type)) stop("Please specify either palette or type (optionally in combination with series)")
		if (is.na(n)) n = .C4A$ndef[[type]]
		pals = c4a_palettes(type = type, series = series)
		z = do.call(rbind, lapply(pals, get_zp))

	}
	show_attach_scores(z)
}




