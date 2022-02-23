#' @rdname c4a
#' @name c4a_defaults
#' @export
c4a_defaults = c(cat = "tol.muted", seq = "hcl.blues2", div = "hcl.purple-green")

#' Get a cols4all color palette
#'
#' Get a cols4all color palette. The function `c4a` returns the colors of the specified palette, and the function `c4a_na` returns the color for missing value that is associated with the specified palette.
#'
#' @param palette name of the palette. See \code{\link{c4a_palettes}} for options. If omitted, the default palette is provided by `c4a_defaults`. The palette name can be prefixed with a `"-"` symbol, which will reverse the palette (this can also be done with the `reverse` argument).
#' @param n number of colors. If omitted then: for type `"cat"` the maximum number of colors is returned, and for types `"seq"` and `"div"`, 9 colors.
#' @param type type of color palette, in case `palette` is not specified: one of `"cat"` (categorical/qualitative palette), `"seq"` (sequential palette) and `"div"` (diverging palette).
#' @param reverse should the palette be reversed?
#' @param order order of colors. Only applicable for `"cat"` palettes
#' @param contrast vector of two numbers that determine the range that is used for sequential and diverging palettes. Both numbers should be between 0 and 1. The first number determines where the palette begins, and the second number where it ends. For sequential palettes, 0 means the leftmost (normally lightest) color, and 1 the rightmost (often darkest) color. For diverging palettes, 0 means the middle color, and 1 both extremes. If only one number is provided, this number is interpreted as the endpoint (with 0 taken as the start). By default, it is set automatically, based on `n`. See `c4a_gui`, or the internal functions `cols4all::default_contrast_seq` and `cols4all::default_contrast_div` to see what the automatic values are.
#' @param n_too_large what should be done in case `n` is larger than the maximum amount of colors? Options are `"error"` (an error is returned), `"repeat"`, the palette is repeated, `"interpolate"` colors are interpolated. For categorical `"cat"` palettes only.
#' @return a vector of colors
#' @importFrom grDevices col2rgb colorRampPalette colors grey.colors rgb
#' @importFrom methods as
#' @importFrom stats na.omit
#' @example ./examples/c4a.R
#' @rdname c4a
#' @name c4a
#' @export
c4a = function(palette = NULL, n = NULL, type = c("cat", "seq", "div"), reverse = FALSE, order = NULL, contrast = NULL, n_too_large = c("error", "repeat", "interpolate")) {
	type = match.arg(type)
	n_too_large = match.arg(n_too_large)

	if (is.null(palette)) {
		palette = c4a_defaults[type]
		mes = paste0("These are the colors from palette \"", palette, "\", the default for type \"", type, "\":")
	} else {
		mes = NULL
	}

	# palettes can also be reversed with a "-" sign
	isrev = (substr(palette, 1, 1) == "-")
	if (isrev) palette = substr(palette, 2, nchar(palette))
	reverse = xor(reverse, isrev)

	palid = which(palette == .z$fullname)
	if (!length(palid)) {
		palid = which(palette == .z$name)
		if (length(palid) > 1) {
			nms = .z$fullname[palid]
			message(paste0("Multiple palettes called \"", palette, " found: \"", paste(nms, collapse = "\", \""), "\". The first one, \"", nms[1], "\", is returned."))
			palid = palid[1]
		}
		if (!length(palid)) stop("Unknown palette. See c4a_palettes() for options, and c4a_show / c4a_gui to see them.")
	}

	if (length(palid) > 1) {
		warning("There are ", length(palid), " palettes with that name. The first one is taken")
		palid = palid[1]
	}

	if (!is.null(n) && n > .z$nmax[palid] && n_too_large == "error") stop("Palette ", palette, " only supports ", .z$nmax[palid], " colors.")

	zl = as.list(.z[palid,])
	zl$palette = zl$palette[[1]]
	zl$contrast = contrast
	zl$n_too_large = n_too_large
	if (is.null(n)) n = ifelse(zl$type == "cat", zl$nmax, 11)
	pal = do.call(get_pal_n, c(list(n = n), zl))

	pal = if (!is.null(order)) {
		if (!all(order %in% 1L:n)) stop("order should consist of numbers 1 to ", n)
		pal[order]
	} else pal

	if (!is.null(mes)) message(mes)
	if (reverse) rev(pal) else pal
}

#' @rdname c4a
#' @name c4a_na
#' @export
c4a_na = function(palette = NULL, type = c("cat", "seq", "div")) {
	type = match.arg(type)
	if (is.null(palette)) {
		palette = c4a_defaults[type]
		mes = paste0("This is the color for missing values associated with palette \"", palette, "\", the default for type \"", type, "\":")
	} else {
		mes = NULL
	}


	palid = which(palette == .z$fullname)
	if (!length(palid)) {
		palid = which(palette == .z$name)
		if (length(palid) > 1) {
			stop(paste0("Multiple palettes called \"", palette, " found. Please use the full name \"<series>.<name>\""))
		}
		if (!length(palid)) stop("Unknown palette. See c4a_palettes() for options, and c4a_show / c4a_gui to see them.")
	}

	if (!is.null(mes)) message(mes)

	.z$na[palid]
}
