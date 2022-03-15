#' Add and remove palette series
#'
#' Add and remove palette series. `c4a_series_add` can be used to add own palette series and  `c4a_series_remove` to remove palette series. `c4a_series_add_as_is` is the same as `c4a_series_add`, but by default without any processing. These functions require the R package `colorblindcheck`.
#'
#' Indexing: for a categorical `"cat"` palette, an optional `"index"` attribute determines which colors to use for which lengths: if the palette consists of k colors, index should be a list of k, where the i-th element is an integer vector of length i with values 1,2,...,k. See `cols4all::.z$palette$rainbow` and  for an example.
#'
#' It may take some time to process, especially large categorical palettes, because of calculations of the color blind checks
#'
#' @param x named list of color palettes. See details for indexing.
#' @param xNA colors for missing values. Vector of the same length as x (or length 1). For `NA` values, the color for missing values is automatically determined (preferable a light grayscale color, but if it is indistinguishable by color blind people, a light color with a low chroma value is selected)
#' @param types character vector of the same length as x (or length 1), which determines the type of palette `"cat"`, `"seq"`, or `"div"`.
#' @param series for `c4a_series_add`, a character vector of the same length as x (or length 1), which determines the series. For `c4a_series_remove` a character vector of series names that should be removed (use `"all"` to remove all).
#' @param format.palette.name should palette names be formatted to lowercase/underscore format?
#' @param remove.blacks,take.gray.for.NA,remove.other.grays These arguments determine the processing of grayscale colors for categorical `"cat"` palettes: if `remove.blacks` and there are (near) blacks, these are removed first. Next, if `take.gray.for.NA`, `xNA` is `NA`, and a palette contains at least one grayscale color (which can also be white), this is used as color for missing values. In case there are more than one grayscale color, the lightest is taken. `remove.other.grays` determines what happens with the other grays.
#' @param light.to.dark should sequential `"seq"` palettes be automatically ordered from light to dark?
#' @param remove.names should individual color names be removed?
#' @param biv.method method to a create bivariate palette. Options are `"byrow"` means that the colors are wrapped row-wise to a color matrix where the number of rows and columns is automatically determined, `"byrowX"` the same but with X (integer between 2 and 9) columns, `"bycol"` and `"bycolX` similar but wrapped column-wise. `"div2seqseq"` and `"div2catseq` means that colors are extracted from a divering palette. The former translates colors into a matrix with the neutral color in the diagonal, while the latter places the neutral color in the middle column. `"seq2uncseq"`
#' @param are.you.sure are you sure you want to remove series?
#' @param ... passed on to `c4a_series_add`
#' @example ./examples/c4a_series_add.R
#' @rdname c4a_series_add
#' @name c4a_series_add
#' @export
c4a_series_add = function(x, xNA = NA, types, series, format.palette.name = TRUE, remove.blacks = TRUE, take.gray.for.NA = TRUE, remove.other.grays = FALSE, light.to.dark = TRUE, remove.names = TRUE, biv.method = "byrow") {

	if (!requireNamespace("colorblindcheck")) stop("Please install colorblindcheck")


	adjust.settings = list(take.gray.for.NA = take.gray.for.NA, remove.other.grays = remove.other.grays, remove.blacks = remove.blacks, light.to.dark = light.to.dark, remove.names = remove.names, biv.method = biv.method)

	k = length(x)
	types = rep(types, length.out = k)
	series = rep(series, length.out = k)

	xNA = rep(xNA, length.out = k)

	if (!is.list(x)) stop("x is not a list")

	nms = names(x)

	if (anyDuplicated(nms)) stop("Duplicated names found")

	x = lapply(x, validate_colors)

	if (any(!is.na(xNA))) xNA[!is.na(xNA)] = validate_colors(xNA[!is.na(xNA)])

	if (!all(types %in% c("div", "seq", "cat", "bivs", "bivc", "bivu"))) stop("Unknown types found. Currently only \"cat\", \"seq\", \"div\", \"bivs\", \"bivc\", and \"bivu\" are supported")


	res = mapply(process_palette, pal = x, type = types, colNA = xNA, SIMPLIFY = FALSE, MoreArgs = adjust.settings)
	x = lapply(res, "[[", "pal")
	xNA = sapply(res, "[[", "colNA", USE.NAMES = FALSE)
	reversed = sapply(res, "[[", "reversed")



	if (format.palette.name) {
		nms = format_name(nms)
		if (any(reversed)) {
			nms2 = nms[reversed]
			ss = strsplit(nms2, "_", fixed = TRUE)
			ss2 = sapply(ss, function(s) {
				s2 = if (length(s) == 2 && !(s[1] %in% c("light", "dark"))) rev(s) else s
				paste(s2, collapse = "_")
			}, USE.NAMES = FALSE)
			isdiff = (ss2 != nms2)
			if (any(isdiff)) {
				message("Some palettes have been reversed. Therefore they may have automatically be renamed. Please check and if needed change the argument settings of tm_series_add.\nOld names: ", paste(nms2[isdiff], collapse = ", "), "\nNew names: ", paste(ss2[isdiff], collapse = ", "))
			}
			if (any(!isdiff)) {
				message("Some palettes have been reversed, but the names have not been changed. Please check and if needed change names manually.\nThe palettes are: ", paste(nms2[!isdiff], collapse = ", "), ".")
			}
			nms[reversed] = ss2
		}
	} else {
		if (any(reversed)) {
			nms2 = nms[reversed]
			message("Some palettes have been reversed, but the names have not been changed. Please check and if needed change names manually or try with format.palette.name.\nThe palettes are: ", paste(nms2, collapse = ", "), ".")
		}
	}


	seriesID = which(series != "")
	fnms = nms
	if (length(seriesID)) fnms[seriesID] = paste0(series[seriesID], ".", fnms[seriesID])

	names(x) = nms

	z = data.frame(name = nms, series = series, fullname = fnms, type = types, palette = I(x), na = xNA)
	rownames(z) = NULL

	z$nmax = mapply(function(pal, type) {
		if (type == "cat") {
			index = attr(pal, "index")
			if (is.null(index)) length(pal) else length(index[[length(index)]])
		} else {
			Inf
		}
	}, z$palette, z$type, SIMPLIFY = TRUE, USE.NAMES = FALSE)

	s = series_add_get_scores(z)


	.z = .C4A$z
	.s = .C4A$s

	if (!is.null(.z)) {
		if (any(fnms %in% .z$fullname)) stop("Fulnames already exist: ", paste(intersect(fnms, .z$fullname), collapse = ", "))

		z = rbind(.z, z)
		s = abind::abind(.s, s, along=1)
	}

	#cfa = structure(list(z = z, s = s), class = "c4a")

	.C4A$z = z
	.C4A$s = s
	fill_P()
	invisible(NULL)
}

#' @rdname c4a_series_add
#' @name c4a_series_add_as_is
#' @export
c4a_series_add_as_is = function(..., format.palette.name = FALSE, remove.blacks = FALSE, take.gray.for.NA = FALSE, remove.other.grays = FALSE, light.to.dark = FALSE, remove.names = FALSE) {

	args = c(list(...), list(format.palette.name = format.palette.name, take.gray.for.NA = take.gray.for.NA, remove.other.grays = remove.other.grays, remove.blacks = remove.blacks, light.to.dark = light.to.dark, remove.names = remove.names))
	do.call(c4a_series_add, args)
}




check_z = function(z) {
	name <- series <- fullname <- type <- nmax <- NULL
	if (!is.data.frame(z) || !setequal(c("name", "series", "fullname", "type", "palette", "na", "nmax"), names(z))) stop("z should be a dataframe of colums: name, series, fullname, type, palette, na, and nmax")

	within(z, {
		if (!is.character(name)) stop("x$z$name should be a character column", call. = FALSE)
		if (!is.character(series)) stop("x$z$series should be a character column", call. = FALSE)
		if (!is.character(fullname)) stop("x$z$fullname should be a character column", call. = FALSE)
		if (!is.character(type)) stop("x$z$type should be a character column", call. = FALSE)
		if (!is.list(palette)) stop("x$z$palette should be a list column", call. = FALSE)
		if (!is.character(na) || all(is.na(na))) stop("x$z$na should be a character column", call. = FALSE)

		if (anyDuplicated(fullname)) stop("x$z$fullname should consist of unique values", call. = FALSE)
		if (!all(type %in% c("cat", "seq", "div", "bivs", "bivc", "bivu"))) stop("x$z$type should consist of \"cat\", \"seq\", \"div\", \"bivs\", \"bivc\" and \"bivu\" values only", call. = FALSE)
		if (!is.numeric(nmax)) stop("x$z$nmax should be a numeric column", call. = FALSE)

		palette = I(lapply(palette, validate_colors))
		if (any(!is.na(na))) na[!is.na(na)] = validate_colors(na[!is.na(na)])
	})
}

check_s = function(s, n) {
	if (!is.array(s)) stop("x$s is not an array", call. = FALSE)
	d = dim(s)

	if (d[1] != n) stop("number of rows (first dim) of x$s does not correspond to the number of rows in x$z", call. = FALSE)
	if (!setequal(dimnames(s)[[2]], .C4A$sc)) stop("columns (second dim) in x$s should correspond to", paste(.C4A$sc, collapse = ","), call. = FALSE)
	if (d[3] != max(.C4A$nmax)) stop("Third dimension of x$s should be", d[3], call. = FALSE)
	s
}


#' @rdname c4a_series_add
#' @name c4a_series_remove
#' @export
c4a_series_remove = function(series = "all", are.you.sure = FALSE) {
	if (!are.you.sure) stop("Please set are.you.sure to TRUE if you are", call. = FALSE)

	z = .C4A$z
	s = .C4A$s

	sel = if (series[1] == "all") TRUE else (z$series %in% series)

	if (all(sel)) {
		message("all cols4all series removed")

		z2 = NULL
		s2 = NULL
	} else {
		message("cols4all series \"", paste(series, collapse = "\", \""), "\" removed")

		z2 = z[!sel, ]
		s2 = s[!sel, ,]
	}
	.C4A$z = z2
	.C4A$s = s2
	fill_P()
	invisible(NULL)
}
