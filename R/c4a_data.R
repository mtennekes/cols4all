#' Build and load palette data
#'
#' Build palette data. Both `c4a_data` and `c4a_data_as_is` build data palette. The difference is that the former processes the palettes (see below) whereas the latter takes the palette data as is. Data can subsequently be loaded into cols4all via `c4a_load`.
#'
#' In cols4all, palettes are organized by series and by type. The **series** or 'family' specifies where the palettes belong to. For instance `"brewer"` stands for the color palettes from ColorBrewer. In cols4all we distinguish 7 **types**:
#'
#' **`"cat"`** categorical
#' **`"seq"`** sequential
#' **`"div"`** diverging
#' **`"bivs`** bivariate (sequential x sequential)
#' **`"bivc`** bivariate (sequential x categorical)
#' **`"bivd`** bivariate (sequential x diverging)
#' **`"bivg`** bivariate (sequential x desaturated (grayscale))
#'
#' This function structures the palette data, such that it is consistent with the other palette data. This includes:
#'
#' * Palette names are made consistent. We use the convention `"my_series.my_palette"`, so all lower case, a period to separate the series name from the palette name, and underscores to separate words.
#' * (Only for `c4a_data`, bypassed for `c4a_data_as_is`)  Categorical palettes: black is removed from categorical palettes, and a grayscale color is assigned to be used for missing values (other grayscale colors are removed). Sequential palettes are sorted from light to dark.
#'
#' Indexing: for a categorical `"cat"` palette, an optional `"index"` attribute determines which colors to use for which lengths: if the palette consists of k colors, index should be a list of k, where the i-th element is an integer vector of length i with values 1,2,...,k. See `c4a_info("rainbow")` and  for an example.
#'
#' Range: sequential and diverging palettes are usually defined for 9+ colors. The optional `"range_matrix"` attribute determines that range is used for less colors. It is a n x 2 matrix where row i defines the applied range of a palette of length i. For sequential palettes a range `c(0,1)` means that the palette is generated (via a color ramp) between the two outermost colors. For diverging palettes, a range `c(x, y)` means that both sides of the palette are generated (via a color ramp) from `x`, which is the distance to the center color, to `y` which represents both outermost colors.
#'
#' The range is automatically set for sequential and diverging palettes that have no `"index"` or `"range_matrix"` attribute via the parameter `range_matrix_args`, which is a list per palette. The arguments for a sequential palette are: `nmin` the minimum number of colors for which the range is reduced, `nmax`, the number of colors for which the range is set to `c(0,1)`, `slope_min` and `slope_max` determine the slopes of range reduction from a palette of length `nmax` to `nmin`, and `space` sets the color space for which the color ramp is applied (`"rgb"` or `"Lab"`). The arguments for a diverging palette are the same, but only one `slope` is used (namely for the outermost colors).
#'
#' It may take some time to process, especially large categorical palettes, because of calculations of the color blind checks.
#'
#' @param x named list of color palettes. See details for indexing.
#' @param xNA colors for missing values. Vector of the same length as x (or length 1). For `NA` values, the color for missing values is automatically determined (preferable a light grayscale color, but if it is indistinguishable by color blind people, a light color with a low chroma value is selected)
#' @param types character vector of the same length as x (or length 1), which determines the type of palette: `"cat"`, `"seq"`, `"div"`, `"bivs"`, `"bivc"`, `"bivd"`, or `"bivg"`. See details.
#' @param series a character vector of the same length as x (or length 1), which determines the series.
#' @param nmin,nmax,ndef minimum / maximum / default number of colors for the palette. By default: `nmin = 1`, for `"cat"` `nmax` and `ndef` the number of supplied colors. For the other types, `nmax` is `Inf`. `ndef` is 7 for `"seq"`, 9. For diverging palettes, these numbers refer to the number of columns. (See `mmin`, `mmax`, `mdef` for the rows)
#' @param mmin,mmax,mdef minimum / maximum / default number of rows for bivariate palettes.
#' @param format.palette.name should palette names be formatted to lowercase/underscore format?
#' @param remove.blacks,take.gray.for.NA,remove.other.grays These arguments determine the processing of grayscale colors for categorical `"cat"` palettes: if `remove.blacks` and there are (near) blacks, these are removed first. Next, if `take.gray.for.NA`, `xNA` is `NA`, and a palette contains at least one grayscale color (which can also be white), this is used as color for missing values. In case there are more than one grayscale color, the lightest is taken. `remove.other.grays` determines what happens with the other grays.
#' @param light.to.dark should sequential `"seq"` palettes be automatically ordered from light to dark?
#' @param remove.names should individual color names be removed?
#' @param biv.method method to a create bivariate palette. Options are `"byrow"` means that the colors are wrapped row-wise to a color matrix where the number of rows and columns is automatically determined, `"byrowX"` the same but with X (integer between 2 and 9) columns, `"bycol"` and `"bycolX` similar but wrapped column-wise. `"div2seqseq"` and `"div2catseq` means that colors are extracted from a divering palette. The former translates colors into a matrix with the neutral color in the diagonal, while the latter places the neutral color in the middle column. `"seq2uncseq"`
#' @param space color space in which interpolated colors are determined. Options: `"rgb"` (RGB) and `"Lab"` (CIE Lab).
#' @param range_matrix_args list of lists, one for each palette. Each such list specifies the range of sequential and diverging palettes, in case they are not indexed. See details.
#' @param bib bibtex reference in the form of a `utils::bibentry` object.
#' @param ... passed on to `c4a_data`
#' @example ./examples/c4a_data.R
#' @rdname c4a_data
#' @name c4a_data
#' @export
c4a_data = function(x, xNA = NA, types = "cat", series = "x", nmin = NA, nmax = NA, ndef = NA, mmin = NA, mmax = NA, mdef = NA, format.palette.name = TRUE, remove.blacks = TRUE, take.gray.for.NA = TRUE, remove.other.grays = FALSE, light.to.dark = TRUE, remove.names = TRUE, biv.method = "byrow", space = "rgb", range_matrix_args = list(NULL), bib = NA) {

	if (!requireNamespace("colorblindcheck")) stop("Please install colorblindcheck")

	# check color list
	if (!is.list(x)) stop("x is not a list")
	nms = names(x)
	if (anyDuplicated(nms)) stop("Duplicated names found")
	x = lapply(x, validate_colors)

	# number of palettes
	k = length(x)

	# make everything length k (number of palettes)
	args = setdiff(ls(), c("x", "k"))
	length(range_matrix_args)

	# manual preprocessing
	if (!is.list(range_matrix_args[[1]])) range_matrix_args = list(range_matrix_args)

	nbib = if (is.na(bib) || inherits(bib, "bibentry")) 1 else k
	if (nbib == 1) bib = list(bib)

	for (arg in args) assign(arg, rep(get(arg), length.out = k), envir = environment())

	# validate na colors
	if (any(!is.na(xNA))) xNA[!is.na(xNA)] = validate_colors(xNA[!is.na(xNA)])

	# check types
	types_supported = unname(.C4A$types)
	if (!all(types %in% types_supported)) stop("Unknown types found. Currently only", paste(types_supported, collapse = ","), "are supported")



	lst = list(pal = x,
			   type = types,
			   colNA = xNA,
			   take.gray.for.NA = take.gray.for.NA,
			   remove.other.grays = remove.other.grays,
			   remove.blacks = remove.blacks,
			   light.to.dark = light.to.dark,
			   remove.names = remove.names,
			   biv.method = biv.method,
			   space = space,
			   range_matrix_args = range_matrix_args)


	res = do.call(mapply, c(list(FUN = process_palette, SIMPLIFY = FALSE), lst))
	x = lapply(res, "[[", "pal")
	xNA = sapply(res, "[[", "colNA", USE.NAMES = FALSE)
	reversed = sapply(res, "[[", "reversed")



	if (format.palette.name[1]) {
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
				message("Some palettes have been reversed (because of the cols4all convention that seq palettes are arranged from light to dark). Therefore they may have automatically be renamed. Please check and if needed change the argument settings of tm_series_add.\nOld names: ", paste(nms2[isdiff], collapse = ", "), "\nNew names: ", paste(ss2[isdiff], collapse = ", "))
			}
			if (any(!isdiff)) {
				message("Some palettes have been reversed (because of the cols4all convention that seq palettes are arranged from light to dark), but the names have not been changed. Please check and if needed change names manually.\nThe palettes are: ", paste(nms2[!isdiff], collapse = ", "), ".")
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

	z = data.frame(name = nms, series = series, fullname = fnms, type = types, palette = I(x), na = xNA, nmin = nmin, nmax = nmax, ndef = ndef, mmin = mmin, mmax = mmax, mdef = mdef)
	rownames(z) = NULL

	z$nmax = mapply(function(pal, type, nmax) {
		index = attr(pal, "index")
		if (!is.na(nmax)) {
			nmax
		} else if (type == "cat") {
			if (is.null(index)) length(pal) else length(index[[length(index)]])
		} else if (type == "bivc") {
			ncol(pal)
		} else {
			Inf
		}
	}, z$palette, z$type, z$nmax, SIMPLIFY = TRUE, USE.NAMES = FALSE)

	z$nmin = mapply(function(pal, nmin) {
		index = attr(pal, "index")
		if (!is.na(nmin)) {
			nmin
		} else if (is.null(index)){
			1
		} else {
			which(vapply(index, length, FUN.VALUE = integer(1)) != 0L)[1]
		}
	}, z$palette, z$nmin, SIMPLIFY = TRUE, USE.NAMES = FALSE)





	z$ndef = mapply(function(nmax, type) {
		if (!is.infinite(nmax)) nmax else unname(.C4A$ndef[type])
	}, z$nmax, z$type, USE.NAMES = FALSE)

	z$mmax = mapply(function(pal, mmax) {
		if (!is.na(mmax)) {
			mmax
		} else if (is.matrix(pal)) {
			Inf
		} else {
			1
		}
	}, z$palette, z$mmax, SIMPLIFY = TRUE, USE.NAMES = FALSE)
	z$mmin[is.na(z$mmin)] = 1L
	z$mdef = mapply(function(mmax, type) {
		if (!is.infinite(mmax)) mmax else unname(.C4A$mdef[type])
	}, z$mmax, z$type, USE.NAMES = FALSE)


	#z$ndef = ifelse(!is.infinite(z$nmax), z$nmax, ifelse(z$type == "seq", 7, ifelse(z$type == "div", 9, 3)))

	s = series_add_get_scores(z)


	.z = .C4A$z
	.s = .C4A$s
	.zbib = .C4A$zbib

	# add citations
	if (nbib == 1) {
		if (!is.na(bib[[1]])) {
			zb = bib[1]
			if (any(z$series != z$series[1])) stop("One bib item defined, while multiple series: bib items are organized by series and optionally palettes")
			zb[[1]]$name = z$series[1]
			names(zb) = z$series[1]
			if (z$series[1] %in% names(.zbib)) stop("Citation for series ", z$series[1], " already defined")
		} else {
			zb = NULL
		}
	} else {
		zb = mapply(function(b, nm) {
			names(b) = nm
			b
		}, bib, z$fullname, SIMPLIFY = FALSE)
		names(zb) = z$fullname
	}

	structure(list(data = z, scores = s, citation = zb), class = "c4a_data")
}

#' @rdname c4a_data
#' @name c4a_load
#' @param data cols4all data created with `c4a_data`
#' @export
c4a_load = function(data) {
	z = data$data
	s = data$scores
	zb = data$citation

	if (!is.null(.z)) {
		.z$bib = NULL
		.z$cit = NULL
		if (any(fnms %in% .z$fullname)) stop("Fulnames already exist: ", paste(intersect(fnms, .z$fullname), collapse = ", "))

		z = rbind(.z, z)
		s = abind::abind(.s, s, along=1)
	}

	zbib = do.call(c, c(list(.zbib), zb))

	.C4A$z = z
	.C4A$zbib = zbib
	.C4A$s = s
	attach_bib()
	fill_P()
	invisible(NULL)

}



#' @rdname c4a_data
#' @name c4a_data_as_is
#' @export
c4a_data_as_is = function(..., format.palette.name = FALSE, remove.blacks = FALSE, take.gray.for.NA = FALSE, remove.other.grays = FALSE, light.to.dark = FALSE, remove.names = FALSE) {

	args = c(list(...), list(format.palette.name = format.palette.name, take.gray.for.NA = take.gray.for.NA, remove.other.grays = remove.other.grays, remove.blacks = remove.blacks, light.to.dark = light.to.dark, remove.names = remove.names))
	do.call(c4a_data, args)
}




check_z = function(z) {
	name <- series <- fullname <- type <- nmax <- NULL
	if (!is.data.frame(z) || !setequal(c("name", "series", "fullname", "type", "palette", "na", "nmin", "nmax", "ndef", "mmin", "mmax", "mdef"), names(z))) stop("z should be a dataframe of colums: name, series, fullname, type, palette, na, nmin, nmax, ndef, mmin, mmax, and mdef")

	within(z, {
		if (!is.character(name)) stop("x$z$name should be a character column", call. = FALSE)
		if (!is.character(series)) stop("x$z$series should be a character column", call. = FALSE)
		if (!is.character(fullname)) stop("x$z$fullname should be a character column", call. = FALSE)
		if (!is.character(type)) stop("x$z$type should be a character column", call. = FALSE)
		if (!is.list(palette)) stop("x$z$palette should be a list column", call. = FALSE)
		if (!is.character(na) || all(is.na(na))) stop("x$z$na should be a character column", call. = FALSE)

		if (anyDuplicated(fullname)) stop("x$z$fullname should consist of unique values", call. = FALSE)
		if (!all(type %in% unname(.C4A$types))) stop("x$z$type should consist of", paste(unname(.C4A$types), collapse = ","), "values only", call. = FALSE)
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
