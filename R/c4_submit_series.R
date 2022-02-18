format_name = function(x) {
	x2 = gsub("[-, \\,, (, ), \\ ]",  "", x)
	tolower(gsub("([a-z])([A-Z])", "\\1_\\L\\2", x2, perl = TRUE))
}


c4a_submit_series_as_is = function(..., format.palette.name = FALSE, from.scratch = FALSE, take_grey_for_NA = FALSE, remove_other_greys = FALSE, remove_blacks = FALSE, light_to_dark = FALSE, remove.names = FALSE) {

	args = c(list(...), list(format.palette.name = format.palette.name, from.scratch = from.scratch, take_grey_for_NA = take_grey_for_NA, remove_other_greys = remove_other_greys, remove_blacks = remove_blacks, light_to_dark = light_to_dark, remove.names = remove.names))
	do.call(c4a_submit_series, args)
}


c4a_submit_series = function(x, xNA = NA, types, series, format.palette.name = TRUE, from.scratch = FALSE, take_grey_for_NA = TRUE, remove_other_greys = FALSE, remove_blacks = TRUE, light_to_dark = TRUE, remove.names = TRUE) {


	adjust.settings = list(take_grey_for_NA = take_grey_for_NA, remove_other_greys = remove_other_greys, remove_blacks = remove_blacks, light_to_dark = light_to_dark, remove.names = remove.names)

	k = length(x)
	types = rep(types, length.out = k)
	series = rep(series, length.out = k)

	xNA = rep(xNA, length.out = k)

	if (!is.list(x)) stop("x is not a list")

	nms = names(x)

	if (anyDuplicated(nms)) stop("Duplicated names found")

	x = lapply(x, validate_colors)

	if (any(!is.na(xNA))) xNA[!is.na(xNA)] = validate_colors(xNA[!is.na(xNA)])

	if (!all(types %in% c("div", "seq", "cat"))) stop("Unknown types found. Currently only \"cat\", \"seq\", and \"div\" are supported")



	if (format.palette.name) nms = format_name(nms)

	seriesID = which(series != "other")
	fnms = nms
	if (length(seriesID)) fnms[seriesID] = paste0(series[seriesID], ".", fnms[seriesID])

	res = mapply(process_palette, pal = x, type = types, colNA = xNA, SIMPLIFY = FALSE, MoreArgs = adjust.settings)
	x = lapply(res, "[[", "pal")
	xNA = sapply(res, "[[", "colNA", USE.NAMES = FALSE)


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

	s = get_scores(z, nmax = c(cat = 36, seq = 15, div = 15))


	if (!from.scratch) {
		.z = get(".z", envir = .C4A_CACHE)
		.s = get(".s", envir = .C4A_CACHE)

		z = rbind(.z, z)
		s = abind::abind(.s, s, along=1)
	}

	#cfa = structure(list(z = z, s = s), class = "c4a")

	assign(".z", z, envir = .C4A_CACHE)
	assign(".s", s, envir = .C4A_CACHE)
}




col2hex <- function(x) {
	y <- apply(col2rgb(x), MARGIN=2, FUN=function(y)do.call(rgb, c(as.list(y), list(maxColorValue=255))))
	y[is.na(x)] <- NA
	y
}

validate_colors <- function(x) {
	if (!is.character(x)) stop("items of x found that are not a character vector", call. = FALSE)
	if (any(is.na(x))) stop("items of x found that contain NAs", call. = FALSE)

	w = which(x %in% colors())

	if (length(w)) x[w] = col2hex(x[w])

	if (any(!vapply(gregexpr("^#(([[:xdigit:]]){6}|([[:xdigit:]]){8})$", x), "[[", integer(1), 1) == 1L)) stop("Colors should be R color names (see \"colors()\") or in hex format", call. = FALSE)

	hasIndex = "index" %in% names(attributes(x))

	if (hasIndex) {
		index = attr(x, "index")
		if (!is.list(index)) stop("index attribute is not a list")
		if (length(index) > length(x)) stop("index too large")
		for (i in 1:length(index)) {
			ind = index[[i]]
			if (!is.numeric(ind) || any(is.na(ind)) || any(ind > length(x)) || length(ind) != i) stop("Incorrect index numbers", call. = FALSE)
		}
	}
	x
}

