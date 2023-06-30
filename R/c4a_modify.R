#' Edit cols4all palettes
#'
#' Edit cols4all palettes. c4a_duplicate duplicates an existing cols4all palette, and c4a_modify is used to change the colors. Use c4a_data to craete palettes from scratch.
#'
#' @param palette name of the palette
#' @param x vector of the new colors. It should either the same length, or a named vector, where the names correspond to the index numbers. E.g. c("3" = "#AABBCC") will replace the third color with the color "#AABBCC".
#' @param xNA the new color for missing values.
#' @rdname c4a_modify
#' @name c4a_modify
#' @seealso [c4a_data()]
#' @example ./examples/c4a_modify.R
#' @export
c4a_modify = function(palette, x = NULL, xNA = NULL) {
	info = c4a_info(palette)

	if (is.null(info)) return(invisible(NULL))

	z = .C4A$z
	s = .C4A$s

	id = which(z$fullname == info$fullname)

	n = length(z$palette[[id]])

	if (!is.null(x)) {
		nms = suppressWarnings(as.numeric(names(x)))
		if (!is.null(nms)) {
			if (any(is.na(nms))) stop("names of x should be id numbers")
			if (any(nms < 1) || any(nms > n)) stop("names of x should be numbers between 1 and ", n)
			z$palette[[id]][nms] = unname(x)
		} else {
			if (length(x) != n) stop("length of x should be ", n)
			z$palette[[id]][1:n] = unname(x)
		}
	}

	if (!is.null(xNA)) {
		z$na[id] = unname(xNA[1])
	}

	sid = series_add_get_scores(z[id, ])[1,,]

	s[id,,] = sid

	.C4A$z = z
	.C4A$s = s
}

#' @param name name of new palette
#' @rdname c4a_modify
#' @name c4a_duplicate
#' @export
c4a_duplicate = function(palette, name = NA) {
	x = c4a_info(palette)

	if (is.null(x)) return(invisible(NULL))

	nms = c4a_palettes(series = x$series, full.names = FALSE)
	if (is.na(name)) {
		newname = make.names(c(nms, x$name), unique = TRUE)[length(nms) + 1L]
		newname = gsub(".", "_", newname, fixed = TRUE)
	} else {
		if (name %in% nms) stop(name, " already exists")
		newname = name
	}

	x$name = newname
	x$fullname = paste(x$series, newname, sep = ".")
	x$cit = NULL
	x$bib = NULL
	y = c4a_data(x)
	c4a_load(y)

	if (is.na(name)) message("New palette created: ", x$fullname)

	invisible(NULL)
}
