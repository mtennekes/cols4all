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
