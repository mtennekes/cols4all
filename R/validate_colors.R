col2hex <- function(x) {
	y <- apply(col2rgb(x), MARGIN=2, FUN=function(y)do.call(rgb, c(as.list(y), list(maxColorValue=255))))
	y[is.na(x)] <- NA
	y
}


validate_colors <- function(x, name = "x", from_list = FALSE) {

	if (from_list) {
		pre = paste("not every item in ", name)
	} else {
		pre = name
	}

	if (!is.character(x)) stop(pre, " is not a character vector", call. = FALSE)
	if (any(is.na(x))) stop(pre, "contain NAs", call. = FALSE)

	w = which(x %in% colors())

	if (length(w)) x[w] = col2hex(x[w])

	if (any(!vapply(gregexpr("^#(([[:xdigit:]]){6}|([[:xdigit:]]){8})$", x), "[[", integer(1), 1) == 1L)) stop("Colors should be R color names (see \"colors()\") or in hex format", call. = FALSE)

	hasIndex = "index" %in% names(attributes(x))

	if (hasIndex) {
		index = attr(x, "index")
		if (!is.list(index)) stop("index attribute is not a list")
		if (length(index) > length(x)) stop("index too large")
		if (is.null(names(index))) names(index) = 1:length(index)
		for (i in 1:length(index)) {
			ind = index[[i]]
			len = as.character(names(index)[i])
			if (!is.null(ind) && (!is.numeric(ind) || any(is.na(ind)) || any(ind > length(x)) || length(ind) != len)) stop("Incorrect index numbers", call. = FALSE)
		}
	}
	x
}
