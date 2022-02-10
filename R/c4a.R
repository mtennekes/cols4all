c4a_names = function() {
	names(z_cat)
}

c4a = function(name, n = NULL, order = NULL) {
	if (!(name %in% names(z_cat))) stop("Unknown palette. See c4a_names() for options, and c4a_show / c4a_gui to see them.")
	zi = z_cat[[name]]
	index = attr(zi, "index")

	pal = if (is.null(index)) {
		if (!is.null(n)) {
			if (length(zi) < n) stop("Palette ", name, " only supports ", length(zi), " colors.")
			zi[1L:n]
		} else {
			n = length(zi)
			zi
		}
	} else {
		ns = as.character(names(index))
		if (!is.null(n)) {
			if (!n %in% ns) {
				stop("Palette ", name, " only supports ", min(ns), " to ", max(ns), " colors.")
			}
			zi[index[[as.character(n)]]]
		} else {
			zi[index[[length(index)]]]
		}
	}
	if (!is.null(order)) {
		if (!all(order %in% 1L:n)) stop("order should consist of numbers 1 to ", n)
		pal[order]
	} else
		pal
}
