format_name = function(x) {
	x2 = gsub("[-, \\,, (, ), \\ ]",  "", x)
	tolower(gsub("([a-z])([A-Z])", "\\1_\\L\\2", x2, perl = TRUE))
}

check_name_presence = function(name, verbose = TRUE) {
	fullnames = .C4A$z$fullname
	nnames = .C4A$z$name


	# 1. full name
	isfn = which(name == fullnames)[1]

	if (!is.na(isfn)) return(name)

	# 2. palette name only
	palid = which(name == nnames)
	if (length(palid)) {
		nms = fullnames[palid]
		if (length(palid) > 1) {
			if (verbose) message(paste0("Multiple palettes called \"", name, "\" found: \"", paste(nms, collapse = "\", \""), "\". The first one, \"", nms[1], "\", is returned."))
			nms = nms[1]
		}
		return(nms)
	}
	NULL
}

closest_match = function(x, y, tolerance = .5, max = 1.5) {
	s = stringdist::stringdist(x, y, weight = c(d = .5, i = .7, s = 1, t = .7))
	smin = min(s)
	th = min(max, (smin + tolerance))
	ids = which(s <= th)
	list(ids = ids,
		 stringdist = smin)
}


fuzzy_names = function(name) {
	fullnames = .C4A$z$fullname
	nnames = .C4A$z$name
	snames = .C4A$z$series

	base = strsplit(name, split = ".", fixed = TRUE)[[1]][-1]
	series = strsplit(name, split = ".", fixed = TRUE)[[1]][1]

	# without "series."
	if (length(base) == 0L) {
		fullnames[closest_match(name, nnames)$ids]
	} else {
		res1 = closest_match(base, nnames[snames != series])
		res2 = closest_match(base, nnames[snames == series])

		if (res2$stringdist <= res1$stringdist) {
			fullnames[snames == series][res2$ids]
		} else {
			c(fullnames[snames == series][res2$ids],
			  fullnames[snames != series][res1$ids])
		}
	}
}

# Converts a name to a known palette name, if it exists.
#
# Converts a name to a known palette name, if it exists. Otherwise, either an error is thrown or `NULL` is returned
#
# @param name palette name to be converted
# @param no.match what happens is no match is found? Options: `"message"`: a message is thrown with suggestions, `"error"`: an error is thrown, `"null"`: `NULL` is returned
# @param verbose should messages be printed?
c4a_name_convert = function(name, no.match = c("message", "error", "null"), verbose = TRUE) {
	no.match = match.arg(no.match)
	name2 = check_name_presence(name, verbose)
	if (!is.null(name2)) return(name2)

	fname = format_name(name)
	fname2 = check_name_presence(fname, verbose)

	if (is.null(fname2)) {
		if (no.match == "error") {
			stop("Unknown palette name. See c4a_palettes() for all palette names", call. = FALSE)
		} else if (no.match == "message") {
			fuzzy = fuzzy_names(name)
			if (length(fuzzy) == 0) {
				message("Unknown palette name. See c4a_palettes() for all palette names")
			} else {
				message("Unknown palette name. Close alternatives are: \"", paste(fuzzy, collapse = "\", \""), "\". See c4a_palettes() for all palette names")
			}
			return(NULL)
		} else {
			return(NULL)
		}
	}
	fname2
}
