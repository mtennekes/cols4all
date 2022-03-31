#' Show how to cite palettes
#'
#' Show how to cite palettes
#'
#' @param name name of a palette or series
#' @export
c4a_citation = function(name) {
	is_series = name %in% c4a_series()

	if (!is_series) {
		inf = c4a_info(name, no.match = NULL, verbose = FALSE)
		if (is.null(inf)) {
			message(paste0("\"", name, "\" is not a known series or palette name"))
			return(invisible(NULL))
		} else {
			name = inf$name
			series = inf$series
		}
	} else {
		series = name
	}

	bnames = names(.C4A$zbib)

	if (is_series) {
		if (!(name %in% bnames)) {
			message("No citation information (bibentry) found for series \"", series, "\"")
			return(invisible(NULL))
		}
		bib = .C4A$zbib[[name]]
	} else {
		split_names = lapply(bnames, function(bn) {
			strsplit(bn, split = ".", fixed = TRUE)[[1]][-1]
		})
		split_series = sapply(bnames, function(bn) {
			strsplit(bn, split = ".", fixed = TRUE)[[1]][1]
		})
		match_name = sapply(split_names, function(sn) {
			name %in% sn
		})
		match_series = (series == split_series)
		if (any(match_name & match_series)) {
			bib = .C4A$zbib[[which(match_name & match_series)]]
		} else {
			if (!(series %in% bnames)) {
				message("No citation information (bibentry) found for palette \"", name, "\" from series \"", series, "\"")
				return(invisible(NULL))
			}
			is_series = TRUE
			bib = .C4A$zbib[[series]]
		}
	}

	cat("\n")

	if (is_series) {
		cat("To cite palettes from", series, "in publications use:\n\n")
	} else {
		cat("To cite palette", name, "from", series, "in publications use:\n\n")
	}

	print(bib, style = "text")
	cat("\n")

	print(bib, style = "Bibtex")
	invisible(bib)
}
