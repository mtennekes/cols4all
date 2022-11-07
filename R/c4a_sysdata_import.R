#' Import and export system data
#'
#' Import and export system data. `c4a_sysdata_import` will import system data and overwrite the current system data, `c4a_sysdata_export` will export the current system data, and `c4a_sysdata_remove` (partly) removes system data.
#'
#' @param data cols4all data (see `c4a_data`)
#' @example ./examples/c4a_sysdata_import.R
#' @return `c4a_sysdata_export` returns the system data
#' @rdname c4a_sysdata_import
#' @name c4a_sysdata_import
#' @export
c4a_sysdata_import = function(data) {
	if (!is.list(data) || !setequal(c("z", "s", "zbib"), names(data))) stop("data should be a list of three: z, s, zbib", call. = FALSE)

	z = check_z(data$z)
	s = check_s(data$s, nrow(z))
	zbib = data$zbib # to do: check

	message("cols4all system data imported successfully")

	.C4A$z = z
	.C4A$s = s
	.C4A$zbib = zbib
	attach_bib()
	fill_P()
	invisible(NULL)
}

#' @rdname c4a_sysdata_import
#' @name c4a_sysdata_export
#' @export
c4a_sysdata_export = function() {
	z = .C4A$z
	z$cit = NULL
	z$bib = NULL
	structure(list(data = z,
				   scores = .C4A$s,
				   citation = .C4A$zbib), class = "c4a_data")
}

#' @rdname c4a_sysdata_import
#' @name c4a_sysdata_remove
#' @param fullnames full palette names (so in the format `series.palette_name`)
#' @param series a character vector of series names that should be removed (use `"all"` to remove all).
#' @param are.you.sure are you sure you want to remove series?
#' @export
c4a_sysdata_remove = function(fullnames = NULL, series = NULL, are.you.sure = NA) {

	def_f = !missing(fullnames)
	def_s = !missing(series)

	if (def_f || def_s) {
		if (identical(are.you.sure, FALSE)) stop("Please set are.you.sure to TRUE if you are", call. = FALSE)
	} else {
		if (!identical(are.you.sure, TRUE)) stop("Without specifying fullnames or series, all palettes will be removed. Please set are.you.sure to TRUE if you are", call. = FALSE)
	}

	z = .C4A$z
	s = .C4A$s

	if (def_f) {
		sel = z$fullname %in% fullnames
		mes = paste0("cols4all palettes \"", paste(intersect(fullnames, z$fullname), collapse = "\", \""), "\" removed")
	} else if (def_s) {
		sel = z$series %in% series
		mes = paste0("cols4all series \"", paste(intersect(series, z$series), collapse = "\", \""), "\" removed")
	} else {
		sel = TRUE
		mes = "all cols4all series removed"
	}

	if (all(sel)) {
		z2 = NULL
		s2 = NULL
	} else {
		z2 = z[!sel, ]
		s2 = s[!sel, ,]
	}

	message(mes)

	.C4A$z = z2
	.C4A$s = s2
	fill_P()
	invisible(NULL)
}
