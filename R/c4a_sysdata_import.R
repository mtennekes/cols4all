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
	if (!is.list(data) || !setequal(c("data", "scores", "citation", "description"), names(data))) stop("data should be a list of four: data, scores, citation, and description", call. = FALSE)

	z = check_z(data$data)
	s = check_s(data$scores, nrow(z))
	zbib = data$citation # to do: check
	zdes = data$description

	message("cols4all system data imported successfully")

	.C4A$z = z
	.C4A$s = s
	.C4A$zbib = zbib
	.C4A$zdes = zdes
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
				   citation = .C4A$zbib,
				   description = .C4A$zdes), class = "c4a_data")
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
	zbib = .C4A$zbib
	zdes = .C4A$zdes

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
		zbib2 = NULL
		zdes2 = NULL
	} else {
		z2 = z[!sel, ]
		s2 = s[!sel, ,]

		if (is.null(zbib)) {
			zbib2 = NULL
		} else {
			zbsel = logical(length(zbib))
			splt = strsplit(names(zbib), ".",fixed = TRUE)
			is_series = (sapply(splt, length) == 1)

			if (any(is_series)) {
				whole_series = unlist(splt[is_series])
				whole_series_retain = intersect(whole_series, z2$series)
				zbsel[names(zbib) %in% whole_series_retain] = TRUE
			}
			if (!all(is_series)) {
				indiv_pals = lapply(splt[!is_series], function(x) {
					paste(x[1], x[-1], sep = ".")
				})
				zbsel[which(!is_series)[sapply(indiv_pals, function(x) {
					any(x %in% z2$fullname)
				})]] = TRUE
			}
			if (any(zbsel)) {
				zbib2 = zbib[zbsel]
			} else {
				zbib2 = NULL
			}
		}

		if (is.null(zdes)) {
			zdes2 = NULL
		} else {
			contained = names(zdes) %in% z2$series
			if (any(contained)) {
				zdes2 = zdes[contained]
			} else {
				zdes2 = NULL
			}
		}
	}

	message(mes)

	.C4A$z = z2
	.C4A$s = s2
	.C4A$zbib = zbib2
	.C4A$zdes = zdes2

	fill_P()
	invisible(NULL)
}
