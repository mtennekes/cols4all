#' Import and export system data
#'
#' Import and export system data. `c4a_sysdata_import` will import system data and overwrite the current system data. System data includes palette data and pre-calculated color-blind-friendliness scores. `c4a_sysdata_export` will export the current system data. These functions are for advanced use; the functions \code{\link{c4a_palettes_add}} and \code{\link{c4a_palettes_remove}} are user-friendly interfaces to change the loaded palettes.
#'
#' @param x system data
#' @example ./examples/c4a_sysdata_import.R
#' @return `c4a_sysdata_export` returns the system data
#' @rdname c4a_sysdata_import
#' @name c4a_sysdata_import
#' @export
c4a_sysdata_import = function(x) {
	if (!is.list(x) || !setequal(c("z", "s", "zbib"), names(x))) stop("x should be a list of three: z, s, zbib", call. = FALSE)

	z = check_z(x$z)
	s = check_s(x$s, nrow(z))
	zbib = x$zbib # to do: check

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
	list(z = z,
		 s = .C4A$s,
		 zbib = .C4A$zbib)
}
