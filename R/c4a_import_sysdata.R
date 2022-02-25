#' Import and export system data
#'
#' Import and export system data. `c4a_import_sysdata` will import system data and overwrite the current system data. System data includes palette data and pre-calculated color-blind-friendliness scores. `c4a_export_sysdata` will export the current system data. These functions are for advanced use; the functions `c4a_add_series` and `c4a_remove_series` are user-friendly interfaces to change the loaded palettes.
#'
#' @param x system data
#' @example ./examples/c4a_import_sysdata.R
#' @return `c4a_export_sysdata` returns the system data
#' @rdname c4a_import_sysdata
#' @name c4a_import_sysdata
#' @export
c4a_import_sysdata = function(x) {
	if (!is.list(x) || !setequal(c("z", "s"), names(x))) stop("x should be a list of two: z and s", call. = FALSE)

	z = check_z(x$z)
	s = check_s(x$s, nrow(z))


	message("cols4all system data imported successfully")

	assign(".z", z, envir = .C4A_CACHE)
	assign(".s", s, envir = .C4A_CACHE)
}

#' @rdname c4a_import_sysdata
#' @name c4a_export_sysdata
#' @export
c4a_export_sysdata = function() {
	list(z = get(".z", envir = .C4A_CACHE),
		 s = get(".s", envir = .C4A_CACHE))
}
