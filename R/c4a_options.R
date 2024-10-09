#' Set cols4all options
#'
#' Get or set global options for c4a. Works similar as the base function `options`
#'
#' @param ... Use character values to retrieve options. To set options, either use named arguments (where the names refer to the options), a list that consists of those options.
#'
#' @details
#'
#' | **Option**        | **Description**   |
#' | ------------- | ------------- |
#' | defaults		|  Default palettes per type |
#' | CBF_th		|  Parameters that label a palette as color blind friendly  |
#' | CBVF_th		|  Parameters that label a palette as very color blind friendly  |
#' | CBU_th		| Parameters that label a palette as color blind unfriendly |
#' | CrangeFair		| Maximum chroma range for which a palette is considered harmonic |
#' | CrangeUnfair		| Minimum chroma range for which a palette is considered disharmonic  |
#' | LrangeFair		| Maximum luminance range for which a palette is considered harmonic |
#' | LrangeUnfair		| Minimum luminance range for which a palette is considered disharmonic |
#' | Cintense		| Chroma of colors that are considered intense |
#' | Cpastel		| Chroma of colors that are considered 'pastel' |
#' | HwidthDivRainbow		| A diverging palette is labeled as 'rainbow hue' if HwidthL or HwidthR are at least `HwidthDivRainbow` |
#' | HwidthDivSingle		| A diverging palette is labeled as 'single hue' if HwidthL and HwidthR are at most `HwidthDivSingle` |
#' | HwidthSeqRainbow | A sequential palette is labeled as 'rainbow hue' if Hwidth is at least `HwidthSeqRainbow` |
#' | HwidthSeqSingle | A sequential palette is labeled as 'single hue' if Hwidth is at most `HwidthSeqSingle` |
#' | naming_fun | Function that returns a distance matrix with the `naming_colors` (see examples) |
#' | naming_fun_args | List of arguments for `naming_fun` |
#' | naming_colors | Vector of prototype colors for the color names (see examples) |
#' | naming_softmax | List of parameters for the softmax function applied to the distance matrix |

#'
#' @md
#' @name c4a_options
#' @rdname c4a_options
#' @return A list of options
#' @example ./examples/c4a_options.R
#' @export
c4a_options = function(...) {
	lst = list(...)
	e1 = parent.frame()
	nms = c("defaults", "CBF_th", "CBVF_th", "CBU_th", "CrangeFair", "CrangeUnfair", "LrangeFair", "LrangeUnfair", "Cintense", "Cpastel", "HwidthDivRainbow", "HwidthDivSingle", "HwidthSeqRainbow", "HwidthSeqSingle", "naming_fun", "naming_fun_args", "naming_colors", "naming_softmax")

	o = as.list(.C4A)[nms]

	if (length(lst) >= 1 && is.null(names(lst))) {
		arg = lst[[1]]
		if (is.list(arg)) {
			## case 1: option list is given
			args = arg
			if (length(lst) > 1) warning("Only the first argument is used; the other arguments are ignored.")
		} else {
			## case 2: option name is given
			args = sapply(lst, "[", 1)
			return(o[args])
		}
	} else {
		## case 3: named options are set
		## case 4: c4a_options is called without arguments
		args = lapply(as.list(match.call()[-1]), eval, envir = e1)
	}


	if (!length(args)) {
		# case 4
		return(o)
	} else {
		# case 1 and 3
		backup = o[names(args)]
		o[names(args)] = args # check_named_items(args, backup)

		list2env(args, envir = .C4A)

		if (any(c("naming_fun", "naming_fun_args", "naming_colors", "naming_softmax") %in% names(args))) {
			.C4A$name_data = create_name_data()
			update_nameability()
		}

		invisible(backup)
	}
}
