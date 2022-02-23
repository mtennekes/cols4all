#' col4all scales for ggplot2
#'
#' col4all scales for ggplot2
#'
#' @param palette,reverse,order,contrast See \code{\link{c4a}}.
#' @param mid data value that should be mapped to the mid-point of the diverging color scale
#' @param n_interp number of discrete colors that should be used to interpolate the continuous color scale. Recommended to use an odd number to include the midpoint
#' @param ... common discrete scale parameters: `name`, `breaks`, `labels`, `na.value`, `limits` and `guide`. See \code{\link[ggplot2:discrete_scale]{discrete_scale}} for more details.
#' @example ./examples/scales_ggplot2.R
#' @rdname scales_ggplot2
#' @name scale_color_discrete_c4a_cat
#' @export
scale_color_discrete_c4a_cat = function (palette = NULL, reverse = FALSE, order = NULL, ...) {
	scale_discrete(aes = "color", type = "cat", palette = palette, reverse = reverse, order = order, ...)
}

#' @rdname scales_ggplot2
#' @name scale_colour_discrete_c4a_cat
#' @export
scale_colour_discrete_c4a_cat = function (palette = NULL, reverse = FALSE, order = NULL, ...) {
	scale_discrete(aes = "color", type = "cat", palette = palette, reverse = reverse, order = order, ...)
}

#' @rdname scales_ggplot2
#' @name scale_fill_discrete_c4a_cat
#' @export
scale_fill_discrete_c4a_cat = function (palette = NULL, reverse = FALSE, order = NULL, ...) {
	scale_discrete(aes = "fill", type = "cat", palette = palette, reverse = reverse, order = order, ...)
}



#' @rdname scales_ggplot2
#' @name scale_color_discrete_c4a_seq
#' @export
scale_color_discrete_c4a_seq = function (palette = NULL, reverse = FALSE, contrast = NULL, ...) {
	scale_discrete(aes = "color", type = "seq", palette = palette, reverse = reverse, contrast = contrast, ...)
}

#' @rdname scales_ggplot2
#' @name scale_colour_discrete_c4a_seq
#' @export
scale_colour_discrete_c4a_seq = function (palette = NULL, reverse = FALSE, contrast = NULL, ...) {
	scale_discrete(aes = "color", type = "seq", palette = palette, reverse = reverse, contrast = contrast, ...)
}

#' @rdname scales_ggplot2
#' @name scale_fill_discrete_c4a_seq
#' @export
scale_fill_discrete_c4a_seq = function (palette = NULL, reverse = FALSE, contrast = NULL, ...) {
	scale_discrete(aes = "fill", type = "seq", palette = palette, reverse = reverse, contrast = contrast, ...)
}


#' @rdname scales_ggplot2
#' @name scale_color_discrete_c4a_div
#' @export
scale_color_discrete_c4a_div = function (palette = NULL, reverse = FALSE, contrast = NULL, ...) {
	scale_discrete(aes = "color", type = "div", palette = palette, reverse = reverse, contrast = contrast, ...)
}

#' @rdname scales_ggplot2
#' @name scale_colour_discrete_c4a_div
#' @export
scale_colour_discrete_c4a_div = function (palette = NULL, reverse = FALSE, contrast = NULL, ...) {
	scale_discrete(aes = "color", type = "div", palette = palette, reverse = reverse, contrast = contrast, ...)
}

#' @rdname scales_ggplot2
#' @name scale_fill_discrete_c4a_div
#' @export
scale_fill_discrete_c4a_div = function (palette = NULL, reverse = FALSE, contrast = NULL, ...) {
	scale_discrete(aes = "fill", type = "div", palette = palette, reverse = reverse, contrast = contrast, ...)
}





#' @rdname scales_ggplot2
#' @name scale_color_continuous_c4a_seq
#' @export
scale_color_continuous_c4a_seq = function (palette = NULL, reverse = FALSE, contrast = NULL, mid = 0, n_interp = 11, ...) {
	scale_continuous(aes = "color", type = "seq", palette = palette, reverse = reverse, contrast = contrast, mid = mid, n_interp = n_interp, ...)
}

#' @rdname scales_ggplot2
#' @name scale_colour_continuous_c4a_seq
#' @export
scale_colour_continuous_c4a_seq = function (palette = NULL, reverse = FALSE, contrast = NULL, mid = 0, n_interp = 11, ...) {
	scale_continuous(aes = "color", type = "seq", palette = palette, reverse = reverse, contrast = contrast, mid = mid, n_interp = n_interp, ...)
}

#' @rdname scales_ggplot2
#' @name scale_fill_continuous_c4a_seq
#' @export
scale_fill_continuous_c4a_seq = function (palette = NULL, reverse = FALSE, contrast = NULL, mid = 0, n_interp = 11, ...) {
	scale_continuous(aes = "fill", type = "seq", palette = palette, reverse = reverse, contrast = contrast, mid = mid, n_interp = n_interp, ...)
}

#' @rdname scales_ggplot2
#' @name scale_color_continuous_c4a_div
#' @export
scale_color_continuous_c4a_div = function (palette = NULL, reverse = FALSE, contrast = NULL, mid = 0, n_interp = 11, ...) {
	scale_continuous(aes = "color", type = "div", palette = palette, reverse = reverse, contrast = contrast, mid = mid, n_interp = n_interp, ...)
}

#' @rdname scales_ggplot2
#' @name scale_colour_continuous_c4a_div
#' @export
scale_colour_continuous_c4a_div = function (palette = NULL, reverse = FALSE, contrast = NULL, mid = 0, n_interp = 11, ...) {
	scale_continuous(aes = "color", type = "div", palette = palette, reverse = reverse, contrast = contrast, mid = mid, n_interp = n_interp, ...)
}

#' @rdname scales_ggplot2
#' @name scale_fill_continuous_c4a_div
#' @export
scale_fill_continuous_c4a_div = function (palette = NULL, reverse = FALSE, contrast = NULL, mid = 0, n_interp = 11, ...) {
	scale_continuous(aes = "fill", type = "div", palette = palette, reverse = reverse, contrast = contrast, mid = mid, n_interp = n_interp, ...)
}











scale_discrete  = function(aes, type, palette = NULL, reverse = FALSE, order = NULL, contrast = NULL, ...) {
	args = list(palette = palette, reverse = reverse, order = order, contrast = contrast, type = type)
	pal <- function(n) {
		args = c(args, list(n = n))
		do.call(c4a, args, envir = parent.frame())
	}
	na = c4a_na(palette = palette, type = type)
	ggplot2::discrete_scale(aesthetics = aes, "manual", pal, na.value = na, ...)
}


scale_continuous  = function(aes, type, palette = NULL, reverse = FALSE, contrast = NULL, mid = 0, n_interp = 11, ...) {
	args = list(palette = palette, reverse = reverse, contrast = contrast, type = type, n = n_interp)
	cols = do.call(c4a, args)
	na = c4a_na(palette = palette, type = type)

	scale_label = if(type == "seq") "continuous_diverging" else "continuous_diverging"

	if (type == "seq") {
		ggplot2::continuous_scale(aesthetics = aes, scale_name = scale_label,
								  scales::gradient_n_pal(cols, values = NULL), na.value = na,
								  guide = "colourbar", ...)
	} else {
		ggplot2::continuous_scale(aesthetics = aes, scale_name = scale_label,
								  scales::gradient_n_pal(cols, values = NULL), na.value = na,
								  guide = "colourbar", rescaler = mid_rescaler(mid), ...)
	}

}

mid_rescaler = function (mid) {
	function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
		scales::rescale_mid(x, to, from, mid)
	}
}
