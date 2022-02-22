#' col4all scales for ggplot2
#'
#' col4all scales for ggplot2
#'
#' @param palette,reverse,order See \code{\link{c4a}}.
#' @param aesthetics The ggplot2 aesthetics to which this scale should be applied.
#' @param ... common discrete scale parameters: `name`, `breaks`, `labels`, `na.value`, `limits` and `guide`. See \code{\link[ggplot:discrete_scale]{discrete_scale}} for more details.
#' @example ./examples/scale_color_c4a_cat.R
#' @rdname scales_ggplot2
#' @name scale_color_c4a_cat
#' @export
scale_color_c4a_cat = function (palette = NULL, reverse = FALSE, order = NULL, ...) {
	scale_discrete(aes = "colour", type = "cat", palette = palette, reverse = reverse, order = order, ...)
}

#' @rdname scales_ggplot2
#' @name scale_colour_c4a_cat
#' @export
scale_colour_c4a_cat = function (palette = NULL, reverse = FALSE, order = NULL, ...) {
	scale_discrete(aes = "colour", type = "cat", palette = palette, reverse = reverse, order = order, ...)
}


#' @rdname scales_ggplot2
#' @name scale_fill_c4a_cat
#' @export
scale_fill_c4a_cat = function (palette = NULL, reverse = FALSE, order = NULL, ...) {
	scale_discrete(aes = "fill", type = "cat", palette = palette, reverse = reverse, order = order, ...)
}

#' @rdname scales_ggplot2
#' @name scale_color_c4a_seq
#' @export
scale_color_c4a_seq = function (palette = NULL, reverse = FALSE, contrast = NULL, ...) {
	scale_discrete(aes = "color", type = "seq", palette = palette, reverse = reverse, contrast = contrast, ...)
}

#' @rdname scales_ggplot2
#' @name scale_colour_c4a_seq
#' @export
scale_colour_c4a_seq = function (palette = NULL, reverse = FALSE, contrast = NULL, ...) {
	scale_discrete(aes = "color", type = "seq", palette = palette, reverse = reverse, contrast = contrast, ...)
}

#' @rdname scales_ggplot2
#' @name scale_fill_c4a_seq
#' @export
scale_fill_c4a_seq = function (palette = NULL, reverse = FALSE, contrast = NULL, ...) {
	scale_discrete(aes = "fill", type = "seq", palette = palette, reverse = reverse, contrast = contrast, ...)
}

#' @rdname scales_ggplot2
#' @name scale_color_c4a_div
#' @export
scale_color_c4a_div = function (palette = NULL, reverse = FALSE, contrast = NULL, ...) {
	scale_discrete(aes = "color", type = "div", palette = palette, reverse = reverse, contrast = contrast, ...)
}

#' @rdname scales_ggplot2
#' @name scale_colour_c4a_div
#' @export
scale_colour_c4a_div = function (palette = NULL, reverse = FALSE, contrast = NULL, ...) {
	scale_discrete(aes = "color", type = "div", palette = palette, reverse = reverse, contrast = contrast, ...)
}

#' @rdname scales_ggplot2
#' @name scale_fill_c4a_div
#' @export
scale_fill_c4a_div = function (palette = NULL, reverse = FALSE, contrast = NULL, ...) {
	scale_discrete(aes = "fill", type = "div", palette = palette, reverse = reverse, contrast = contrast, ...)
}



scale_discrete  = function(aes, type, palette = NULL, reverse = FALSE, order = NULL, contrast = NULL, ...) {
	#c4a_args <- c("palette", "reverse", "order")
	args <- list(palette = palette, reverse = reverse, order = order, contrast = contrast)
	#args[[1]] <- NULL
	#args <- args[na.omit(match(c4a_args, names(args)))]
	pal <- function(n) {
		args <- c(args, list(n = n, type = type))
		do.call(c4a, args, envir = parent.frame())
	}
	na = c4a_na(palette)

	ggplot2::discrete_scale(aesthetics = aes, "manual", pal, na.value = na, ...)

}



