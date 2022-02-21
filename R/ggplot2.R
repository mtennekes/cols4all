#' col4all scales for ggplot2
#'
#' col4all scales for ggplot2
#'
#' @param palette,reverse,order See \code{\link{c4a}}.
#' @param aesthetics The ggplot2 aesthetics to which this scale should be applied.
#' @param ... common discrete scale parameters: `name`, `breaks`, `labels`, `na.value`, `limits` and `guide`. See \code{\link[ggplot:discrete_scale]{discrete_scale}} for more details.
#' @example ./examples/scale_color_c4a_cat.R
#' @export
scale_color_c4a_cat = function (palette = NULL, reverse = NULL, order = NULL, aesthetics = "colour",
		  ...)
{
	c4a_args <- c("palette", "reverse", "order")
	args <- as.list(match.call())
	args[[1]] <- NULL
	args <- args[na.omit(match(c4a_args, names(args)))]
	pal <- function(n) {
		#
		# if (is.null(nmax))
		# 	nmax <- n
		# if (is.null(order))
		# 	order <- 1:n
		# if (n > nmax) {
		# 	warning("Insufficient values in scale_colour_discrete_qualitative. ",
		# 			n, " needed but only ", nmax, " provided.", call. = FALSE)
		# }
		args <- c(args, list(n = n, type = "cat"))
		do.call(c4a, args, envir = parent.frame())
	}
	ggplot2::discrete_scale(aesthetics, "manual", pal, ...)
}
