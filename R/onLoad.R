.onLoad <- function(...) {
	assign(".z", .z, envir = .C4A_CACHE)
	assign(".s", .s, envir = .C4A_CACHE)
}

.C4A_CACHE <- new.env(FALSE, parent=globalenv())
