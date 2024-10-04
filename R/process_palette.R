process_palette = function(pal, type, colNA = NA, take.gray.for.NA = FALSE, remove.other.grays = FALSE, remove.blacks = NA, remove.whites = NA, light.to.dark = TRUE, remove.names = TRUE, biv.method = "byrow", space = "rgb", range_matrix_args = list()) {

	if (is.na(remove.blacks)) remove.blacks = (type == "cat")
	if (is.na(remove.whites)) remove.whites = (type == "cat")

	# maybe need to reindex
	index = attr(pal, "index")
	range_matrix = attr(pal, "range_matrix")
	orig_pal = pal

	if (substr(type, 1, 3) == "biv") {
		pal = create_biv_palette(pal, biv.method)
	}

	if (type == "cyc") {
		if (pal[1] != tail(pal, 1)) {
			pal = c(pal, pal[1])
		}
	}


	hcl = get_hcl_matrix(pal)

	#specplot(hcl(h=seq(0,360,by=10), c = 0, l= 15))
	#specplot(hcl(h=seq(0,360,by=10), c = 5, l= 10))
	#specplot(hcl(h=seq(0,360,by=10), c = 10, l= 5))
	#specplot(hcl(h=seq(0,360,by=10), c = 15, l= 0))
	if (remove.blacks && type == "cat") {
		isB = ((hcl[,3] + hcl[,2]) <= 15) | (hcl[,2] == 0)
		if (all(isB)) {
			message("Palette contains only (almost) blacks. Therefore remove.blacks is set to FALSE")
			remove.blacks = FALSE
		} else if (any(isB)) {
			pal = pal[!isB]
			hcl = hcl[!isB,]
		}
	}

	#specplot(hcl(h=seq(0,360,by=10), c = 0, l= 15))
	#specplot(hcl(h=seq(0,360,by=10), c = 5, l= 10))
	#specplot(hcl(h=seq(0,360,by=10), c = 10, l= 5))
	#specplot(hcl(h=seq(0,360,by=10), c = 15, l= 0))
	if (remove.whites && type == "cat") {
		isW = hcl[,2] <= 3 & hcl[,3] >= 99
		if (all(isW)) {
			message("Palette contains only (almost) whites. Therefore remove.whites is set to FALSE")
			remove.whites = FALSE
		} else if (any(isW)) {
			pal = pal[!isW]
			hcl = hcl[!isW,]
		}
	}

	# take lightest gray as NA color
	if (type == "cat") {
		if (take.gray.for.NA) {
			wG = which(hcl[,2] < .C4A$Cgray)
			if (length(wG) == nrow(hcl)) {
				message("Palette contains only (almost) greys. Therefore take.gray.for.NA and remove.other.grays will be set to FALSE")
				take.gray.for.NA = FALSE
				remove.other.grays = FALSE
			} else if (length(wG) && is.na(colNA)) {
				wNA = wG[which.max(hcl[wG, 3])]
				colNA = pal[wNA]
				pal = pal[-wNA]
				hcl = hcl[-wNA,,drop=FALSE]
			}
		}

		# remove other grays
		if (remove.other.grays) {
			wG = which(hcl[,2] < .C4A$Cgray)
			if (length(wG)) {
				pal = pal[-wG]
				hcl = hcl[-wG,]
			}
		}

	}

	if (light.to.dark && type == "seq") {
		ls = hcl[,3]
		ls_sg = sign(ls[-1] - ls[-length(ls)])
		if (all(ls_sg>=0)) {
		#if ((hcl[nrow(hcl), 1] - hcl[1,1]) > 30) {
			pal = rev(pal)
			hcl = hcl[nrow(hcl):1L,]
			reversed = TRUE
		} else {
			reversed = FALSE
		}
	} else {
		reversed = FALSE
	}



	if (is.na(colNA)) {
		if (substr(type, 1, 3) == "biv") {
			colNA = "#FFFFFF"
		} else {
			# first candidates: choose NA from grays: for bright palette prefer even lighter grey, dark palettes dark gray or black
			# prefer lightest gray
			gray_range = range(hcl[,3]) / 100


			candidates = list(grDevices::gray.colors(10, start = gray_range[2], end = 1),
							  grDevices::gray.colors(10, start = gray_range[2], end = 0))

			if (gray_range[2] < 0.6) {
				# => dark palette
				candidates = rev(candidates)
			}

			if (substr(type, 1, 3) == "cat") {
				# restrict to ligher color for bright palettes and darker colors for dark palettes, because otherwise colNA stands out too much
				candidates = candidates[[1]]
			}


			cand = unlist(candidates)
			pal2 = c(pal, cand)
			m = sapply(c("protan", "deutan", "tritan"), function(cvd) {
				m = get_dist_matrix(pal2, cvd = cvd)
				m2 = m[1L:length(pal), (length(pal) + 1L):length(pal2)]
				apply(m2, MARGIN = 2, min)
			})
			scores = apply(m, MARGIN = 1, min)

			# take the first with at least 10 difference (larger would be too much)
			s10 = which(scores >= 10)
			if (length(s10)) {
				colNA = cand[s10[1]]
			} else {
				colNA = cand[which.max(scores)]
			}
		}
	}

	if (remove.names) {
		pal = unname(pal)
		colNa = unname(colNA)
	}


	if (!is.null(index)) {
		mat = match(as.vector(orig_pal), pal)
		index2 = lapply(index, function(ind) {
			i = mat[ind]
			i[!is.na(i)]
		})
		ls = sapply(index2, length)

		newlength = max(ls)

		index3 = lapply(1:newlength, function(i) {
			w = which(i == ls)[1]
			if (!length(w)) stop("Re-indexing failed", call. = FALSE)
			index2[[w]]
		})
		attr(pal, "index") = index3
	} else if (is.null(range_matrix) && type %in% c("seq", "div", "cyc")) {

		rma = formals(get(paste0("range_", type)))
		rma$n = NULL


		if (!is.null(range_matrix_args) && length(range_matrix_args)) {
			rma[names(range_matrix_args)] = range_matrix_args
		}

		range_matrix = do.call(paste0("range_", type), c(list(n = length(pal)), rma))
		attr(pal, "range_matrix") = range_matrix
	} else if (is.null(range_matrix)) {
		attr(pal, "range_matrix") = range_matrix
	}

	attr(pal, "space") = space

	pal[] = toupper(pal[])
	colNA = toupper(colNA)

	list(pal = pal, colNA = colNA, reversed = reversed)
}
