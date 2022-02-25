process_palette = function(pal, type, colNA = NA, take.gray.for.NA = TRUE, remove.other.grays = FALSE, remove.blacks = TRUE, light.to.dark = TRUE, remove.names = TRUE) {

	# maybe need to reindex
	index = attr(pal, "index")
	orig_pal = pal

	hcl = get_hcl_matrix(pal)

	#specplot(hcl(h=seq(0,360,by=10), c = 0, l= 15))
	#specplot(hcl(h=seq(0,360,by=10), c = 5, l= 10))
	#specplot(hcl(h=seq(0,360,by=10), c = 10, l= 5))
	#specplot(hcl(h=seq(0,360,by=10), c = 15, l= 0))
	if (remove.blacks && type == "cat") {
		isB = (hcl[,1] + hcl[,2]) <= 15
		if (any(isB)) {
			pal = pal[!isB]
			hcl = hcl[!isB,]
		}
	}

	# take lightest gray as NA color
	if (type == "cat") {
		if (take.gray.for.NA) {
			wG = which(hcl[,2] < 10)
			if (length(wG) && is.na(colNA)) {
				wNA = wG[which.max(hcl[wG, 1])]
				colNA = pal[wNA]
				pal = pal[-wNA]
				hcl = hcl[-wNA,]
			}
		}

		# remove other grays
		if (remove.other.grays) {
			wG = which(hcl[,2] < 10)
			if (length(wG)) {
				pal = pal[-wG]
				hcl = hcl[-wG,]
			}
		}

	}

	if (light.to.dark && type == "seq") {
		if ((hcl[nrow(hcl), 1] - hcl[1,1]) > 40) {
			pal = rev(pal)
			hcl = hcl[nrow(hcl):1L,]
		}
	}

	if (is.na(colNA)) {
		# choose NA from grays, such that luminance is at most 0.2 darker or lighter than darkest/lightest color.
		# prefer lightest gray
		gray_range = c(max(0, (min(hcl[,1]/100) - 0.2)), min(1, (max(hcl[,1]/100) + 0.2)))

		grays = grDevices::gray.colors(10, start = gray_range[1], end = gray_range[2])
		pal2 = c(pal, grays)
		m = sapply(c("pro", "deu", "tri"), function(cvd) {
			m = colorblindcheck::palette_dist(pal2, cvd = cvd)
			m2 = m[1L:length(pal), (length(pal) + 1L):length(pal2)]
			apply(m2, MARGIN = 2, min)
		})
		colNA = grays[which.max(apply(m, MARGIN = 1, min))]
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
	}

	list(pal = pal, colNA = colNA)
}
