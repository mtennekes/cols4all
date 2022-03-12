process_palette = function(pal, type, colNA = NA, take.gray.for.NA = TRUE, remove.other.grays = FALSE, remove.blacks = TRUE, light.to.dark = TRUE, remove.names = TRUE, biv.method = "byrow") {

	# maybe need to reindex
	index = attr(pal, "index")
	orig_pal = pal

	if (type %in% c("bivs", "bivc")) {
		pal = create_biv_palette(pal, biv.method)
	}

	hcl = get_hcl_matrix(pal)

	#specplot(hcl(h=seq(0,360,by=10), c = 0, l= 15))
	#specplot(hcl(h=seq(0,360,by=10), c = 5, l= 10))
	#specplot(hcl(h=seq(0,360,by=10), c = 10, l= 5))
	#specplot(hcl(h=seq(0,360,by=10), c = 15, l= 0))
	if (remove.blacks && type == "cat") {
		isB = (hcl[,3] + hcl[,2]) <= 15
		if (any(isB)) {
			pal = pal[!isB]
			hcl = hcl[!isB,]
		}
	}

	# take lightest gray as NA color
	if (type == "cat") {
		if (take.gray.for.NA) {
			wG = which(hcl[,2] < .C4A$Cgray)
			if (length(wG) && is.na(colNA)) {
				wNA = wG[which.max(hcl[wG, 3])]
				colNA = pal[wNA]
				pal = pal[-wNA]
				hcl = hcl[-wNA,]
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

		# first candidates: choose NA from grays, such that luminance is at most 0.3 lighter and not darker than the lightest resp. darkest color.
		# prefer lightest gray

		gray_range = c(min(hcl[,3]/100), min(1, (max(hcl[,3]/100) + 0.3)))
		candidates = list(grDevices::gray.colors(10, start = gray_range[1], end = gray_range[2]),
						  grDevices::hcl(h = seq(0, 340, by = 20), c = 30, l = 70),
						  grDevices::hcl(h = seq(0, 340, by = 20), c = 50, l = 70))

		colNA = "#FFFFFF"
		for (cand in candidates) {
			pal2 = c(pal, cand)
			m = sapply(c("pro", "deu", "tri"), function(cvd) {
				m = colorblindcheck::palette_dist(pal2, cvd = cvd)
				m2 = m[1L:length(pal), (length(pal) + 1L):length(pal2)]
				apply(m2, MARGIN = 2, min)
			})
			scores = apply(m, MARGIN = 1, min)
			if (max(scores) >= 10) {
				colNA = cand[which.max(scores)[1]]
				break
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
	}
	pal[] = toupper(pal[])
	colNA = toupper(colNA)

	list(pal = pal, colNA = colNA, reversed = reversed)
}
