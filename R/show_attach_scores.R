show_attach_scores = function(z, contrast) {
	type = if (all(z$type == "cat")) "cat" else if (all(z$type == "seq")) "seq" else if (all(z$type == "div")) "div" else "mixed"

	k = nrow(z)

	s = .C4A$s
	s2 = s[match(z$fullname, dimnames(s)[[1]]), , , drop = FALSE]
	s3 = do.call(rbind, lapply(1:k, function(i) {
		# maximum n to take scores from (cat: dim max, seq/div, the scores for the largest palettes)
		mmax = if (type == "cat") dim(s2)[3] else min(z$n[i], which(is.na(s2[i, "rank", ][-1]))[1])
		m = min(z$n[i], mmax)
		s2[i,,m]
	}))

	# approximation of min step for decreased contrast
	if (!is.null(contrast)) {
		rng = contrast[2] - contrast[1]
		s3[, "min_step"] = round(s3[, "min_step"] * rng)
		s3[, "max_step"] = round(s3[, "max_step"] * rng)
	}

	z2 = cbind(z, as.data.frame(s3))

	z2$cbfriendly = get_friendlyness(z2)
	a = t(sapply(z2$palette, analyse_hcl))
	z2 = cbind(z2, a)

	z2$highC = z2$Cmax >= .C4A$Cintense

	if (type == "div") {
		z2$hueType = ifelse(z2$HwidthL >= .C4A$HwidthDivRainbow | z2$HwidthR >= .C4A$HwidthDivRainbow, "RH",
					 ifelse(z2$HwidthL < .C4A$HwidthDivSingle & z2$HwidthR < .C4A$HwidthDivSingle, "SH", "MH"))
		z2$HwidthLR = pmax(z2$HwidthL, z2$HwidthR)
		z2$rank[z2$cbfriendly] = z2$rank[z2$cbfriendly] - 1e9 - ((!z2$highC[z2$cbfriendly]) * 1e6) - ((z2$hueType[z2$cbfriendly] == "SH") * 1e3)
		z2$rank[!z2$cbfriendly] = z2$rank[!z2$cbfriendly] - ((!z2$highC[!z2$cbfriendly]) * 1e-3) - ((z2$hueType[!z2$cbfriendly] == "SH") * 1e-6)
	} else if (type == "seq") {
		z2$hueType = ifelse(z2$Hwidth < .C4A$HwidthSeqSingle, "SH", ifelse(z2$Hwidth < .C4A$HwidthSeqRainbow, "MH", "RH"))
		z2$rank[z2$cbfriendly] = z2$rank[z2$cbfriendly] - 1e9 - ((!z2$highC[z2$cbfriendly]) * 1e6)
		z2$rank[!z2$cbfriendly] = z2$rank[!z2$cbfriendly] - ((!z2$highC[!z2$cbfriendly]) * 1e-3)
	} else if (type == "cat") {
		z2$harmonic = (z2$LCrange < .C4A$LCrangeHarmonic)
		z2$rank[z2$cbfriendly] = z2$rank[z2$cbfriendly] - 1e9 + ((z2$LCrange[z2$cbfriendly]) * 1e6) + (z2$highC[z2$cbfriendly] * 1e3)
		z2$rank[!z2$cbfriendly] = z2$rank[!z2$cbfriendly] + ((z2$LCrange[!z2$cbfriendly]) * 1e-3) + (z2$highC[!z2$cbfriendly] * 1e-6)
	}
	z2$rank = floor(rank(z2$rank, ties.method = "min"))
	z2
}


get_friendlyness = function(zn) {
	with(zn, {
		ifelse(type == "cat", min_dist >= .C4A$CBF_th$cat["min_dist"],
		ifelse(type == "seq", min_step >= .C4A$CBF_th$seq["min_step"],
		ifelse(type == "div", inter_wing_dist >= .C4A$CBF_th$div["inter_wing_dist"] & inter_wing_hue_dist >= .C4A$CBF_th$div["inter_wing_hue_dist"] & min_step >= .C4A$CBF_th$div["min_step"], FALSE)))
	})
}
