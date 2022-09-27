show_attach_scores = function(z) {
	type = z$type[1]
	if (!all(z$type == type)) stop("mixed palette types not allowed")


	k = nrow(z)

	s = .C4A$s
	s2 = s[match(z$fullname, dimnames(s)[[1]]), , , drop = FALSE]
	s3 = do.call(rbind, lapply(1:k, function(i) {
		# maximum n to take scores from (cat: dim max, seq/div, the scores for the largest palettes)
		mmax = if (type == "cat") dim(s2)[3] else min(z$n[i], utils::tail(which(!is.na(s2[i, "rank", ])), 1))
		m = min(z$n[i], mmax)
		s2[i,,m]
	}))

	# approximation of min step for decreased range
	# if (!is.null(range)) {
	# 	rng = range[2] - range[1]
	# 	s3[, "min_step"] = round(s3[, "min_step"] * rng)
	# 	s3[, "max_step"] = round(s3[, "max_step"] * rng)
	# }

	z2 = cbind(z, as.data.frame(s3))

	z2$cbfriendly = get_friendlyness(z2)
	z2$cbfriendly[is.na(z2$cbfriendly)] = 0
	z2$iscbf = (z2$cbfriendly == 1)
	#a = t(mapply(analyse_hcl, z2$palette, z2$type))
	#z2 = cbind(z2, a)

	z2$chroma = "M"

	z2$chroma[z2$Cmax >= .C4A$Cintense] = "H"
	z2$chroma[z2$Cmax < .C4A$Cpastel] = "L"


	#z2$highC = z2$Cmax >= .C4A$Cintense

	if (type %in% c("cat", "bivc")) {
		z2$harmony = ifelse(z2$Crange < .C4A$CrangeHarm & z2$Lrange < .C4A$LrangeHarm, "H",
							ifelse(z2$Crange > .C4A$CrangeDisH | z2$Lrange > .C4A$LrangeDisH, "L", "M"))
	} else {
		z2$harmony = ifelse(z2$Crange < .C4A$CrangeHarm, "H",
					 ifelse(z2$Crange > .C4A$CrangeDisH, "L", "M"))
	}


	if (type == "div") {
		z2$hueType = ifelse(z2$HwidthL >= .C4A$HwidthDivRainbow | z2$HwidthR >= .C4A$HwidthDivRainbow, "RH",
					 ifelse(z2$HwidthL < .C4A$HwidthDivSingle & z2$HwidthR < .C4A$HwidthDivSingle, "SH", "MH"))
		z2$HwidthLR = pmax(z2$HwidthL, z2$HwidthR)
		#z2$chroma[z2$Cmin < .C4A$Cpastel] = "L"
		#z2$rank[z2$iscbf] = z2$rank[z2$iscbf] - 1e9 - ((!z2$highC[z2$iscbf]) * 1e6) - ((z2$hueType[z2$iscbf] == "SH") * 1e3)
		#z2$rank[!z2$iscbf] = z2$rank[!z2$iscbf] - ((!z2$highC[!z2$iscbf]) * 1e-3) - ((z2$hueType[!z2$iscbf] == "SH") * 1e-6)
	} else if (type == "seq") {
		z2$hueType = ifelse(z2$Hwidth < .C4A$HwidthSeqSingle, "SH", ifelse(z2$Hwidth < .C4A$HwidthSeqRainbow, "MH", "RH"))
		#z2$chroma[z2$Cmin < .C4A$Cpastel] = "L"
		#z2$rank[z2$iscbf] = z2$rank[z2$iscbf] - 1e9 - ((!z2$highC[z2$iscbf]) * 1e6)
		#z2$rank[!z2$iscbf] = z2$rank[!z2$iscbf] - ((!z2$highC[!z2$iscbf]) * 1e-3)
	} else if (type %in% c("cat", "bivc")) {
		# z2$harmony = ifelse(z2$Crange < .C4A$CrangeHarm & z2$Lrange < .C4A$LrangeHarm, "H",
		# 			 ifelse(z2$Crange > .C4A$CrangeDisH | z2$Lrange > .C4A$LrangeDisH, "L", "M"))
		#
		#z2$chroma[z2$Crange < .C4A$CrangeHarmonic] = "L"

		#z2$rank[z2$iscbf] = z2$rank[z2$iscbf] - 1e9 + ((z2$Crange[z2$iscbf]) * 1e6) + (z2$highC[z2$iscbf] * 1e3)
		#z2$rank[!z2$iscbf] = z2$rank[!z2$iscbf] + ((z2$Crange[!z2$iscbf]) * 1e-3) + (z2$highC[!z2$iscbf] * 1e-6)
	} else if (type %in% c("bivs", "bivd", "bivg")) {
		z2$hueType = ifelse(z2$HwidthL >= .C4A$HwidthDivRainbow | z2$HwidthR >= .C4A$HwidthDivRainbow, "RH",
					 ifelse(z2$HwidthL < .C4A$HwidthDivSingle & z2$HwidthR < .C4A$HwidthDivSingle, "SH", "MH"))
		z2$HwidthLR = pmax(z2$HwidthL, z2$HwidthR)
		#z2$rank[z2$iscbf] = z2$rank[z2$iscbf] - 1e9 - ((!z2$highC[z2$iscbf]) * 1e6) - ((z2$hueType[z2$iscbf] == "SH") * 1e3)
		#z2$rank[!z2$iscbf] = z2$rank[!z2$iscbf] - ((!z2$highC[!z2$iscbf]) * 1e-3) - ((z2$hueType[!z2$iscbf] == "SH") * 1e-6)
	}
	z2$contrast = z2$CRmin < 120
	z2$contrastWT = z2$CRwt < 120
	z2$contrastBK = z2$CRbk < 120
	#z2$rank = floor(rank(z2$rank, ties.method = "min"))
	z2
}


get_friendlyness = function(zn) {
	with(zn, {
		ifelse(type == "cat", ifelse(min_dist >= .C4A$CBF_th$cat["min_dist"], 1,
							  ifelse(min_dist <= .C4A$CBU_th$cat["min_dist"], -1, 0)),
		ifelse(type == "seq", ifelse(min_step >= .C4A$CBF_th$seq["min_step"], 1,
							  ifelse(min_step <= .C4A$CBU_th$seq["min_step"], -1, 0)),
		ifelse(type == "div", ifelse(inter_wing_dist >= .C4A$CBF_th$div["inter_wing_dist"] & min_step >= .C4A$CBF_th$div["min_step"], 1,
							  ifelse(inter_wing_dist <= .C4A$CBU_th$div["inter_wing_dist"] | min_step <= .C4A$CBU_th$div["min_step"], -1, 0)),

		ifelse(type == "bivs", ifelse(inter_wing_dist >= .C4A$CBF_th$bivs["inter_wing_dist"] & min_step >= .C4A$CBF_th$bivs["min_step"], 1,
							   ifelse(inter_wing_dist <= .C4A$CBU_th$bivs["inter_wing_dist"] | min_step <= .C4A$CBU_th$bivs["min_step"], -1, 0)),
	   ifelse(type == "bivc", ifelse(min_dist >= .C4A$CBF_th$cat["min_dist"], 1,
	   							 ifelse(min_dist <= .C4A$CBU_th$cat["min_dist"], -1, 0)),
			   	   ifelse(type == "bivd", ifelse(inter_wing_dist >= .C4A$CBF_th$bivd["inter_wing_dist"] & min_step >= .C4A$CBF_th$bivd["min_step"], 1,
   							  ifelse(inter_wing_dist <= .C4A$CBU_th$bivd["inter_wing_dist"] | min_step <= .C4A$CBU_th$bivd["min_step"], -1, 0)),
	   ifelse(type == "bivg", ifelse(inter_wing_dist >= .C4A$CBF_th$bivg["inter_wing_dist"] & min_step >= .C4A$CBF_th$bivg["min_step"], 1,
							   ifelse(inter_wing_dist <= .C4A$CBU_th$bivg["inter_wing_dist"] | min_step <= .C4A$CBU_th$bivg["min_step"], -1, 0)), 0)))))))
	})
}
