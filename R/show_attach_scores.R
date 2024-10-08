show_attach_scores = function(z) {
	type = z$type[1]
	if (!all(z$type == type)) stop("mixed palette types not allowed")


	k = nrow(z)

	s = .C4A$s

	s[,.C4A$score_x100,] = s[,.C4A$score_x100,] / 100


	s2 = s[match(z$fullname, dimnames(s)[[1]]), , , drop = FALSE]
	s3 = do.call(rbind, lapply(1:k, function(i) {
		# maximum n to take scores from (cat: dim max, seq/div, the scores for the largest palettes)
		mmax = if (type == "cat") dim(s2)[3] else min(z$n[i], utils::tail(which(!is.na(s2[i, "min_dist", ])), 1))
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
	#ßz2$iscbf = (z2$cbfriendly == 1)
	#a = t(mapply(analyse_hcl, z2$palette, z2$type))
	#z2 = cbind(z2, a)

	z2$chroma = "M"

	z2$chroma[z2$Cmax >= .C4A$Cintense] = "H"
	z2$chroma[z2$Cmax < .C4A$Cpastel] = "L"


	#z2$highC = z2$Cmax >= .C4A$Cintense

	z2$Hspread = round(get_spread(z2$Hwidth, z2$n))


	if (type %in% c("cat", "bivc")) {
		z2$fairness = get_fairness(z2$Lrange, z2$Crange)
		z2$fair = ifelse(z2$fairness >= .C4A$LC_fair, "H", ifelse(z2$fairness < .C4A$LC_unfair, "L", "M"))
		# z2$fair = ifelse(z2$Crange < .C4A$CrangeFair & z2$Lrange < .C4A$LrangeFair, "H",
		# 					ifelse(z2$Crange > .C4A$CrangeUnfair | z2$Lrange > .C4A$LrangeUnfair, "L", "M"))
		# z2$fairRank = rank(c("H" = 2000000, "M" = 1000000, "L" = 0)[z2$fair] + (999000 - z2$Crange * 1000) + (999 - z2$Lrange))
	} else {
		z2$fairness = get_fairness(0, z2$Crange)
		z2$fair = ifelse(z2$fairness >= .C4A$LC_fair, "H", ifelse(z2$fairness < .C4A$LC_unfair, "L", "M"))
		# z2$fair = ifelse(z2$Crange < .C4A$CrangeFair, "H",
		# 			 ifelse(z2$Crange > .C4A$CrangeUnfair, "L", "M"))
		# z2$fairRank = rank(z2$Crange)
	}


	if (type == "div") {
		z2$hues = ifelse(z2$HwidthL >= .C4A$HwidthDivRainbow | z2$HwidthR >= .C4A$HwidthDivRainbow, "RH",
					 ifelse(z2$HwidthL < .C4A$HwidthDivSingle & z2$HwidthR < .C4A$HwidthDivSingle, "SH", "MH"))
		z2$HwidthLR = pmax(z2$HwidthL, z2$HwidthR)
	} else if (type == "seq") {
		z2$hues = ifelse(z2$Hwidth < .C4A$HwidthSeqSingle, "SH", ifelse(z2$Hwidth < .C4A$HwidthSeqRainbow, "MH", "RH"))
	} else if (type %in% c("cat")) {
		z2$hues = ifelse(z2$Hspread > .C4A$Hspread, "RH", "NA")
	} else if (type %in% c("bivs", "bivd", "bivg")) {
		z2$hues = ifelse(z2$HwidthL >= .C4A$HwidthDivRainbow | z2$HwidthR >= .C4A$HwidthDivRainbow, "RH",
					 ifelse(z2$HwidthL < .C4A$HwidthDivSingle & z2$HwidthR < .C4A$HwidthDivSingle, "SH", "MH"))
		z2$HwidthLR = pmax(z2$HwidthL, z2$HwidthR)
	}
	z2$equiluminance = z2$CRmin < .C4A$contrastEL
	z2$contrastWT = z2$CRwt < .C4A$contrastTxt
	z2$contrastBK = z2$CRbk < .C4A$contrastTxt

	z2$float = z2$Blues >= .C4A$Blues

	z2$H[z2$Hwidth >= 180] = 360

	z2$nameable = as.logical(z2$nameability)



	z2
}

get_spread = function(Hwidth, n) {
	one_piece = (360 / n)

	mx = 360 - one_piece

	Hwidth / mx * 100

}



get_friendlyness = function(zn) {
	with(zn, {
		ifelse(type == "cat", (min_dist / 1000) + ifelse(min_dist >= .C4A$CBVF_th$cat["min_dist"], 2, ifelse(min_dist >= .C4A$CBF_th$cat["min_dist"], 1,
							  ifelse(min_dist <= .C4A$CBU_th$cat["min_dist"], -1, 0))),


		ifelse(type == "seq", (min_dist / 1000) + ifelse(min_dist >= .C4A$CBF_th$seq["min_dist"] & tri_ineq >= .C4A$CBF_th$seq["tri_ineq"], 1,
							  ifelse(min_dist < .C4A$CBU_th$seq["min_dist"] | tri_ineq < .C4A$CBU_th$seq["tri_ineq"], -1, 0)),

		ifelse(type == "cyc", (min_dist / 1000) + ifelse(min_dist >= .C4A$CBF_th$cyc["min_dist"] & tri_ineq >= .C4A$CBF_th$cyc["tri_ineq"], 1,
			   												 ifelse(min_dist <= .C4A$CBU_th$cyc["min_dist"] | tri_ineq < .C4A$CBU_th$cyc["tri_ineq"], -1, 0)),

		ifelse(type == "div", (inter_wing_dist / 1000) + (min_step / 1e6) + ifelse(inter_wing_dist >= .C4A$CBF_th$div["inter_wing_dist"] & min_step >= .C4A$CBF_th$div["min_step"] & tri_ineq >= .C4A$CBF_th$div["tri_ineq"], 1,
							  ifelse(inter_wing_dist < .C4A$CBU_th$div["inter_wing_dist"] | min_step < .C4A$CBU_th$div["min_step"] | tri_ineq < .C4A$CBU_th$div["tri_ineq"], -1, 0)),


		ifelse(type == "bivs", (inter_wing_dist / 1000) + (min_step / 1e6) + ifelse(inter_wing_dist >= .C4A$CBF_th$bivs["inter_wing_dist"] & min_step >= .C4A$CBF_th$bivs["min_step"] & tri_ineq >= .C4A$CBF_th$bivs["tri_ineq"], 1,
							   ifelse(inter_wing_dist < .C4A$CBU_th$bivs["inter_wing_dist"] | min_step < .C4A$CBU_th$bivs["min_step"] | tri_ineq < .C4A$CBU_th$bivs["tri_ineq"], -1, 0)),


	   ifelse(type == "bivc", (min_dist / 1000) + ifelse(min_dist >= .C4A$CBF_th$cat["min_dist"], 1,
	   							 ifelse(min_dist <= .C4A$CBU_th$cat["min_dist"], -1, 0)),


		ifelse(type == "bivd", (inter_wing_dist / 1000) + (min_step / 1e6) + ifelse(inter_wing_dist >= .C4A$CBF_th$bivd["inter_wing_dist"] & min_step >= .C4A$CBF_th$bivd["min_step"] & tri_ineq >= .C4A$CBF_th$bivd["tri_ineq"], 1,
   							  ifelse(inter_wing_dist < .C4A$CBU_th$bivd["inter_wing_dist"] | min_step < .C4A$CBU_th$bivd["min_step"] | tri_ineq < .C4A$CBU_th$bivd["tri_ineq"], -1, 0)),


	   ifelse(type == "bivg", (inter_wing_dist / 1000) + (min_step / 1e6) + ifelse(inter_wing_dist >= .C4A$CBF_th$bivg["inter_wing_dist"] & min_step >= .C4A$CBF_th$bivg["min_step"] & tri_ineq >= .C4A$CBF_th$bivg["tri_ineq"], 1,
							   ifelse(inter_wing_dist < .C4A$CBU_th$bivg["inter_wing_dist"] | min_step < .C4A$CBU_th$bivg["min_step"] | tri_ineq < .C4A$CBU_th$bivs["tri_ineq"], -1, 0)), 0))))))))
	})
}
