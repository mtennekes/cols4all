.onLoad <- function(...) {
	assign("z", .z, envir = .C4A)
	assign("s", .s, envir = .C4A)

	with(.C4A,{
		defaults = c(cat = "tol.muted", seq = "hcl.blues2", div = "hcl.purple-green")
		CBF_th = list(cat = c(min_dist = 10),
					   seq = c(min_step = 5),
					   div = c(inter_wing_dist = 10, inter_wing_hue_dist = 100, min_step = 5)) #color-blind-friendly threshold

		Cgray = 10 # maximum chroma value to be considered as gray (used for Hwidth and c4a_add_series)
		LrangeWeight = 2/3 # LCrange (which determines harmony) is calculated as max(Lrange * LrangeWeight, Crange * (1-LrangeWeight))
		LCrangeHarmonic = 80/3 # Maximum LCrange values for which the palette is labeled "harmonic"
		Cintense = 100 # chroma of colors that are considered intense
		HwidthDivRainbow = 90 # a diverging palette is labeled as 'rainbow hue' if HwidthL or HwidthR are at least HwidthDivRainbow
		HwidthDivSingle = 20 # a diverging palette is labeled as 'single hue' if HwidthL and HwidthR are at most HwidthDivSingle
		HwidthSeqRainbow = 180 # a sequential palette is labeled as 'rainbow hue' if Hwidth is at least HwidthSeqRainbow
		HwidthSeqSingle = 15 # a sequential palette is labeled as 'single hue' if Hwidth is at most HwidthSeqSingle

		sc = c("min_dist", "min_step", "max_step", "inter_wing_dist", "inter_wing_hue_dist", "rank")

		indicators = list(cat = c("min_dist"),
						  seq = c("min_step", "max_step"),
						  div = c("inter_wing_dist", "inter_wing_hue_dist", "min_step"))
		hcl = c("Cmax", "Hwidth", "HwidthL", "HwidthR", "Lrange", "Crange")

		labels = c(min_dist = "Minimum distance",
					min_step = "Minimum step",
					max_step = "Maximum step",
					inter_wing_dist = "Inter-wing-distance",
					inter_wing_hue_dist = "Inter Wing Hue Dist",
					Crel = "Chroma (rel) max",
					Cmax = "Chroma max",
					Hwidth = "Hue width",
					HwidthL = "Hue width L",
					HwidthR = "Hue width R",
					Lrange = "Luminance range",
					Crange = "Chroma range",
					LCrange = "Lum/Chr range",
					rank = "Ranking",
					cbfriendly = "Colorblind-friendly",
					highC = "Intense colors",
					hueType = "Hues",
					harmonic = "Harmonic palette",
					nmax = "Max number")

		nmax = c(cat = 36, seq = 15, div = 15)
	})
}

.C4A <- new.env(FALSE, parent=globalenv())
