.onLoad <- function(...) {
	assign("z", .z, envir = .C4A)
	assign("s", .s, envir = .C4A)

	with(.C4A,{
		defaults = c(cat = "tol.muted", seq = "hcl.blues2", div = "hcl.purple-green")
		scores = list(cat = c(min_dist = 8),
					   seq = c(min_step = 5, max_step = -Inf),
					   div = c(inter_wing_dist = 10, min_step = 5))

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
