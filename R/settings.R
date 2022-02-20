# to do: use this for ranking and cbfriendly (currently hard-coded)
.scores = list(cat = c(min_dist = 8),
			   seq = c(min_step = 5, max_step = -Inf),
			   div = c(inter_wing_dist = 10, min_step = 5))


.indicators = list(cat = c("min_dist"), seq = c("min_step", "max_step"), div = c("inter_wing_dist", "inter_wing_hue_dist", "min_step"))
#.hcl = c("Crel", "Cmax", "Hwidth", "HwidthL", "HwidthR", "Lrange", "Crange", "LCrange")
.hcl = c("Cmax", "Hwidth", "HwidthL", "HwidthR", "Lrange", "Crange")

.labels = c(min_dist = "Minimum distance", min_step = "Minimum step", max_step = "Maximum step", inter_wing_dist = "Inter-wing-distance", inter_wing_hue_dist = "Inter Wing Hue Dist", Crel = "Chroma (rel) max", Cmax = "Chroma max", Hwidth = "Hue width", HwidthL = "Hue width L", HwidthR = "Hue width R", Lrange = "Luminance range", Crange = "Chroma range", LCrange = "Lum/Chr range")



.rank = "Ranking"

.friendly = "Colorblind-friendly"

.highC = "Intense"

.hueType = "Spectrum"

.harmonic = "Harmonic"

.maxn = "Max colors"
