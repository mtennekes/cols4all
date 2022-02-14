# to do: use this for ranking and cbfriendly (currently hard-coded)
.scores = list(cat = c(min_dist = 8),
			   seq = c(min_step = 5, max_step = -Inf),
			   div = c(inter_wing_dist = 10, min_step = 5))


.indicators = list(cat = c("min_dist"), seq = c("min_step", "max_step"), div = c("inter_wing_dist", "min_step"))
.hcl = c("Crel", "Hwidth")

.labels = c(min_dist = "Minimum distance", min_step = "Minimum step", max_step = "Maximum step", inter_wing_dist = "Inter-wing-distance", Crel = "Chroma max", Hwidth = "Hue width")



.rank = "Rank"

.friendly = "Colorblind-friendly"

.highC = "High chroma"


.maxn = "Max colors"
