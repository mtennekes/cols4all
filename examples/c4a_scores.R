c4a_scores("blues3")

pals = c4a_palettes(type = "cat")
scores_cat7 = t(sapply(pals, c4a_scores, n = 7))

head(scores_cat7)
