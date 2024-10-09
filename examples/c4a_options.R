# Example how to lower the color-blind friendly threshold
# for categorical palettes (so more smileys in the GUI!)
# CBF_th: one smiley
# CBVF_th: two smileys

# current table
\dontrun{
c4a_table(n = 9, sort = "cbfriendly")

opts = c4a_options("CBF_th", "CBVF_th")
opts$CBF_th$cat["min_dist"] = 7
opts$CBVF_th$cat["min_dist"] = 10


old = c4a_options(opts)

# more smileys :-) :-)
c4a_table(n = 9, sort = "cbfriendly")

# set the old settings back
c4a_options(old)
}

# Example how to use own nameability function
#
# This function should:
# - have an argument "pal" (vector of colors)
# - optionally have other arguments
# - return a distance matrix of n rows (length of pal) and k columns (classes).
#   It shoud have columns names that correspond to the naming colors (see below).
naming_RGB = function(pal) {
	cols = colorspace::hex2RGB(pal)
	coords = cols@coords

	cls = apply(coords, MARGIN = 1, which.max)
	mx = apply(coords, MARGIN = 1, max)
	dominance = ((mx + 0.001) / (rowSums(coords) + 0.001))
	cls[dominance < 0.4] = 4L

	m = matrix(0, nrow = length(pal), ncol = 4,
        dimnames = list(NULL, c("Red", "Green", "Blue", "Other")))
	for (i in 1:nrow(m)) {
		m[i, cls[i]] = 1
	}

	-m
}

# testing this function...
naming_RGB(c4a("brewer.set1")) #fair enough

# This vector should contain the 'prototype' colors, and have names that correspond
# to the column names of the returned matrices by the function above.
names_RGB =
    c("Red" = "#FF0000",
      "Green" = "#00FF00",
      "Blue" = "#0000FF",
      "Other" = "#AAAAAA")

# Set the options (may take a while because if calculated the nameability scores)
\dontrun{
c4a_options(naming_fun = naming_RGB,
			naming_fun_args = list(),
			naming_colors = names_RGB)
}
