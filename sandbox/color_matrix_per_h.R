library(grid)
library(colorspace)


LC_plot = function(h) {
	require(grid)
	require(colorspace)
	grid.newpage()

	pushViewport(viewport(layout=grid.layout(ncol=21,nrow=21)))

	cs = seq(0, .maxC[h+1], length.out = 20)


	ls = seq(0, 100, length.out = 20)

	mC = max_chroma(h = h, l = ls)

	maxC = max(mC)


	df = expand.grid(c = cs, l = ls)

	df$col = rep(1:20, each = 20)
	df$row = rep(1:20, times = 20)

	df$color = hcl(h = h, c =df$c, l = df$l)

	for (i in 1:nrow(df)) {
		pushViewport(viewport(layout.pos.row = df$row[i], layout.pos.col = df$col[i]))
		if (df$c[i] <= mC[df$col[i]]) grid.rect(gp=gpar(fill=df$color[i]))
		popViewport()
	}

	for (i in 1:20) {
		pushViewport(viewport(layout.pos.row = 21, layout.pos.col = i))
		grid.text(round(ls[i]))
		popViewport()
	}

	for (i in 1:20) {
		pushViewport(viewport(layout.pos.row = i, layout.pos.col = 21))
		grid.text(round(cs[i]))
		popViewport()
	}
}

HC_plot = function(l, cmax, relCmax = TRUE) {
	require(grid)
	require(colorspace)
	grid.newpage()

	pushViewport(viewport(layout=grid.layout(ncol=21,nrow=21)))

	hs = seq(0, 360, length.out = 20)

	if (relCmax) {
		cratio = seq(0, 1, length.out = 20)
		df = expand.grid(cratio = cratio, h = hs)
		df$c = max_chroma(df$h, l) * df$cratio
	} else {
		cs = seq(0, cmax, length.out = 20)
		df = expand.grid(c = cs, h = hs)
	}

	df$col = rep(1:20, each = 20)
	df$row = rep(1:20, times = 20)

	df$color = hcl(h = df$h, c =df$c, l = l)

	for (i in 1:nrow(df)) {
		pushViewport(viewport(layout.pos.row = df$row[i], layout.pos.col = df$col[i]))
		if (df$c[i] <= max_chroma(df$h, l)) grid.rect(gp=gpar(fill=df$color[i]))
		popViewport()
	}

	for (i in 1:20) {
		pushViewport(viewport(layout.pos.row = 21, layout.pos.col = i))
		grid.text(round(hs[i]))
		popViewport()
	}

	for (i in 1:20) {
		pushViewport(viewport(layout.pos.row = i, layout.pos.col = 21))
		if (!relCmax) grid.text(round(cs[i]))
		popViewport()
	}
}


HL_plot = function(c) {
	require(grid)
	require(colorspace)
	grid.newpage()

	pushViewport(viewport(layout=grid.layout(ncol=21,nrow=21)))

	hs = seq(0, 360, length.out = 20)
	ls = seq(0, 100, length.out = 20)

	df = expand.grid(l = ls, h = hs)

	df$col = rep(1:20, each = 20)
	df$row = rep(1:20, times = 20)

	df$color = hcl(h = df$h, c = c, l = df$l)

	for (i in 1:nrow(df)) {
		pushViewport(viewport(layout.pos.row = df$row[i], layout.pos.col = df$col[i]))
		if (c <= max_chroma(h = df$h[i], l = df$l[i])) grid.rect(gp=gpar(fill=df$color[i]))
		popViewport()
	}

	for (i in 1:20) {
		pushViewport(viewport(layout.pos.row = 21, layout.pos.col = i))
		grid.text(round(hs[i]))
		popViewport()
	}

	for (i in 1:20) {
		pushViewport(viewport(layout.pos.row = i, layout.pos.col = 21))
		grid.text(round(ls[i]))
		popViewport()
	}
}
HL_plot(70)


HC_plot(50, cmax = 180)
HC_plot(50, cmax = 180, relCmax = FALSE)


for (h in seq(0, 360, by = 25)) LC_plot(h)
