format_ids = function(x) {
	n = length(x)

	s = split(x, cumsum(c(TRUE, diff(x)!=1)))

	paste(sapply(s, function(si) {
		if (length(si) == 1) {
			si
		} else {
			paste(si[1], si[length(si)], sep = "-")
		}
	}), collapse = ", ")
}

c4a_plot_hues = function(pal, dark = FALSE, C_grey = 5, width = c("none", "total", "halves")) {
	width = match.arg(width)

	n = length(pal)

	hcl = get_hcl_matrix(pal)

	# hue width
	h = round(hcl[,1])
	h[hcl[,2]<=.C4A$Cgray] = NA

	Hwidth = get_hue_width(h)

	is_even = ((n %% 2) != 1)
	nh = floor(n/2)

	hL = h[1:nh]
	hR = h[(nh+1+!is_even):n]


	Hwidth = get_hue_width(h)
	HwidthL = get_hue_width(hL)
	HwidthR = get_hue_width(hR)





	bc = if (dark) "#000000" else "#FFFFFF"
	fc = if (dark) "#FFFFFF" else "#000000"





	grid::grid.newpage()
	grid::pushViewport(grid::viewport(width = grid::unit(1, "snpc"), height = grid::unit(1, "snpc")))

	grid::grid.rect(gp = grid::gpar(fill = bc, col = NA))

	cx = 0.5
	cy = 0.55
	rd = 0.4
	necklace_width = 0.02

	# plot necklace

	np = 5000
	necklace = data.frame(h = runif(np, min = 0, max = 360),
					 c = runif(np, min = .3, max = 1),
					 l = runif(np, min = 30, max = 90))

	necklace$max_c = colorspace::max_chroma(h = necklace$h, l = necklace$l)
	necklace$c = necklace$c * necklace$max_c

	cols = hcl(necklace$h, necklace$c, necklace$l)

	d = runif(np, min = rd - necklace_width/2, max = rd + necklace_width/2)
	x = sin(necklace$h / 180 * pi) * d + cx
	y = cos(necklace$h / 180 * pi) * d + cy

	grid::grid.points(x = grid::unit(x, "npc"), y = grid::unit(y, "npc"), pch = 16, size = grid::unit(5, "pt"), gp = grid::gpar(col = cols))


	hcl[,1] = round(hcl[,1] / 3) * 3
	hcl[,1][hcl[,1] == 360] = 0

	rad = rep(rd, n)
	wg = (hcl[,2] <= C_grey)


	rad[wg] = 0



	h_text = rep("", 360)
	h_text_n = rep(0, 360)

	hcl[wg,1] = head(seq(0, 360, length.out = sum(wg) + 1), -1)

	for (h in 0:359) {
		ids = hcl[,1] == h & hcl[,2] > C_grey
		if (sum(ids) < 1) next
		h_text[h+1] = format_ids(which(ids))
		h_text_n[h+1] = sum(ids)
		if (sum(ids) > 1) rad[ids] = rad[ids] + seq(0, by = .02, length.out = sum(ids))
	}

	o = order(rad)

	x = cx + sin(hcl[,1] / 180 * pi) * rad
	y = cy + cos(hcl[,1] / 180 * pi) * rad

	if (sum(wg) > 1) {
		x[wg] = seq(0.5, by = 0.02, length.out = sum(wg))
	} else if (sum(wg) == 1) {
		x[wg] = 0.5
	}

	y[wg] = 0.05

	ht = (0:359)[h_text!=""]
	nt = h_text_n[h_text!=""]
	xt = cx + sin(ht / 180 * pi) * .35
	yt = cy + cos(ht / 180 * pi) * .35
	tt = h_text[h_text!=""]

	rt = rep("center", length(ht))
	rt[ht > 225 & ht < 315] = "left"
	rt[ht > 45 & ht < 135] = "right"


	plot_Hw = function(HWidth) {
		HL = attr(Hwidth, "hueL")
		HR = attr(Hwidth, "hueR")
		grid::grid.lines(x = grid::unit(c(cx, cx + sin(HL / 180 * pi) * (rd-0.15)), "npc"),
						 y = grid::unit(c(cy, cy + cos(HL / 180 * pi) * (rd-0.15)), "npc"), arrow = arrow(length = grid::unit(0.02, "npc")))
		grid::grid.lines(x = grid::unit(c(cx, cx + sin(HR / 180 * pi) * (rd-0.15)), "npc"),
						 y = grid::unit(c(cy, cy + cos(HR / 180 * pi) * (rd-0.15)), "npc"), arrow = arrow(length = grid::unit(0.02, "npc")))
		if (HR < HL) HR = HR + 360
		coords = t(sapply(HL:HR, function(h) {
			c(cx + sin(h/180*pi) * 0.07,
			  cy + cos(h/180*pi) * 0.07)
		}))
		th = (HL+HR)/2
		tcoords = c(cx + sin(th/180*pi) * 0.085,
					cy + cos(th/180*pi) * 0.085)

		grid::grid.polyline(x = grid::unit(coords[,1], "npc"),
							y = grid::unit(coords[,2], "npc"))
		grid::grid.text(paste0(HWidth, "°"),
						x = grid::unit(tcoords[1], "npc"),
						y = grid::unit(tcoords[2], "npc"),
						just = "right")
	}

	# if (width == "total") {
	# 	plot_Hw(Hwidth)
	# } else if (width == "halves") {
	# 	plot_Hw(HwidthL)
	# 	plot_Hw(HwidthR)
	# }


	#grid::grid.circle(r = .4, gp = grid::gpar(fill = NA, col = "#AAAAAA", lwd = 2))
	grid::grid.points(x = grid::unit(x[o], "npc"), y = grid::unit(y[o], "npc"), size = grid::unit(2, "lines"), pch = 21, gp = grid::gpar(fill = pal[o], col = bc, lwd = 6))
	grid::grid.points(x = grid::unit(x[o], "npc"), y = grid::unit(y[o], "npc"), size = grid::unit(2, "lines"), pch = 21, gp = grid::gpar(fill = pal[o], col = fc, lwd = 1))

	if (any(!wg)) {
		for (rts in c("left", "right", "center")) {
			if (any(rt == rts)) {
				grid::grid.text(tt[rt == rts], x = grid::unit(xt[rt == rts], "npc"), y = grid::unit(yt[rt == rts], "npc"), just = rts, gp = grid::gpar(col = fc))
			}
		}

	}
	if (any(wg)) {
		grid::grid.text(paste0("(Almost) grayscale: ",format_ids(which(wg))), x = grid::unit(0.45, "npc"), y = grid::unit(0.05, "npc"), just = "right", gp = grid::gpar(col = fc))
	}


}
