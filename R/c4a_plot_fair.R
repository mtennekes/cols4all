logit <- function(x, midpoint = 0, steepness = 1) {
	scale <- (x - midpoint) * steepness
	1 / (1 + exp(1)^(-scale))
}



Sigma = matrix(c(1,0.5,0.5,1), ncol=2)
R = chol(Sigma) # Sigma == t(R)%*%  R
n = 1000
X = t(R) %*% matrix(rnorm(n*2), 2)

X %*% t(X)/n # test


get_fairness_before_normalisation = function(dL, dC) {
	eL = logit(dL, midpoint = .C4A$Lrange_mid, steepness = .C4A$Lrange_steep)
	eC = logit(dC, midpoint = .C4A$Crange_mid, steepness = .C4A$Crange_steep)
	(1 - eL) * (1 - eC)
	#dbinorm(dL / 100, dC / 160, mean1 = 0, mean2 = 0, var1 = .2, var2 = .2, cov12 = -.05)
}


calc_fairness = function(pal) {
	m = get_hcl_matrix(pal)
	dL = diff(range(m[,3]))
	dR = diff(range(m[,2]))
	get_fairness(dL, dR)
}


get_fairness = function(dL, dC, round = TRUE) {
	f = (get_fairness_before_normalisation(dL, dC) / get_fairness_before_normalisation(0, 0)) * 100
	if (round) round(f) else f
}


c4a_plot_fair = function(pal = NULL, type = c("LC", "C"), dark = FALSE, contours = seq(10, 90, by = 10)) {
	grid::grid.newpage()

	type = match.arg(type)

	fc = ifelse(dark, "#FFFFFF", "#000000")
	bc = ifelse(dark, "#000000", "#FFFFFF")
	gc = ifelse(dark, "#222222", "#EEEEEE")
	dc = ifelse(dark, "#BBBBBB", "#555555")

	grid::grid.rect(gp=grid::gpar(fill = bc, col = NA))
	grid::pushViewport(grid::viewport(width = grid::unit(1, "snpc"), height = grid::unit(1, "snpc"), clip = TRUE))
	grid::pushViewport(grid::viewport(layout = grid::grid.layout(3, 3,
																 widths = grid::unit(c(2.5,1,1), c("lines", "null", "lines")),
																 heights = grid::unit(c(1,1,2.5), c("lines", "null", "lines")))))

	if (!is.null(pal)) {
		hcl = get_hcl_matrix(pal)
		cr = diff(range(hcl[,2]))
	}

	if (type == "LC") {
		dL = seq(0,100, by = 1)[2:100]
		dC = seq(0,160, by = 1.6)[2:100]
		df = as.data.frame(expand.grid(L = dL, C = dC))
		df$LC = get_fairness(df$L, df$C, round = FALSE)
		m = t(matrix(df$LC, nrow = length(dL)))[length(dC):1L, ]
		df = as.data.frame(expand.grid(L = dL, C = dC))
		df$LC = get_fairness(df$L, df$C, round = FALSE)
		m = t(matrix(df$LC, nrow = length(dL)))[length(dC):1L, ]

		clines = contourLines(z = m, levels = contours) #c(.C4A$LC_fair / 100, .C4A$LC_unfair / 100)

		clines = lapply(clines, function(cli) {
			within(cli, {
				L = y * 100
				C = 160 - (x * 160)
			})
		})

		sq = 2
		marg = 1.5

		if (!is.null(pal)) {
			lr = diff(range(hcl[,3]))
			LC = get_fairness(lr, cr)
		}
	} else {
		sq = 2
		marg = 1.5
		dC = seq(0,160, by = .1)
		Fs = get_fairness(0, dC, round = FALSE)
		if (!is.null(pal)) {
			LC = get_fairness(0, cr)
		}
	}



	if (type == "LC") {

		cellplot(2,2, {
			grid::grid.rect(gp = grid::gpar(fill = gc, col = fc))


			#grid::grid.raster(m/100, x = 0.5, y = 0.5, width = 1, height = 1)

			for (cli in clines) {
				grid::grid.polyline(x = grid::unit(cli$C, "native"), y = grid::unit(cli$L, "native"), gp = grid::gpar(col = dc))
				id = round(length(cli$x) * .75)
				grid::grid.rect(x = grid::unit((1 - cli$x[id]) * 160, "native"), y = grid::unit(cli$y[id] * 100, "native"), width = grid::unit(1.3, "lines"), height = grid::unit(1, "lines"), gp = grid::gpar(col = NA, fill = gc))
				grid::grid.text(cli$level, x = grid::unit((1 - cli$x[id]) * 160, "native"), y = grid::unit(cli$y[id] * 100, "native"), gp = grid::gpar(col = fc))
			}

			# contourTextIds = sapply(contours, FUN = function(ci) {
			# 	which.min(abs(ci - diag(m[length(dL):1L,]) * 100))
			# })
			#
			# grid::grid.text(contours, x = grid::unit(dC[contourTextIds], "native"), y= grid::unit(dL[contourTextIds], "native"))

			if (!is.null(pal)) {
				if (cr < marg*1.8) cr = marg*1.8
				if (lr < marg) lr = marg

				cr2 = cr + c(-marg, marg) * 1.8
				lr2 = lr + c(-marg, marg)
			}



			#grid::grid.points(m[,2], m[,3], pch = 15, gp = grid::gpar(col = cols))
			if (!is.null(pal)) {
				grid::grid.rect(grid::unit(cr, "native"), grid::unit(lr, "native"), width = grid::unit(sq * 1.8, "native"), height = grid::unit(sq, "native"), gp = grid::gpar(col = bc, fill = fc))

				grid::grid.rect(x = grid::unit(cr, "native") + grid::unit(0.75, "lines"), y = grid::unit(lr, "native"), width = grid::unit(1, "lines"), height = grid::unit(1, "lines"), gp = grid::gpar(col = NA, fill = gc), just = "left")
				grid::grid.text(round(LC), x = grid::unit(cr, "native") + grid::unit(0.75, "lines"), y = grid::unit(lr, "native"), gp = grid::gpar(col = fc), just = "left")
			}
		}, yscale = c(0,100), xscale = c(0,180))

		cellplot(2, 1, {
			s = seq(10, 90, by = 10)
			k = length(s)
			grid::grid.polyline(rep(c(0.85, 1), k), grid::unit(rep(s, each  = 2), "native"), id = rep(1:k, each = 2), gp = grid::gpar(col = fc))
			grid::grid.text(s, rep(0.75, k), grid::unit(s, "native"), just = "right", gp = grid::gpar(col = fc, cex = 0.8))
			grid::grid.text("Luminance range", rot = 90, x = 0.2, gp = grid::gpar(col = fc))
		}, yscale = c(0,100))

		cellplot(3, 2, {
			s = seq(20, 160, by = 20)
			k = length(s)
			grid::grid.polyline(grid::unit(rep(s, each  = 2), "native"), rep(c(0.85, 1), k), id = rep(1:k, each = 2), gp = grid::gpar(col = fc))
			grid::grid.text(s, grid::unit(s, "native"), rep(0.65, k), just = "center", gp = grid::gpar(col = fc, cex = 0.8))
			grid::grid.text("Chroma range", y = .2, gp = grid::gpar(col = fc))
		}, xscale = c(0,180))

	} else {

		cellplot(2,2, {
			grid::grid.rect(gp = grid::gpar(fill = gc, col = fc))

			grid::grid.polyline(x = grid::unit(dC, "native"), y = grid::unit(Fs, "native"))

			#grid::grid.points(m[,2], m[,3], pch = 15, gp = grid::gpar(col = cols))
			if (!is.null(pal)) {
				grid::grid.rect(grid::unit(cr, "native"), grid::unit(LC, "native"), width = grid::unit(sq * 1.8, "native"), height = grid::unit(sq, "native"), gp = grid::gpar(col = bc, fill = fc))

				grid::grid.rect(x = grid::unit(cr, "native") + grid::unit(0.75, "lines"), y = grid::unit(LC, "native"), width = grid::unit(1, "lines"), height = grid::unit(1, "lines"), gp = grid::gpar(col = NA, fill = gc), just = "left")
				grid::grid.text(round(LC), x = grid::unit(cr, "native") + grid::unit(0.75, "lines"), y = grid::unit(LC, "native"), gp = grid::gpar(col = fc), just = "left")			}


		}, yscale = c(0,100), xscale = c(0,180))

		cellplot(2, 1, {
			s = seq(10, 90, by = 10)
			k = length(s)
			grid::grid.polyline(rep(c(0.85, 1), k), grid::unit(rep(s, each  = 2), "native"), id = rep(1:k, each = 2), gp = grid::gpar(col = fc))
			grid::grid.text(s, rep(0.75, k), grid::unit(s, "native"), just = "right", gp = grid::gpar(col = fc, cex = 0.8))
			grid::grid.text("Fairness", rot = 90, x = 0.2, gp = grid::gpar(col = fc))
		}, yscale = c(0,100))

		cellplot(3, 2, {
			s = seq(20, 160, by = 20)
			k = length(s)
			grid::grid.polyline(grid::unit(rep(s, each  = 2), "native"), rep(c(0.85, 1), k), id = rep(1:k, each = 2), gp = grid::gpar(col = fc))
			grid::grid.text(s, grid::unit(s, "native"), rep(0.65, k), just = "center", gp = grid::gpar(col = fc, cex = 0.8))
			grid::grid.text("Chroma range", y = .2, gp = grid::gpar(col = fc))
		}, xscale = c(0,180))
	}

	grid::upViewport(2)
}

