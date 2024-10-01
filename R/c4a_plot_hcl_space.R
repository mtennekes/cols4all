add_coords2= function(df, width = 180) {
	df = within(df, {
		x = sin(H / 180 * pi) * (C / 180) * width
		y = cos(H / 180 * pi) * (C / 180) * width
	})
}

c4a_plot_hcl_space = function(Lmin = 0, Lmax = 100, Cmin = 0, Cmax = 180, Hmin = 0, Hmax = 360, colors = NULL) {
	if (!requireNamespace("plotly")) stop("Package plotly required", call. = FALSE)

	camera_eye = list(x=0.8, y=0.8, z=0.8)

	# from volcano3D: have to check how useful
	xy_aspectratio = 1
	z_aspectratio = .5
	z_axis_title_offset = 1.1
	z_axis_title_size = 12
	z_axis_angle = 0.5
	radial_axis_title_size = 12
	radial_axis_title_offset = 1.1
	axis_colour = "black"
	axis_width = 2
	axis_angle = 0


	Hlab = data.frame(H = seq(0, 330, by = 30),
					  C = 180)
	Hlab = add_coords2(Hlab)

	Clab = data.frame(H = 180,
					  C = c(45, 90, 135))
	Clab = add_coords2(Clab)

	with(rdata, {

		if (is.null(colors)) {
			hdf = hdf[hdf$H >= Hmin & hdf$H <= Hmax &
					  	hdf$C >= Cmin & hdf$C <= Cmax &
					  	hdf$L >= Lmin & hdf$L <= Lmax, ]
		} else {
			crds = as.data.frame((hex2RGB(colors) |> as("polarLUV"))@coords)[,3:1]
			hdf = data.frame(H = crds$H,
							 L = crds$L,
							 Crel = 1,
							 Cmax = crds$C,
							 C = crds$C,
							 hex = colors,
							 prob = 1e15)
			hdf = add_coords2(hdf)
			hdf$text = paste0("Color ", 1:length(colors), "\n", hdf$hex, "\n", "H ", round(hdf$H), ", C ", round(hdf$C), ", L ", round(hdf$L))

		}





		max_offset = max(c(z_axis_title_offset, radial_axis_title_offset))
		xyrange <- c(-1.05*(max_offset)*hdf_pg@r,
					 1.05*max_offset*hdf_pg@r)
		axis_settings <- list(title = "", zeroline = FALSE, showline = FALSE,
							  showticklabels = FALSE, showgrid = FALSE,
							  autotick = FALSE, showspikes = FALSE)
		axis_settings_xy <- list(title = "", zeroline = FALSE, showline = FALSE,
								 showticklabels = FALSE, showgrid = FALSE,
								 autotick = FALSE, showspikes = FALSE,
								 range = xyrange)


		plotly::plot_ly(x = hdf$x, y = hdf$y, z = hdf$L,
						#color = I("#CBCBCB"), alpha = 0.01,
						marker = list(color = hdf$hex),

						type = "scatter3d", mode = "markers", hoverinfo = "text", text = hdf$text) |>
			plotly::add_trace(x = hdf_pg@polar_grid$x / hdf_pg@r * 180,
							  y = hdf_pg@polar_grid$y / hdf_pg@r * 180,
							  z = hdf_pg@polar_grid$z,
							  color = I("#CBCBCB"), line = list(width = 2), showlegend = FALSE,
							  type = "scatter3d", mode = "lines", hoverinfo = "none",
							  inherit = FALSE) |>
			plotly::add_trace(x = hdf_pg@polar_grid$x / hdf_pg@r * 180,
							  y = hdf_pg@polar_grid$y / hdf_pg@r * 180,
							  z = 0, color = I("black"), line = list(width = 2),
							  showlegend = FALSE, type = "scatter3d", mode = "lines",
							  hoverinfo = "none", inherit = FALSE) |>
			plotly::add_text(
				x = radial_axis_title_offset*Hlab$x,
				y = radial_axis_title_offset*Hlab$y,
				z= 0, text = Hlab$H,
				color = I(axis_colour), type = "scatter3d", mode = "text",
				textfont = list(size = radial_axis_title_size),
				textposition = 'middle center',
				hoverinfo = 'none', showlegend = FALSE, inherit = FALSE) |>
			plotly::add_text(x = c(rep(1.05 * hdf_pg@r * sinpi(axis_angle),
									   hdf_pg@n_z_breaks)),
							 y = c(rep(1.05 * hdf_pg@r * cospi(axis_angle), hdf_pg@n_z_breaks)),
							 z = c(hdf_pg@z_breaks) * 0.95,
							 text = c(hdf_pg@z_breaks),
							 textposition = "middle left",
							 textfont = list(size = 12), color = I("black"),
							 hoverinfo = "none", showlegend = FALSE, inherit = FALSE) |>
			plotly::add_text(x = Clab$x,
							 y = Clab$y,
							 z = 0,
							 text = Clab$C,
							 textposition = "middle left", textfont = list(size = 12),
							 color = I("black"), hoverinfo = "none", showlegend = FALSE,
							 inherit = FALSE) |>
			plotly::layout(
				margin = list(0, 0, 0, 0),
				paper_bgcolor = 'rgba(0, 0, 0, 0)',
				plot_bgcolor = 'rgba(0, 0, 0, 0)',

				scene = list(
					camera = list(eye = camera_eye),
					aspectratio = list(x = xy_aspectratio,
									   y = xy_aspectratio,
									   z = z_aspectratio),
					dragmode = "turntable",
					xaxis = axis_settings_xy,
					yaxis = axis_settings_xy,
					zaxis = axis_settings
				),
				xaxis = list(title = "x"),
				yaxis = list(title = "y")
			)
	})

}
