def_n = function(npref = NA, type, series, tab_nmin, tab_nmax) {

	nmin = suppressWarnings(min(tab_nmin[series, type], na.rm = TRUE))
	nmax = suppressWarnings(max(tab_nmax[series, type], na.rm = TRUE))

	if (is.na(npref)) npref = switch(type, cat = 7, seq = 7, div = 9, 3)
	if (is.infinite(nmax)) nmax = 15
	if (nmin < 2) nmin = 2

	n = if (is.infinite(nmin)) {
		npref # should not happen or else throw a message elsewhere
	} else if (nmin < npref && nmax > npref) {
		npref
	} else if (is.infinite(nmax)) {
		nmin
	} else {
		nmax
	}
	list(n = n, nmin = nmin, nmax = nmax)
}



#' @rdname c4a_gui
#' @name c4a_gui
#' @export
c4a_gui = function(type = "cat", n = NA, series = c("misc", "brewer", "scico", "hcl", "tol", "viridis", "c4a")) {
	if (!requireNamespace("shiny")) stop("Please install shiny")
	if (!requireNamespace("shinyjs")) stop("Please install shinyjs")
	if (!requireNamespace("kableExtra")) stop("Please install kableExtra")

	shiny::addResourcePath(prefix = "imgResources", directoryPath = system.file("img", package = "cols4all"))

	#############################
	## Catelogue tab
	#############################

	z = .C4A$z

	z = z[order(z$fullname), ]

	tps = unname(.C4A$types)

	tab_nmin = tapply(z$nmin, INDEX = list(z$series, factor(z$type, levels = tps)), FUN = min)
	tab_nmax = tapply(z$nmax, INDEX = list(z$series, factor(z$type, levels = tps)), FUN = max)
	tab_k = as.data.frame(tapply(z$nmin, INDEX = list(z$series, factor(z$type, levels = tps)), FUN = length))
	tab_k$series = rownames(tab_k)
	tab_k = tab_k[, c("series", tps)]


	allseries = sort(unique(z$series))
	if (series[1] == "all") {
		series = allseries
	} else {
		if (!all(series %in% allseries)) message("These series do not exist: \"", paste(setdiff(series, allseries), collapse = "\", \""), "\"")
		series = intersect(series, allseries)
	}
	if (!length(series)) {
		message("No palette series to show. Either restart c4a_gui with different parameters, add series with c4a_palettes_add, or import data with c4a_sysdata_import")
		return(invisible(NULL))
	}

	types_available = names(which(apply(tab_nmin, MARGIN = 2, function(x)any(!is.na(x)))))

	stopifnot(length(types_available) > 0L)
	if (!(type %in% types_available)) {
		type = types_available[1]
	}


	ns = def_n(npref = n, type, series, tab_nmin, tab_nmax)

	types = .C4A$types

	types1 = .C4A$types1
	types2 = .C4A$types2

	if (nchar(type) == 3) {
		type1 = type
		type2 = if (type %in% names(types2)) unname(types2[[type]][1]) else ""
	} else {
		type2 = type
		type1 = substr(type, 1, 3)
	}
	type12 = paste0(type1, type2)

	#############################
	## Contrast tab
	#############################

	palette = z$fullname[1]

	x = c4a_info(palette)

	n_init = x$ndef
	pal_init = c(c4a(x$fullname, n = n_init), "#ffffff", "#000000")


	getNames = function(p) {
		lapply(p, function(pi) {
			shiny::HTML(paste0("<div style='font-size:2em;line-height:0.5em;height:0.5em;color:", pi, "'>&#9632;</div>"))
		})
	}





	ui = shiny::fluidPage(
		shinyjs::useShinyjs(),
		shiny::tags$head(shiny::includeCSS(system.file("www/light.css", package = "cols4all"))),
		shiny::tags$head(shiny::includeCSS(system.file("www/dark.css", package = "cols4all"))),
		shiny::tags$head(shiny::includeScript(system.file("www/func.js", package = "cols4all"))),
		shiny::tabsetPanel(
			id="inTabset",
			shiny::tabPanel("Catelogue",
							value = "tab_catel",
					 shiny::fluidRow(
					 	shiny::column(width = 3,
					 				  shiny::img(src = "imgResources/cols4all_logo.png", height="200", align = "center", 'vertical-align' = "center")),
					 	shiny::column(width = 3,
					 				  shiny::radioButtons("type1", "Palette Type", choices = types1, selected = type1),
					 				  shiny::conditionalPanel(
					 				  	condition = "input.type1 == 'biv'",
					 				  	shiny::selectizeInput("type2", "Subtype", choices = types2[["biv"]], selected = type2)),
					 				  shiny::div(style = "margin-bottom: 5px;", shiny::strong("Palette series")),
					 				  shiny::div(class = 'multicol',
					 				  		   shiny::checkboxGroupInput("series", label = "", choices = allseries, selected = series, inline = FALSE)),
					 				  shiny::fluidRow(
					 				  	shiny::column(12, align="right",
					 				  	shiny::actionButton("all", label = "All"),
					 				  	shiny::actionButton("none", label = "None"),
					 				  	shiny::actionButton("overview", label = "Overview")))),
					 	shiny::column(width = 3,
					 				  shiny::conditionalPanel(
					 				  	condition = "input.type1 != 'biv'",
					 				  	shiny::sliderInput("n", "Number of colors", min = ns$nmin, max = ns$nmax, value = ns$n, ticks = FALSE)),
					 				  shiny::conditionalPanel(
					 				  	condition = "input.type1 == 'biv'",
					 				  	shiny::fluidRow(
					 				  		shiny::column(6,
					 				  					  shiny::sliderInput("nbiv", "Number of columns", min = 3, max = 7, value = 3, ticks = FALSE)),
					 				  		shiny::column(6,
					 				  					  shinyjs::disabled(shiny::sliderInput("mbiv", "Number of rows", min = 3, max = 7, value = 3, ticks = FALSE))))),
					 				  shiny::checkboxInput("na", "Color for missing values", value = FALSE),
					 				  shiny::conditionalPanel(
					 				  	condition = "input.type1 == 'seq' || input.type1 == 'div'",
					 				  	shiny::fluidRow(
					 				  		shiny::column(4,
					 				  					  #shiny::br(),
					 				  					  shiny::radioButtons("auto_range", label = "Range", choices = c("Automatic", "Manual"), selected = "Automatic")),
					 				  		shiny::conditionalPanel(
					 				  			condition = "input.auto_range == 'Manual'",
					 				  			shiny::column(8,
					 				  						  shiny::div(style = "font-size:0;margin-bottom:-10px", shiny::sliderInput("range", "",
					 				  						  																		 min = 0, max = 1, value = c(0,1), step = .05)),
					 				  						  shiny::uiOutput("range_info"))))),
					 				  shiny::fluidRow(
					 				  	shiny::column(6,
					 				  		shiny::radioButtons("format", "Text format", choices = c("Hex" = "hex", "RGB" = "RGB", "HCL" = "HCL"), inline = FALSE)
					 				  	),
					 				  	shiny::column(6,
					 				  		shiny::radioButtons("textcol", "Text color", choices = c("Hide text" = "same", Black = "#000000", White = "#FFFFFF", Automatic = "auto"), inline = FALSE)	))),
					 	shiny::column(
					 		width = 3,
					 		shiny::radioButtons("cvd", "Color vision", choices = c(Normal = "none", 'Deutan (red-green blind)' = "deutan", 'Protan (also red-green blind)' = "protan", 'Tritan (blue-yellow)' = "tritan"), selected = "none"),
					 		shiny::selectizeInput("sort", "Sort", choices = structure(c("name", "rank"), names = c("Name", .C4A$labels["cbfriendly"])), selected = "name"),
					 		shiny::checkboxInput("sortRev", "Reverse sorting", value = FALSE),
					 		shiny::checkboxInput("advanced", "Show underlying scores", value = FALSE),
					 		shiny::checkboxInput("dark", "Dark mode", value = FALSE)
					 	),

					 ),

					 shiny::fluidRow(
					 	shiny::column(
					 		width = 11,
					 		shiny::tableOutput("show"))
					 )
			),
			shiny::tabPanel("Color Blind Friendliness",
							value = "tab_cvd",
					shiny::selectizeInput("cbfPal", "Palette", choices = z$fullname),
					#shiny::plotOutput("cbfPlot", "Color Blind Friendliness simulation", width = "800px", height = "200px"),
					shiny::plotOutput("cbfRGB", "Confusion lines", width = "800px", height = "800px")),
			shiny::tabPanel("Application",
							value = "tab_app",
					shiny::selectizeInput("appPal", "Palette", choices = z$fullname),
					shiny::plotOutput("CLplot", "CL plot", width = "600px", height = "600px")),
			shiny::tabPanel("Contrast (borders needed?)",
							value = "tab_cont",
				 	shiny::fluidRow(
				 		shiny::column(width = 12,
				 		shiny::markdown("Two colors shown next to each other are pereived *unstable* when they are equally light (luminant). Even though they may have totally different hues, it is hard to separate the colored shapes. The go-to solution is to use black or white shape borders.")),
			 			shiny::column(width = 3,
			 						  shiny::selectizeInput("contrastPal", "Palette", choices = z$fullname),
			 						  shiny::fluidRow(
			 						  	shiny::column(width = 6,
			 						  				  shiny::radioButtons("col1", "Color 1",
			 						  				  					choiceNames = getNames(pal_init),
			 						  				  					choiceValues = pal_init, selected = pal_init[1])),
			 						  	shiny::column(width = 6,
			 						  				  shiny::radioButtons("col2", "Color 2",
			 						  				  					choiceNames = getNames(pal_init),
			 						  				  					choiceValues = pal_init, selected = pal_init[2]))
			 						  )
			 			),
			 			shiny::column(width = 9,
			 						  shiny::markdown("**Contrast Ratio**"),
			 						  shiny::plotOutput("table", height = "300px", width = "400px"),
			 						  shiny::markdown("Contrast Ratio = (L1 + 0.05) / (L2 + 0.05), where L1 and L2 are the luminances (normalized between 0 and 1) of the lighter and darker colors, respectively."))),
				 	shiny::fluidRow(
				 		shiny::column(width = 3,
				 					  shiny::radioButtons("chart", "Example chart", c("Choropleth", "Barchart"), "Choropleth", inline = FALSE),
			 					  	  shiny::sliderInput("lwd", "Line Width", min = 0, max = 3, step = 1, value = 0),
				 					  shiny::selectInput("borders", "Borders", choices = c("black", "white"), selected = "black")),
				 		shiny::column(
				 			width = 9,
				 			shiny::plotOutput("ex", height = "300px", width = "600px")
				 		)

				 	),
				 	shiny::fluidRow(
				 		shiny::column(width = 12,
				 					  shiny::markdown("**Optical Art** "),
				 					  shiny::plotOutput("ex_plus", height = "703", width = "900"),
				 					  shiny::markdown("_Plus Reversed_ by Richard Anuszkiewicz (1960)")
				 		)
					)

			)

		)



	)
	server = function(input, output, session) {
		#############################
		## Catelogue tab
		#############################

		series_d = shiny::debounce(shiny::reactive(input$series), 300)

		get_type12 = shiny::reactive({
			type1 = input$type1
			type12 = if (type1 %in% names(types2)) {
				input$type2
			} else {
				type1
			}
		})

		shiny::observeEvent(input$all, {
			shiny::freezeReactiveValue(input, "series")
			shiny::updateCheckboxGroupInput(session, "series", selected = allseries)
		})

		shiny::observeEvent(input$none, {
			shiny::freezeReactiveValue(input, "series")
			shiny::updateCheckboxGroupInput(session, "series", selected = character())
		})

		shiny::observeEvent(get_cols(), {
			cols = get_cols()
			sort = shiny::isolate(input$sort)
			shiny::freezeReactiveValue(input, "sort")
			sortNew = if (sort %in% cols) sort else "name"
			shiny::updateSelectInput(session, "sort", choices  = cols,selected = sortNew)
		})

		shiny::observeEvent(input$dark, {

			if ( ! input$dark ) {
				shinyjs::removeClass(selector = "body", class = "dark")
			} else {
				shinyjs::addClass(selector = "body", class = "dark")
			}

		})


		shiny::observeEvent(get_type12(), {
			type = get_type12()

			if (!(type %in% types_available)) return(NULL)
			if (type %in% c("cat", "seq", "div")) {
				series = series_d()
				ns =  def_n(npref = input$n, type, series, tab_nmin, tab_nmax)
				# print("+++++++++++++")
				# print(ns)
				shiny::freezeReactiveValue(input, "n")
				shiny::updateSliderInput(session, "n", min = ns$nmin, max = ns$nmax, value = ns$n)
			} else {
				ndef = unname(.C4A$ndef[type])
				mdef = unname(.C4A$mdef[type])

				if (is.na(mdef)) {
					shinyjs::disable("mbiv")
				} else {
					shinyjs::enable("mbiv")
				}
				nmin = if (type == "bivd") 3 else 2
				nstep = if (type == "bivd") 2 else 1

				shiny::freezeReactiveValue(input, "nbiv")
				shiny::freezeReactiveValue(input, "mbiv")
				shiny::updateSliderInput(session, "nbiv", value = ndef, min = nmin, step = nstep)
				shiny::updateSliderInput(session, "mbiv", value = mdef, min = nmin, step = nstep)
			}
			# print("/+++++++++")
		})


		shiny::observeEvent(series_d(), {
			type = get_type12()

			if (!(type %in% c("cat", "seq", "div"))) return(NULL)
			series = series_d()
			ns =  def_n(npref = input$n, type, series, tab_nmin, tab_nmax)
			# print("-------------")
			# print(ns)
			shiny::freezeReactiveValue(input, "n")
			shiny::updateSliderInput(session, "n", min = ns$nmin, max = ns$nmax, value = ns$n)
			# print("/-------------")
		})

		shiny::observeEvent(input$overview, {
			type = get_type12()#rv$type12
			title = paste0("Overview of palettes per series of type ", type)
			shiny::showModal(shiny::modalDialog(title = "Number of palettes per series (rows) and type (columns)",
												shiny::renderTable(tab_k, na = "", striped = TRUE, hover = TRUE, bordered = TRUE),
												shiny::div(style="font-size: 75%;", shiny::renderTable(.C4A$type_info)),
												footer = shiny::modalButton("Close"),
												style = "color: #000000;"))
		})

		get_cols = shiny::reactive({
			print(names(session$userData))
			type = get_type12()
			res = table_columns(type, input$advanced)
			anyD = duplicated(res$ql)
			structure(c("name", res$qn[!anyD]), names = c("Name", res$ql[!anyD]))
		})



		get_values = shiny::reactive({
			if (input$sort == "") return(NULL)

			type = get_type12()
			list(n = input$n,
				 nbiv = input$nbiv,
				 mbiv = input$mbiv,
				 type = type,
				 cvd = input$cvd,
				 sort = input$sort,
				 sortRev = input$sortRev,
				 series = series_d(),
				 show.scores = input$advanced,
				 columns = if (input$n > 16) 12 else input$n,
				 na = input$na,
				 range = if (input$auto_range == "Automatic") NA else input$range,
				 textcol = input$textcol,
				 format = input$format)
		})
		#get_values_d = shiny::debounce(get_values, 300)


		shiny::observeEvent(input$nbiv, {
			nbiv = input$nbiv
			type = get_type12()#rv$type12
			if (type == "bivs") {
				shiny::freezeReactiveValue(input, "mbiv")
				shiny::updateSliderInput(session, "mbiv", value = nbiv)
			}
		})

		# shiny::observe({
		# 	n = input$n
		# 	ac = input$auto_range
		# 	type = input$type
		#
		# 	if (type == "cat") return(NULL)
		# 	if (ac != "Manual") {
		# 		shiny::freezeReactiveValue(input, "range")
		# 		shinyjs::disable("range")
		# 		if (ac == "Maximum") {
		# 			rng = c(0, 1)
		# 		} else {
		# 			fun = paste0("default_range_", type)
		# 			rng = do.call(fun, list(k = n))
		# 		}
		# 		shinyjs::disable("range")
		# 		shiny::updateSliderInput(session, "range", value = c(rng[1], rng[2]))
		# 	} else {
		# 		shinyjs::enable("range")
		# 	}
		# })

		output$range_info = shiny::renderUI({
			#if (input$type == "div") shiny::div(style="text-align:left;", shiny::tagList("middle", shiny::span(stype = "float:right;", "each side"))) else ""
			type = get_type12()#rv$type12


			if (type == "div") {
				shiny::HTML("<div style='font-size:70%; color:#111111; text-align:left;'>Middle<span style='float:right;'>Both sides</span></div>")
			} else {
				shiny::HTML("<div style='font-size:70%; color:#111111; text-align:left;'>Left<span style='float:right;'>Right</span></div>")
			}
		})


		output$show = function() {

			values = get_values()#_d()
			sort = paste0({if (values$sortRev) "-" else ""}, values$sort)


			if (is.null(values$series)) {
				tab = NULL
			} else {
				# Create a Progress object
				progress <- shiny::Progress$new()
				# Make sure it closes when we exit this reactive, even if there's an error
				on.exit(progress$close())

				progress$set(message = "Colors in progress...", value = 0)

				if (substr(values$type, 1, 3) == "biv") {
					tab = c4a_table(n = values$nbiv, m = values$mbiv, cvd.sim = values$cvd, sort = sort, columns = values$columns, type = values$type, show.scores = values$show.scores, series = values$series, range = values$range, include.na = values$na, text.col = values$textcol, text.format = values$format, verbose = FALSE)
				} else {
					tab = c4a_table(n = values$n, cvd.sim = values$cvd, sort = sort, columns = values$columns, type = values$type, show.scores = values$show.scores, series = values$series, range = values$range, include.na = values$na, text.col = values$textcol, text.format = values$format, verbose = FALSE)
				}
			}
			if (is.null(tab)) {
				kableExtra::kbl(data.frame("No palettes found. Please change the selection."), col.names = " ")
			} else {
				tab
			}
		}

		#############################
		## CBF tab
		#############################

		# output$cbfPlot = shiny::renderPlot({
		# 	pal = input$cbfPal
		# 	x = c4a_info(pal)
		# 	n_init = x$ndef
		#
		# 	pal_new = c4a(x$fullname, n = n_init)
		#
		# 	colorblindcheck::palette_plot(pal_new)
		# })

		output$cbfRGB = shiny::renderPlot({
			pal = input$cbfPal
			x = c4a_info(pal)
			n_init = x$ndef

			pal_new = c4a(x$fullname, n = n_init)

			c4a_confusion_lines(pal_new)
		})

		#############################
		## Application tab
		#############################


		output$CLplot = shiny::renderPlot({
			pal = input$appPal
			x = c4a_info(pal)
			n_init = x$ndef

			pal_new = c4a(x$fullname, n = n_init)

			c4a_CL_plot(pal_new)
		})



		#############################
		## Contrast tab
		#############################

		shiny::observeEvent(input$contrastPal, {
			pal = input$contrastPal
			x = c4a_info(pal)
			n_init = x$ndef

			pal_new = c(c4a(x$fullname, n = n_init), "#ffffff", "#000000")


			shiny::updateRadioButtons(session, inputId = "col1", choiceNames = getNames(pal_new), choiceValues = pal_new, selected = pal_new[1])
			shiny::updateRadioButtons(session, inputId = "col2", choiceNames = getNames(pal_new), choiceValues = pal_new, selected = pal_new[2])

		})

		get_cols2 = shiny::reactive({
			c(input$col1, input$col2)
		})

		output$ex_plus = shiny::renderPlot({
			cols = get_cols2()
			borders = input$borders
			lwd = input$lwd

			c4a_example_Plus_Reversed(cols[1], cols[2], orientation = "landscape", borders = borders, lwd = lwd)
		})

		output$ex = shiny::renderPlot({
			cols = get_cols2()
			borders = input$borders
			lwd = input$lwd
			if (input$chart == "Barchart") {
				c4a_example_bars(cols[1], cols[2], borders = borders, lwd = lwd)
			} else {
				c4a_example_map(cols[1], cols[2], borders = borders, lwd = lwd)
			}
		})

		output$table = shiny::renderPlot({
			pal = input$contrastPal
			x = c4a_info(pal)
			n_init = x$ndef

			pal_new = c(c4a(x$fullname, n = n_init), "#ffffff", "#000000")
			get_CRmatrix(pal_new)
		})


	}
	shiny::shinyApp(ui = ui, server = server)
}
