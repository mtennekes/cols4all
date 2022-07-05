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


	z = .C4A$z

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


	# series_per_type = structure(lapply(types, function(tp) {
	# 	sort(unique(z$series[z$type == tp]))
	# }), names = unname(types))
	# first_series = sort(intersect(series, series_per_type[[type]]))


	shiny::addResourcePath(prefix = "imgResources", directoryPath = system.file("img", package = "cols4all"))




	ui = shiny::fluidPage(
		shinyjs::useShinyjs(),
		shiny::tags$head(shiny::includeCSS(system.file("www/light.css", package = "cols4all"))),
		shiny::tags$head(shiny::includeCSS(system.file("www/dark.css", package = "cols4all"))),

		shiny::titlePanel(title = "Colors for all!"),

		shiny::column(
			width = 4,
			shiny::wellPanel(
				shiny::radioButtons("type1", "Palette Type", choices = types1, selected = type1),
				shiny::conditionalPanel(
					condition = "input.type1 == 'biv'",
					shiny::selectizeInput("type2", "Subtype", choices = types2[["biv"]], selected = type2)),
				shiny::div(style = "margin-bottom: 5px;", shiny::strong("Palette series")),
				shiny::div(class = 'multicol',
						   shiny::checkboxGroupInput("series", label = "", choices = allseries, selected = series, inline = FALSE)),
				shiny::actionButton("overview", label = "Overview"),

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
				shiny::radioButtons("format", "Text format", choices = c("Hex" = "hex", "RGB" = "RGB", "HCL" = "HCL"), inline = FALSE),
				shiny::selectizeInput("textcol", "Text color", choices = c("Hide text" = "same", Black = "#000000", White = "#FFFFFF", Automatic = "auto")),
				shiny::radioButtons("cvd", "Color vision", choices = c(Normal = "none", 'Deutan (red-green blind)' = "deutan", 'Protan (also red-green blind)' = "protan", 'Tritan (blue-yellow)' = "tritan"), selected = "none"),
				shiny::selectizeInput("sort", "Sort", choices = structure(c("name", "rank"), names = c("Name", .C4A$labels["cbfriendly"])), selected = "name"),
				shiny::checkboxInput("sortRev", "Reverse sorting", value = FALSE),
				shiny::checkboxInput("advanced", "Show underlying scores", value = FALSE),
				shiny::checkboxInput("dark", "Dark mode", value = FALSE))),
			#), class = "sticky"),

			#shiny::mainPanel(
			shiny::column(
				width = 8,
				shiny::div(style = 'overflow-y:scroll; height:90vh; min-width:40vw; margin-left: 20px',
						   shiny::tableOutput("show"))
		)
	)
	server = function(input, output, session) {
		series_d = shiny::debounce(shiny::reactive(input$series), 300)

		get_type12 = shiny::reactive({
			type1 = input$type1
			type12 = if (type1 %in% names(types2)) {
				input$type2
			} else {
				type1
			}
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
			type = get_type12()
			res = table_columns(type, input$advanced)
			structure(c("name", res$qn), names = c("Name", res$ql))
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

	}
	shiny::shinyApp(ui = ui, server = server)
}
