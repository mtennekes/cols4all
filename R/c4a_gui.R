def_n = function(type) switch(type, cat = 7, seq = 7, div = 9, 3)



#' @rdname c4a_gui
#' @name c4a_gui
#' @export
c4a_gui = function(type = "cat", n = NA, series = c("misc", "brewer", "hcl", "tol", "viridis")) {
	if (!requireNamespace("shiny")) stop("Please install shiny")
	if (!requireNamespace("shinyjs")) stop("Please install shinyjs")
	if (!requireNamespace("kableExtra")) stop("Please install kableExtra")

	if (is.na(n)) n = def_n(type)

	z = .C4A$z

	allseries = sort(unique(z$series))
	if (series[1] == "all") {
		series = allseries
	} else {
		if (!all(series %in% allseries)) message("These series do not exist: \"", paste(setdiff(series, allseries), collapse = "\", \""), "\"")
		series = intersect(series, allseries)
	}
	if (!length(series)) {
		message("No palette series loaded. Please reload cols4all, add series with c4a_series_add, or import data with c4a_sysdata_import")
		return(invisible(NULL))
	}

	types = c(Categorical = "cat", Sequential = "seq", Diverging = "div", 'Bivariate (seq-seq)' = "bivs", 'Bivariate (cat-seq)' = "bivc",  'Bivariate (uncertainty)' = "bivu")
	series_per_type = structure(lapply(types, function(tp) {
		sort(unique(z$series[z$type == tp]))
	}), names = unname(types))
	first_series = sort(intersect(series, series_per_type[[type]]))


	shiny::addResourcePath(prefix = "imgResources", directoryPath = system.file("img", package = "cols4all"))

	ui = shiny::fluidPage(
		shinyjs::useShinyjs(),
		shiny::tags$style(shiny::HTML("div.sticky {
		  position: -webkit-sticky;
		  position: sticky;
		  top: 0;
		  z-index: 1;
		}")),

		# Application title
		#shiny::titlePanel("col4all: colors for all!"),

		shiny::titlePanel(title = "Colors for all!"),


		shiny::sidebarLayout(
			shiny::tagAppendAttributes(shiny::sidebarPanel(
				width = 3,
				shiny::fluidRow(
				shiny::column(9,
					shiny::radioButtons("type", "Type", choices = types, selected = type)
				), shiny::column(3,
					shiny::img(align = "right", alt = "test", src = "imgResources/cols4all_logo.png", height = 100)
				)),
				shiny::selectizeInput("series", "Palette Series", choices = series_per_type[[type]], selected = first_series, multiple = TRUE),
				shiny::conditionalPanel(
					condition = "input.type.substring(0, 3) != 'biv'",
					shiny::sliderInput("n", "Number of colors", min = 2, max = 36, value = n, ticks = FALSE)),
				shiny::conditionalPanel(
					condition = "input.type.substring(0, 3) == 'biv'",
					shiny::fluidRow(
						shiny::column(6,
							shiny::sliderInput("nbiv", "Number of columns", min = 3, max = 5, value = 5, ticks = FALSE)),
						shiny::column(6,
							shinyjs::disabled(shiny::sliderInput("mbiv", "Number of rows", min = 3, max = 5, value = 5, ticks = FALSE))))),
				shiny::checkboxInput("na", shiny::strong("Color for missing values"), value = FALSE),
				shiny::conditionalPanel(
					condition = "input.type == 'seq' || input.type == 'div'",
					shiny::fluidRow(
						shiny::column(4,
							#shiny::br(),
							shiny::radioButtons("auto_range", label = "Range", choices = c("Maximum", "Automatic", "Manual"), selected = "Maximum")),
							shiny::column(8,
								shinyjs::disabled(shiny::div(style = "font-size:0;margin-bottom:-20px", shiny::sliderInput("range", "",
												   min = 0, max = 1, value = c(0,1), step = .05))),
								shiny::uiOutput("range_info")))),
				shiny::radioButtons("cvd", "Color vision", choices = c(Normal = "none", 'Deutan (red-green blind)' = "deutan", 'Protan (also red-green blind)' = "protan", 'Tritan (blue-yellow)' = "tritan"), selected = "none"),
				shiny::checkboxInput("advanced", shiny::strong("Show underlying scores"), value = FALSE),

				shiny::fluidRow(
					shiny::column(6,
						shiny::selectInput("sort", "Sort", choices = structure(c("name", "rank"), names = c("Name", .C4A$labels["cbfriendly"])), selected = "rank")),
					shiny::column(6,
						shiny::br(),
						shiny::checkboxInput("sortRev", "Reverse", value = FALSE))),
				shiny::fluidRow(
					shiny::column(6,
						shiny::radioButtons("format", "Text format", choices = c("Hex" = "hex", "RGB" = "RGB", "HCL" = "HCL"), inline = TRUE)),
					shiny::column(6,
						shiny::selectInput("textcol", "Text color", choices = c("Hide text" = "same", Black = "#000000", White = "#FFFFFF"))))
			), class = "sticky"),

			shiny::mainPanel(
				shiny::tableOutput("show")
			)
		)
	)
	server = function(input, output, session) {


		rv <- reactiveValues(selected_series = first_series,
							 current_type = type)

		shiny::observeEvent(get_cols(), {
			cols = get_cols()
			sortNew = if (input$sort %in% cols) input$sort else "name"
			shiny::updateSelectInput(session, "sort", choices  = cols,selected = sortNew)
		})

		get_values = shiny::reactive({
			list(n = input$n,
				 nbiv = input$nbiv,
				 mbiv = input$mbiv,
				 type = input$type,
				 cvd = input$cvd,
				 sort = input$sort,
				 sortRev = input$sortRev,
				 series = input$series,
				 show.scores = input$advanced,
				 columns = if (input$n > 16) 12 else input$n,
				 na = input$na,
				 range = input$range,
				 textcol = input$textcol,
				 format = input$format)
		})
		get_values_d = shiny::debounce(get_values, 300)

		get_cols = shiny::reactive({
			res = table_columns(input$type, input$advanced)
			structure(c("name", res$qn), names = c("Name", res$ql))
		})


		shiny::observeEvent(input$type, {
			type = input$type
			n = def_n(type)

			choices = series_per_type[[type]]
			not_selected = setdiff(series_per_type[[rv$current_type]], input$series)
			rv$selected_series = union(setdiff(rv$selected_series, not_selected), input$series)
			rv$current_type = type

			selected = intersect(choices, rv$selected_series)

			shiny::updateSelectizeInput(session, "series", choices = choices, selected = selected)
			if (type == "cat") {
				shiny::updateSliderInput(session, "n", min = 2, max = 36, value = n)
			} else if (type == "seq") {
				shiny::updateSliderInput(session, "n", min = 3, max = 11,  value = n)
			} else if (type == "div") {
				shiny::updateSliderInput(session, "n", min = 3, max = 13,  value = n)
			}
		})

		shiny::observeEvent(input$type, {
			type = input$type
			if (type == "bivs") {
				shinyjs::enable("nbiv")
				shinyjs::disable("mbiv")
				shiny::updateSliderInput(session, "nbiv", value = 5)
				shiny::updateSliderInput(session, "mbiv", value = 5)
			} else if (type == "bivc") {
				shinyjs::enable("mbiv")
				shinyjs::disable("nbiv")
				shiny::updateSliderInput(session, "nbiv", value = 3)
				shiny::updateSliderInput(session, "mbiv", value = 5)
			} else  {
				shinyjs::enable("nbiv")
				shinyjs::enable("mbiv")
				shiny::updateSliderInput(session, "nbiv", value = 5)
				shiny::updateSliderInput(session, "mbiv", value = 5)
			}
		})

		shiny::observeEvent(input$nbiv, {
			nbiv = input$nbiv
			type = input$type
			if (type == "bivs") {
				shiny::updateSliderInput(session, "mbiv", value = nbiv)
			}
		})

		shiny::observe({
			n = input$n
			ac = input$auto_range
			type = input$type

			if (type == "cat") return(NULL)
			if (ac != "Manual") {
				shiny::freezeReactiveValue(input, "range")
				shinyjs::disable("range")
				if (ac == "Maximum") {
					rng = c(0, 1)
				} else {
					fun = paste0("default_range_", type)
					rng = do.call(fun, list(k = n))
				}
				shinyjs::disable("range")
				shiny::updateSliderInput(session, "range", value = c(rng[1], rng[2]))
			} else {
				shinyjs::enable("range")
			}
		})

		output$range_info = shiny::renderUI({
			#if (input$type == "div") shiny::div(style="text-align:left;", shiny::tagList("middle", shiny::span(stype = "float:right;", "each side"))) else ""
			if (input$type == "div") {
				shiny::HTML("<div style='font-size:70%; color:#111111; text-align:left;'>Middle<span style='float:right;'>Both sides</span></div>")
			} else {
				shiny::HTML("<div style='font-size:70%; color:#111111; text-align:left;'>Left<span style='float:right;'>Right</span></div>")
			}
		})


		output$show = function() {
			shiny::req(get_values_d())
			values = get_values_d()
			sort = paste0({if (values$sortRev) "-" else ""}, values$sort)
			if (values$type %in% c("bivs", "bivc", "bivu")) {
				tab = c4a_table(n = values$nbiv, m = values$mbiv, cvd.sim = values$cvd, sort = sort, columns = values$columns, type = values$type, show.scores = values$show.scores, series = values$series, range = values$range, include.na = values$na, text.col = values$textcol, text.format = values$format, verbose = FALSE)
			} else {
				tab = c4a_table(n = values$n, cvd.sim = values$cvd, sort = sort, columns = values$columns, type = values$type, show.scores = values$show.scores, series = values$series, range = values$range, include.na = values$na, text.col = values$textcol, text.format = values$format, verbose = FALSE)
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
