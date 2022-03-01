#' @rdname c4a_gui
#' @name c4a_gui
#' @export
c4a_gui = function(type = "cat", n = 9, series = "all") {
	if (!requireNamespace("shiny")) stop("Please install shiny")
	if (!requireNamespace("kableExtra")) stop("Please install kableExtra")

	z = .C4A$z

	allseries = unique(z$series)
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

	ui = shiny::fluidPage(
		tags$style(HTML("div.sticky {
		  position: -webkit-sticky;
		  position: sticky;
		  top: 0;
		  z-index: 1;
		}")),
		shinyjs::useShinyjs(),

		# Application title
		shiny::titlePanel("col4all: colors for all!"),

		shiny::sidebarLayout(
			tagAppendAttributes(shiny::sidebarPanel(
				width = 3,
				shiny::radioButtons("type", "Type", choices = c(Categorical = "cat", Sequential = "seq", Diverging = "div"), selected = type), #, Cyclic = "cyc", Bivariate = "biv", Tree = "tree"), selected = "cat"),
				shiny::sliderInput("n", "Number of colors",
								   min = 2, max = 36, value = n, ticks = FALSE),
				shiny::checkboxInput("na", shiny::strong("Color for missing values"), value = FALSE),
				shiny::conditionalPanel(
					condition = "input.type != 'cat'",
					shiny::fluidRow(
						shiny::column(4,
							shiny::radioButtons("auto_contrast", label = "Contrast range", choices = c("Maximum", "Automatic", "Manual"), selected = "Maximum")),
							shiny::column(8,
								shiny::br(),
								shinyjs::disabled(shiny::sliderInput("contrast", "",
												   min = 0, max = 1, value = c(0,1), step = .05))))),
				shiny::selectizeInput("series", "Palette Series", choices = allseries, selected = series, multiple = TRUE),
				shiny::radioButtons("cvd", "Color vision", choices = c(Normal = "none", 'Deutan (red-green blind)' = "deutan", 'Protan (also red-green blind)' = "protan", 'Tritan (blue-yellow)' = "tritan"), selected = "none"),

				shiny::fluidRow(
					shiny::column(6,
						shiny::selectInput("sort", "Sort", choices = structure(c("name", "rank"), names = c("Name", .C4A$labels["cbfriendly"])), selected = "rank")),
					shiny::column(6,
						shiny::br(),
						shiny::checkboxInput("sortRev", "Reverse", value = FALSE))),

				shiny::selectInput("textcol", "Text color", choices = c("Hide text" = "same", Black = "#000000", White = "#FFFFFF")),
				shiny::radioButtons("format", "Text format", choices = c("Hex" = "hex", "RGB" = "RGB", "HCL" = "HCL"), inline = TRUE),
				shiny::checkboxInput("advanced", "Show underlying scores", value = FALSE)
			), class = "sticky"),

			shiny::mainPanel(
				shiny::tableOutput("show")
			)
		)
	)
	server = function(input, output, session) {
		shiny::observeEvent(get_cols(), {
			cols = get_cols()
			sortNew = if (input$sort %in% cols) input$sort else "name"
			shiny::updateSelectInput(session, "sort", choices  = cols,selected = sortNew)
		})

		get_values = shiny::reactive({
			list(n = input$n,
				 type = input$type,
				 cvd = input$cvd,
				 sort = input$sort,
				 sortRev = input$sortRev,
				 series = input$series,
				 show.scores = input$advanced,
				 columns = if (input$n > 16) 12 else input$n,
				 na = input$na,
				 contrast = input$contrast,
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
			n = input$n
			if (type == "cat") {
				shiny::updateSliderInput(session, "n", min = 2, max = 36, value = n)
			} else if (type == "seq") {
				shiny::updateSliderInput(session, "n", min = 3, max = 11,  value = max(min(n, 11), 3))
			} else {
				shiny::updateSliderInput(session, "n", min = 3, max = 13,  value = max(min(n, 13), 3))
			}
		})

		shiny::observe({
			n = input$n
			ac = input$auto_contrast
			type = input$type

			if (type == "cat") return(NULL)
			if (ac != "Manual") {
				shiny::freezeReactiveValue(input, "contrast")
				shinyjs::disable("contrast")
				if (ac == "Maximum") {
					rng = c(0, 1)
				} else {
					fun = paste0("default_contrast_", type)
					rng = do.call(fun, list(k = n))
				}
				shiny::updateSliderInput(session, "contrast", value = c(rng[1], rng[2]))
			} else {
				shinyjs::enable("contrast")
			}
		})

		output$show = function() {
			shiny::req(get_values_d())
			values = get_values_d()
			sort = paste0({if (values$sortRev) "-" else ""}, values$sort)
			c4a_table(n = values$n, cvd.sim = values$cvd, sort = sort, columns = values$columns, type = values$type, show.scores = values$show.scores, series = values$series, contrast = values$contrast, include.na = values$na, text.col = values$textcol, text.format = values$format)
		}
	}
	shiny::shinyApp(ui = ui, server = server)
}

