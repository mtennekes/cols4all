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
		if (!all(series %in% allseries)) stop("These series do not exist: \"", paste(setdiff(series, allseries), collapse = "\", \""), "\"")
	}

	ui = shiny::fluidPage(
		#shinyjs::useShinyjs(),

		# Application title
		shiny::titlePanel("col4all: colors for all!"),

		shiny::sidebarLayout(
			shiny::sidebarPanel(
				width = 3,
				shiny::radioButtons("type", "Type", choices = c(Categorical = "cat", Sequential = "seq", Diverging = "div"), selected = type), #, Cyclic = "cyc", Bivariate = "biv", Tree = "tree"), selected = "cat"),
				shiny::sliderInput("n", "Number of colors",
								   min = 2, max = 36, value = n),
				shiny::checkboxInput("na", shiny::strong("Color for missing values"), value = FALSE),
				shiny::conditionalPanel(
					condition = "input.type != 'cat'",
					shiny::strong("Contrast range"),
					shiny::sliderInput("contrast", "",
									   min = 0, max = 1, value = c(0,1), step = .01),
					shiny::checkboxInput("auto_contrast", label = "Automatic (based on number of colors)", value = FALSE)),
				shiny::selectizeInput("series", "Palette Series", choices = allseries, selected = series, multiple = TRUE),
				shiny::radioButtons("cvd", "Color vision", choices = c(Normal = "none", 'Deutan (red-green blind)' = "deutan", 'Protan (also red-green blind)' = "protan", 'Tritan (blue-yellow)' = "tritan"), selected = "none"),
				shiny::selectInput("sort", "Sort", choices = structure(c("name", "rank"), names = c("Name", .C4A$labels["cbfriendly"])), selected = "rank"),
				shiny::selectInput("textcol", "Text color", choices = c("No text" = "same", Black = "#000000", White = "#FFFFFF")),
				shiny::checkboxInput("advanced", "Show underlying scores", value = FALSE)
			),

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
				 series = input$series,
				 show.scores = input$advanced,
				 columns = if (input$n > 16) 12 else input$n,
				 na = input$na,
				 contrast = input$contrast,
				 textcol = input$textcol)
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
			} else {
				shiny::updateSliderInput(session, "n", min = 3, max = 15,  value = max(min(n, 15), 3))
			}
		})

		shiny::observe({
			n = input$n
			ac = input$auto_contrast
			type = input$type

			if (type == "cat") return(NULL)
			fun = paste0("default_contrast_", type)
			rng = do.call(fun, list(k = n))
			if (ac) {
				shiny::freezeReactiveValue(input, "contrast")
				shiny::updateSliderInput(session, "contrast", value = c(rng[1], rng[2]))
			}
		})

		output$show = function() {
			shiny::req(get_values_d())
			values = get_values_d()
			c4a_table(n = values$n, cvd.sim = values$cvd, sort = values$sort, columns = values$columns, type = values$type, show.scores = values$show.scores, series = values$series, contrast = values$contrast, include.na = values$na, text.col = values$textcol)
		}
	}
	shiny::shinyApp(ui = ui, server = server)
}

