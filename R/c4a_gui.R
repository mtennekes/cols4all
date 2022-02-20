#' Graphical user interface to select palettes
#'
#' Graphical user interface to select palettes. See \code{\link{c4a_show}} for description of the color palette table. Input controls should be self explanatory (if not, let us know).
#'
#' @example ./examples/c4a_gui.R
#' @export
c4a_gui = function() {
	if (requireNamespace("shiny")) {
		z = get(".z", envir = .C4A_CACHE)

		series = unique(z$series)
		ui = shiny::fluidPage(
			shinyjs::useShinyjs(),

			# Application title
			shiny::titlePanel("col4all: colors for all!"),

			shiny::sidebarLayout(
				shiny::sidebarPanel(
					width = 3,
					shiny::checkboxInput("advanced", "Show underlying scores", value = FALSE),
					shiny::radioButtons("type", "Type", choices = c(Categorical = "cat", Sequential = "seq", Diverging = "div")), #, Cyclic = "cyc", Bivariate = "biv", Tree = "tree"), selected = "cat"),
					shiny::sliderInput("n", "Number of colors",
									   min = 2, max = 36, value = 7),
					shiny::conditionalPanel(
						condition = "input.type != 'cat'",
						shiny::strong("Contrast range"),
						shiny::sliderInput("contrast", "",
										   min = 0, max = 1, value = c(0,1), step = .01),
						shiny::checkboxInput("auto_contrast", label = "Automatic (based on number of colors)", value = FALSE)),
					shiny::checkboxInput("na", "Include color for NA", value = FALSE),
					shiny::radioButtons("cvd", "Color vision", choices = c(Normal = "none", 'Red-Green blind ("deutan")' = "deutan", 'Red-Green blind ("protan")' = "protan", 'Blue-Yellow blind ("tritan")' = "tritan"), selected = "none"),
					shiny::selectizeInput("series", "Palette Series", choices = series, selected = series, multiple = TRUE),
					shiny::selectInput("sort", "Sort", choices = structure(c("name", "rank"), names = c("Name", .friendly)), selected = "rank"),
				),

				shiny::mainPanel(
					shiny::tableOutput("show")
				)
			)
		)
		server = function(input, output, session) {
			observeEvent(get_cols(), {
				cols = get_cols()
				sortNew = if (input$sort %in% cols) input$sort else "name"

				updateSelectInput(session, "sort",
								  choices  = cols,selected = sortNew)
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
					 contrast = input$contrast)
			})
			get_values_d = get_values %>% debounce(300)

			get_cols = shiny::reactive({
				res = table_columns(input$type, input$advanced)
				structure(c("name", res$qn), names = c("Name", res$ql))
			})



			observe({
				n = input$n
				ac = input$auto_contrast
				type = input$type

				if (type == "cat") return(NULL)
				fun = paste0("default_contrast_", type)
				rng = do.call(fun, list(k = n))
				if (ac) {
					freezeReactiveValue(input, "contrast")
					shiny::updateSliderInput(session, "contrast", value = c(rng[1], rng[2]))
				}
			})


			output$show = function() {
				shiny::req(get_values_d())
				values = get_values_d()
				c4a_show(n = values$n, cvd.sim = values$cvd, sort = values$sort, columns = values$columns, type = values$type, show.scores = values$show.scores, series = values$series, contrast = values$contrast, include.na = values$na)
			}


		}
		shiny::shinyApp(ui = ui, server = server)
	} else {
		message("Please install shiny")
	}
}

