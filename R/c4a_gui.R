#' Graphical user interface to select palettes
#'
#' Graphical user interface to select palettes
#'
#' @export
c4a_gui = function() {
	if (requireNamespace("shiny")) {
		series = unique(.z$series)
		ui = shiny::fluidPage(

			# Application title
			shiny::titlePanel("col4all: colors for all!"),

			shiny::sidebarLayout(
				shiny::sidebarPanel(
					shiny::checkboxInput("advanced", "Expert mode", value = FALSE),
					shiny::radioButtons("type", "Type", choices = c(Categorical = "cat", Sequential = "seq", Diverging = "div"), selected = "cat"),
					shiny::sliderInput("n", "Number of colors",
									   min = 2, max = 11, value = 9),
					shiny::radioButtons("cvd", "Color vision deficiency", choices = c(None = "none", Deutan = "deutan", Protan = "protan", Tritan = "tritan"), selected = "none"),
					shiny::selectInput("sort", "Sort", choices = structure(c("name", "rank"), names = c("Name", .friendly)), selected = "rank"),
					shiny::selectizeInput("series", "Series", choices = series, selected = series, multiple = TRUE)
				),

				shiny::mainPanel(
					shiny::tableOutput("show")
				)
			)
		)
		server = function(input, output, session) {

			output$show = function() {
				shiny::req(input$n, input$cvd, input$sort, input$type, input$series)

				columns = if (input$n > 16) 12 else input$n
				c4a_show(n = input$n, cvd.sim = input$cvd, sort = input$sort, columns = columns, type = input$type, advanced.mode = input$advanced, series = input$series)
			}
			observe({
				tp = input$type
				adv = input$advanced

				ind = .indicators[[tp]]
				if (!adv) ind = NULL

				sort = shiny::isolate(input$sort)
				choi = structure(c("name", "rank", ind, .hcl), names = c("Name", .friendly, unname(.labels[c(ind, .hcl)])))

				sortNew = if (sort %in% choi) sort else "name"

				updateSelectInput(session, "sort",
								   choices  = choi,selected = sortNew)
			})
		}
		shiny::shinyApp(ui = ui, server = server)
	} else {
		message("Please install shiny")
	}
}

