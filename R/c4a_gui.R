#' Graphical user interface to select palettes
#'
#' Graphical user interface to select palettes
#'
#' @export
c4a_gui = function() {
	if (requireNamespace("shiny")) {
		ui = shiny::fluidPage(

			# Application title
			shiny::titlePanel("col4all: colors for all!"),

			shiny::sidebarLayout(
				shiny::sidebarPanel(
					shiny::radioButtons("type", "Type", choices = c(Categorical = "cat", Sequential = "seq", Diverging = "div"), selected = "cat"),
					shiny::sliderInput("n", "Number of colors",
									   min = 2, max = 36, value = 8),
					shiny::radioButtons("cvd", "Color vision deficiency", choices = c("none", "deutan", "protan", "tritan"), selected = "none"),
					shiny::radioButtons("sort", "Sort", choices = c("name", "score"), selected = "name")
				),

				shiny::mainPanel(
					shiny::tableOutput("show")
				)
			)
		)
		server = function(input, output) {

			output$show = function() {
				shiny::req(input$n, input$cvd, input$sort, input$type)

				columns = if (input$n > 16) 12 else input$n

				c4a_show(n = input$n, cvd.sim = input$cvd, order.by.score = (input$sort == "score"), columns = columns, type = input$type)
			}
		}
		shiny::shinyApp(ui = ui, server = server)
	} else {
		message("Please install shiny")
	}
}

