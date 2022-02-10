#' Graphical user interface to select palettes
#'
#' Graphical user interface to select palettes
#'
#' @export
c4a_gui = function() {
	if (requireNamespace("shiny")) {
		ui = shiny::fluidPage(

			# Application title
			shiny::titlePanel("c4a: col4all: colors for all"),

			shiny::sidebarLayout(
				shiny::sidebarPanel(
					shiny::sliderInput("n", "number of colors",
									   min = 1, max = 36, value = 8),
					shiny::radioButtons("cvd", "Color vision deficiency", choices = c("none", "deutan", "protan", "tritan"), selected = "none")
				),

				shiny::mainPanel(
					shiny::tableOutput("show")
				)
			)
		)
		server = function(input, output) {

			output$show = function() {
				shiny::req(input$n, input$cvd)
				c4a_show(n = input$n, cvd.sim = input$cvd)
			}
		}
		shiny::shinyApp(ui = ui, server = server)
	} else {
		message("Please install shiny")
	}
}

