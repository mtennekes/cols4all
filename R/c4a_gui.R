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
									   min = 2, max = 36, value = 7),
					shiny::radioButtons("cvd", "Color vision deficiency", choices = c(None = "none", Deutan = "deutan", Protan = "protan", Tritan = "tritan"), selected = "none"),
					shiny::radioButtons("sort", "Sort", choiceValues = c("name", "rank"),
										choiceNames = c("Name", .friendly), selected = "name"),
					shiny::checkboxInput("advanced", "Show scores", value = FALSE)
				),

				shiny::mainPanel(
					shiny::tableOutput("show")
				)
			)
		)
		server = function(input, output, session) {

			output$show = function() {
				shiny::req(input$n, input$cvd, input$sort, input$type)

				columns = if (input$n > 16) 12 else input$n
				c4a_show(n = input$n, cvd.sim = input$cvd, sort = input$sort, columns = columns, type = input$type, advanced.mode = input$advanced)
			}
			observe({
				tp = input$type
				ind = .indicators[[tp]]

				updateRadioButtons(session, "sort",
								   choiceValues = c("name", "rank", ind, "Crel", "Hwidth"),
								   choiceNames = c("Name", .friendly, unname(.labels[ind]), "Chroma (rel. max)", "Hue width")
				)
			})
		}
		shiny::shinyApp(ui = ui, server = server)
	} else {
		message("Please install shiny")
	}
}

