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
					shiny::checkboxInput("advanced", "Show underlying data", value = FALSE),
					shiny::selectInput("sort", "Sort", choices = structure(c("name", "rank"), names = c("Name", .friendly)), selected = "rank"),
					shiny::selectizeInput("series", "Series", choices = c("hcl", "tol", "viridis", "brewer", "carto", "scico", "lb", "other"), selected = c("hcl", "tol", "viridis", "brewer", "carto", "scico", "lb", "other"), multiple = TRUE)
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
				choi = structure(c("name", "rank", ind, "Crel", "Hwidth"), names = c("Name", .friendly, unname(.labels[ind]), "Chroma (rel. max)", "Hue width"))

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

