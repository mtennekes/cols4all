#' Graphical user interface to select palettes
#'
#' Graphical user interface to select palettes
#'
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
					shiny::checkboxInput("advanced", "Expert mode", value = FALSE),
					shiny::radioButtons("type", "Type", choices = c(Categorical = "cat", Sequential = "seq", Diverging = "div")), #, Cyclic = "cyc", Bivariate = "biv", Tree = "tree"), selected = "cat"),
					shiny::sliderInput("n", "Number of colors",
									   min = 2, max = 36, value = 7),
					shiny::checkboxInput("na", "Include NA", value = FALSE),
					shiny::radioButtons("cvd", "Color vision deficiency", choices = c(None = "none", Deutan = "deutan", Protan = "protan", Tritan = "tritan"), selected = "none"),
					shiny::selectInput("sort", "Sort", choices = structure(c("name", "rank"), names = c("Name", .friendly)), selected = "rank"),
					shiny::selectizeInput("series", "Series", choices = series, selected = series, multiple = TRUE),
					shiny::conditionalPanel(
						condition = "input.type != 'cat'",
						shiny::strong("Contrast range"),
						shiny::checkboxInput("auto_contrast", label = "Automatic", value = TRUE),
						shiny::div(
							style = "font-size:0;margin-top:-20px",
							shiny::sliderInput("contrast", "",
											   min = 0, max = 1, value = c(0,1), step = .01)
						))
					#shiny::sliderInput("contrast", "Contrast", min = 0, max = 1, step = 0.05, value = c(0,1))
				),

				shiny::mainPanel(
					shiny::tableOutput("show")
				)
			)
		)
		server = function(input, output, session) {

			output$show = function() {
				d = plotData()
				c4a_show(n = d$n, cvd.sim = d$cvd, sort = d$sort, columns = d$columns, type = d$type, advanced.mode = d$advanced, series = d$series, contrast = d$contrast, include.na = d$na)
			}


			plotData = shiny::reactive({
				x = list(n = input$n,
						 type = input$type,
						 cvd = input$cvd,
						 sort = input$sort,
						 series = input$series,
						 advanced = input$advanced,
						 columns = if (input$n > 16) 12 else input$n,
						 na = input$na,
						 contrast = NULL)
				if (input$type != "cat") x$contrast = input$contrast
				x
			})

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
					shiny::updateSliderInput(session, "contrast", value = c(rng[1], rng[2]))
				}
			})


			# shiny::observe({
			# 	input$n
			# 	if (input$auto_contrast && input$type != "cat") {
			# 		shinyjs::delay(0, {
			# 			shinyjs::toggleState("contrast", !input$auto_contrast)
			# 		})
			# 	}
			# })

			observe({
				tp = input$type
				adv = input$advanced
				shiny::isolate({
					sort = shiny::isolate(input$sort)

					choi = get_cols()
					sortNew = if (sort %in% choi) sort else "name"

					updateSelectInput(session, "sort",
									  choices  = choi,selected = sortNew)
				})
			})
		}
		shiny::shinyApp(ui = ui, server = server)
	} else {
		message("Please install shiny")
	}
}

