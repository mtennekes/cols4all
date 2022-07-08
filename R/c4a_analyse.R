c4a_analyse = function(palette) {
	x = c4a_info(palette)

	n_init = x$ndef
	pal_init = c(c4a(x$fullname, n = n_init), "#ffffff", "#000000")


	getNames = function(p) {
		lapply(p, function(pi) {
			HTML(paste0("<div style='font-size:2em;line-height:0.5em;height:0.5em;color:", pi, "'>&#9632;</div>"))
		})
	}

	options(mar = c(0,0,0,0))

	ui = fluidPage(
		#shiny::includeCSS(system.file("www/misc.css", package = "cols4all")),
		tabsetPanel(
			tabPanel("Contrast",
					 sidebarLayout(
					 	sidebarPanel(
					 		width = 3,
						 	fluidRow(
						 		column(width = 6,
						 			   radioButtons("col1", "Color 1",
						 			   			 choiceNames = getNames(pal_init),
						 			   			 choiceValues = pal_init)),
						 		column(width = 6,
						 			   radioButtons("col2", "Color 2",
						 			   			 choiceNames = getNames(pal_init),
						 			   			 choiceValues = pal_init))
						 	),
						 	fluidRow(
						 		selectInput("borders", "Borders", choices = c("no", "black", "white"), selected = "no"),
						 		conditionalPanel(
						 			condition = "input.borders != 'no'",
						 			sliderInput("lwd", "Line Width", min = 0.5, max = 3, step = 0.5, value = 1))
						 	)
						 ),
					 mainPanel(
					 	width = 9,
					 	   fluidRow(
					 	   	column(6, plotOutput("table", height = "300px", width = "300px")),
					 	   	column(6, plotOutput("ex_plus", height = "600px", width = "469px"))
					 	   ),
					 	   fluidRow(
					 	   	column(4, plotOutput("ex_bars", height = "300px", width = "300px")),
					 	   	column(8, plotOutput("ex_map", height = "300px", width = "600px"))
					 	   )
					 ))),
			tabPanel("Color-blindness", shiny::p("test1")),
			tabPanel("Harmony", shiny::p("test1"))
		)
	)

	server = function(input, output, session) {
		output$ex_plus = renderPlot({
			c4a_example_Plus_Reversed(input$col1, input$col2)
		})
		output$ex_map = renderPlot({
			border = if (input$borders == "no") NA else input$borders
			c4a_example_map(input$col1, input$col2, border, lwd = input$lwd)
		})
		output$ex_bars = renderPlot({
			border = if (input$borders == "no") NA else input$borders
			c4a_example_bars(input$col1, input$col2, border, lwd = input$lwd)
		})

		output$table = renderPlot({
			get_CRmatrix(pal_init)
		})

	}

	shinyApp(ui, server)


}

