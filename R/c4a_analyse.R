c4a_analyse = function(palette) {
	x = c4a_info(palette)

	n_init = x$ndef
	pal_init = c(c4a(x$fullname, n = n_init), "#ffffff", "#000000")


	getNames = function(p) {
		lapply(p, function(pi) {
			HTML(paste0("<div style='font-size:2em;line-height:0.5em;height:0.5em;color:", pi, "'>&#9632;</div>"))
		})
	}

	ui = fluidPage(
		tabsetPanel(
			tabPanel("Contrast",
				fluidRow(
					 column(width = 5,
					 	plotOutput("table", height = "600px", click = "plot_click"),
					 	fluidRow(
					 		column(width = 3,
					 			   radioButtons("col1", "Color 1",
					 			   			 choiceNames = getNames(pal_init),
					 			   			 choiceValues = pal_init)),
					 		column(width = 3,
					 			   radioButtons("col2", "Color 2",
					 			   			 choiceNames = getNames(pal_init),
					 			   			 choiceValues = pal_init))
					 	),
					 ),
					 column(width = 7,
					 	plotOutput("plot", height = "600px")
					 ))),
			tabPanel("Color-blindness", shiny::p("test1")),
			tabPanel("Harmony", shiny::p("test1"))
		)
	)

	server = function(input, output, session) {
		output$plot = renderPlot({
			plus_rev(input$col1, input$col2)
		})
		output$table = renderPlot({
			get_CRmatrix(pal_init)
		})
	}

	shinyApp(ui, server)


}

