c4a_analyse = function(palette) {
	x = c4a_info(palette)

	n_init = x$ndef
	pal_init = c(c4a(x$fullname, n = n_init), "#ffffff", "#000000")


	getNames = function(p) {
		lapply(p, function(pi) {
			shiny::HTML(paste0("<div style='font-size:2em;line-height:0.5em;height:0.5em;color:", pi, "'>&#9632;</div>"))
		})
	}

	ui = shiny::fluidPage(
		#shiny::includeCSS(system.file("www/misc.css", package = "cols4all")),
		shiny::tabsetPanel(
			shiny::tabPanel("Contrast",
							shiny::wellPanel(
								shiny::fluidRow(
									shiny::column(width = 6,
												  shiny::fluidRow(
												  	shiny::column(width = 2,
												  				  shiny::radioButtons("col1", "Color 1",
					 		   		   			 choiceNames = getNames(pal_init),
					 		   		   			 choiceValues = pal_init, selected = pal_init[1])),
												  	shiny::column(width = 2,
												  				  shiny::radioButtons("col2", "Color 2",
					 		   		   			 choiceNames = getNames(pal_init),
					 		   		   			 choiceValues = pal_init, selected = pal_init[2])),
												  	shiny::column(8,
												  				  shiny::plotOutput("table", height = "250px", width = "250px"),
												  				  shiny::markdown("[Contrast Ratio](http://colorspace.r-forge.r-project.org/reference/contrast_ratio.html). Values close to one (bold) indicate low contrast; use borders to separate them."))),
					 		   shiny::fluidRow(
					 		   	shiny::radioButtons("chart", "Example chart", c("Barchart", "Choropleth"), "Choropleth", inline = TRUE)
					 		   ),
					 		   shiny::fluidRow(
					 		   	shiny::column(4, shiny::selectInput("borders", "Borders", choices = c("no", "black", "white"), selected = "no")),
					 		   	shiny::column(8,
					 		   				  shiny::conditionalPanel(
					 		   				  	condition = "input.borders != 'no'",
					 		   				  	shiny::sliderInput("lwd", "Line Width", min = 0.5, max = 3, step = 0.5, value = 1)),
					 		   	)),
					 		   shiny::fluidRow(
					 		   	shiny::plotOutput("ex", height = "250px", width = "500px"),
					 		   )),
									shiny::column(6,
												  shiny::plotOutput("ex_plus", height = "500px", width = "391px"),
												  shiny::markdown("_Plus Reversed_ by Richard Anuszkiewicz (1960)"))
					 ))),
			shiny::tabPanel("Color-blindness", shiny::p("test1")),
			shiny::tabPanel("Harmony", shiny::p("test1"))
		)
	)

	server = function(input, output, session) {
		output$ex_plus = shiny::renderPlot({
			c4a_example_Plus_Reversed(input$col1, input$col2)
		})
		output$ex = shiny::renderPlot({
			border = if (input$borders == "no") NA else input$borders
			if (input$chart == "Barchart") {
				c4a_example_bars(input$col1, input$col2, border, lwd = input$lwd)
			} else {
				c4a_example_map(input$col1, input$col2, border, lwd = input$lwd)
			}
		})

		output$table = shiny::renderPlot({
			get_CRmatrix(pal_init)
		})

	}

	shiny::shinyApp(ui, server)


}


