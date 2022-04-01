def_n = function(npref = NA, type, series, tab_nmin, tab_nmax) {

	nmin = suppressWarnings(min(tab_nmin[series, type], na.rm = TRUE))
	nmax = suppressWarnings(max(tab_nmax[series, type], na.rm = TRUE))

	if (is.na(npref)) npref = switch(type, cat = 7, seq = 7, div = 9, 3)
	if (is.infinite(nmax)) nmax = 15
	if (nmin < 2) nmin = 2

	n = if (is.infinite(nmin)) {
		npref # should not happen or else throw a message elsewhere
	} else if (nmin < npref && nmax > npref) {
		npref
	} else if (is.infinite(nmax)) {
		nmin
	} else {
		nmax
	}
	list(n = n, nmin = nmin, nmax = nmax)
}



#' @rdname c4a_gui
#' @name c4a_gui
#' @export
c4a_gui = function(type = "cat", n = NA, series = c("misc", "brewer", "hcl", "tol", "viridis", "c4a")) {
	if (!requireNamespace("shiny")) stop("Please install shiny")
	if (!requireNamespace("shinyjs")) stop("Please install shinyjs")
	if (!requireNamespace("kableExtra")) stop("Please install kableExtra")


	z = .C4A$z

	tps = c("cat", "seq", "div", "bivs", "bivc", "bivu")

	tab_nmin = tapply(z$nmin, INDEX = list(z$series, factor(z$type, levels = tps)), FUN = min)
	tab_nmax = tapply(z$nmax, INDEX = list(z$series, factor(z$type, levels = tps)), FUN = max)
	tab_k = as.data.frame(tapply(z$nmin, INDEX = list(z$series, factor(z$type, levels = tps)), FUN = length))
	tab_k$series = rownames(tab_k)
	tab_k = tab_k[, c("series", tps)]






	allseries = sort(unique(z$series))
	if (series[1] == "all") {
		series = allseries
	} else {
		if (!all(series %in% allseries)) message("These series do not exist: \"", paste(setdiff(series, allseries), collapse = "\", \""), "\"")
		series = intersect(series, allseries)
	}
	if (!length(series)) {
		message("No palette series to show. Either restart c4a_gui with different parameters, add series with c4a_palettes_add, or import data with c4a_sysdata_import")
		return(invisible(NULL))
	}

	ns = def_n(npref = n, type, series, tab_nmin, tab_nmax)

	types = .C4A$types
	series_per_type = structure(lapply(types, function(tp) {
		sort(unique(z$series[z$type == tp]))
	}), names = unname(types))
	first_series = sort(intersect(series, series_per_type[[type]]))


	shiny::addResourcePath(prefix = "imgResources", directoryPath = system.file("img", package = "cols4all"))

	ui = shiny::fluidPage(
		shinyjs::useShinyjs(),
		shiny::tags$script(shiny::HTML("
	      Shiny.addCustomMessageHandler('background-color', function(x) {
	        document.body.style.backgroundColor = x.bg;
	        document.body.style.color = x.text;
	      });
	    ")),
		# function setProp(cls, prop, value) {
		# 	var elements = document.getElementsByClassName(cls);
		# 	for(var i = 0; i < elements.length; i++) {
		# 		elements[i].style[prop] = value;
		# 	}
		# }
		# setProp('item', 'backgroundColor', x.activebg);
		# setProp('item', 'color', x.textlight);
		# setProp('selectize-input', 'backgroundColor', x.bg);
		# setProp('selectize-input', 'color', x.text);
		# setProp('option.active', 'backgroundColor', x.bg);
		# setProp('option.active', 'color', x.text);
		# setProp('selectize-dropdown-content', 'backgroundColor', x.bg);
		# setProp('form-control', 'color', x.text);

		shiny::tags$style(shiny::HTML('div.sticky {
		  position: -webkit-sticky;
		  position: sticky;
		  top: 0;
		  z-index: 1;
		}

		.modal-title {
			color: #000000;
		}

		 .well {
			background-image: url("imgResources/cols4all_logo.png");
			background-repeat: no-repeat;
			background-size: 86px 100px;
			background-position: 97% 1%;
           	background-color:transparent;
		 }
		')),
		# font-size: 13px;
		# line-height: 1.333;
# 		shiny::tags$style(shiny::HTML("div.sticky {
# 		  position: -webkit-sticky;
# 		  position: sticky;
# 		  font-size: 13px;
# 		  line-height: 1.333;
# 		  top: 0;
# 		  z-index: 1;
# 		}
# 		.radio {color: #6699EE; background-color: coral; vertical-align:bottom; display:block-inline;}
# 		h4 {font-weight: bold;margin-top: 20px; color: }
# 		.control-label {font-size: 1.1em;}
# 		.selectize-control {line-height: 16px;}
# 		.selectize-input { font-size: 13px; line-height: 13px; min-height: 20px;}
# .selectize-dropdown { font-size: 13px; line-height: 13px; }
# 		")),

		# Application title
		#shiny::titlePanel("col4all: colors for all!"),

		shiny::titlePanel(title = "Colors for all!"),


		shiny::sidebarLayout(
			shiny::div(shiny::sidebarPanel(
				width = 3,
				shiny::radioButtons("type", "Palette Type", choices = types, selected = type),
				shiny::fluidRow(
					shiny::column(8,
								  shiny::selectizeInput("series", "Palette Series", choices = series_per_type[[type]], selected = first_series, multiple = TRUE)),
					shiny::column(4,
								  shiny::div(style = "margin-top: 25px", shiny::actionButton("overview", label = "Overview")))),
				shiny::conditionalPanel(
					condition = "input.type.substring(0, 3) != 'biv'",
					shiny::sliderInput("n", "Number of colors", min = ns$nmin, max = ns$nmax, value = ns$n, ticks = FALSE)),
				shiny::conditionalPanel(
					condition = "input.type.substring(0, 3) == 'biv'",
					shiny::fluidRow(
						shiny::column(6,
							shiny::sliderInput("nbiv", "Number of columns", min = 3, max = 5, value = 5, ticks = FALSE)),
						shiny::column(6,
							shinyjs::disabled(shiny::sliderInput("mbiv", "Number of rows", min = 3, max = 5, value = 5, ticks = FALSE))))),
				shiny::checkboxInput("na", "Color for missing values", value = FALSE),
				shiny::conditionalPanel(
					condition = "input.type == 'seq' || input.type == 'div'",
					shiny::fluidRow(
						shiny::column(4,
							#shiny::br(),
							shiny::radioButtons("auto_range", label = "Range", choices = c("Automatic", "Manual"), selected = "Automatic")),
						shiny::conditionalPanel(
							condition = "input.auto_range == 'Manual'",
							shiny::column(8,
								shiny::div(style = "font-size:0;margin-bottom:-20px", shiny::sliderInput("range", "",
												   min = 0, max = 1, value = c(0,1), step = .05)),
								shiny::uiOutput("range_info"))))),
				shiny::fluidRow(
					shiny::column(7,
								  shiny::radioButtons("format", "Text format", choices = c("Hex" = "hex", "RGB" = "RGB", "HCL" = "HCL"), inline = TRUE)),
					shiny::column(5,
								  shiny::selectizeInput("textcol", "Text color", choices = c("Hide text" = "same", Black = "#000000", White = "#FFFFFF", Automatic = "auto")))),
				shiny::radioButtons("cvd", "Color vision", choices = c(Normal = "none", 'Deutan (red-green blind)' = "deutan", 'Protan (also red-green blind)' = "protan", 'Tritan (blue-yellow)' = "tritan"), selected = "none"),
				shiny::div(
				shiny::fluidRow(
					shiny::column(7,
						shiny::selectizeInput("sort", "Sort", choices = structure(c("name", "rank"), names = c("Name", .C4A$labels["cbfriendly"])), selected = "rank")),
					shiny::column(5,
						shiny::br(),
						shiny::checkboxInput("sortRev", "Reverse", value = FALSE))), style = "margin-bottom:-10px;"),
				shiny::checkboxInput("advanced", "Show underlying scores", value = FALSE),
				shiny::checkboxInput("dark", "Dark mode", value = FALSE)
			), class = "sticky"),

			shiny::mainPanel(
				shiny::tableOutput("show")
			)
		)
	)
	server = function(input, output, session) {


		rv = shiny::reactiveValues(selected_series = series,
								   current_type = type)

		shiny::observeEvent(get_cols(), {
			cols = get_cols()
			sortNew = if (input$sort %in% cols) input$sort else "name"
			shiny::freezeReactiveValue(input, "sort")
			shiny::updateSelectInput(session, "sort", choices  = cols,selected = sortNew)
		})

		shiny::observeEvent(input$dark, {

			#input$series # otherwise newly added items will always be white
			if (input$dark) {
				x = list(bg = "#000000", text = "#bbbbbb", activebg = "#202020", textlight = "#bbbbbb")
			} else {
				x = list(bg = "#ffffff", text = "#000000", activebg = "#efefef", textlight = "#333333")
			}
			session$sendCustomMessage("background-color", x)
		})


		shiny::observeEvent(input$type, {
			type = input$type
			series = input$series

			choices = series_per_type[[type]]
			not_selected = setdiff(series_per_type[[rv$current_type]], series)
			rv$selected_series = union(setdiff(rv$selected_series, not_selected), series)
			rv$current_type = type

			selected = intersect(choices, rv$selected_series)

			shiny::freezeReactiveValue(input, "series")
			shiny::updateSelectizeInput(session, "series", choices = choices, selected = selected)
		})

		shiny::observeEvent(input$series, {
			type = input$type

			if (!(type %in% c("cat", "seq", "div"))) return(NULL)
			series = input$series

			ns =  def_n(npref = NA, type, series, tab_nmin, tab_nmax)

			shiny::freezeReactiveValue(input, "n")
			shiny::updateSliderInput(session, "n", min = ns$nmin, max = ns$nmax, value = ns$n)
		})

		shiny::observeEvent(input$overview, {
			type = input$type
			title = paste0("Overview of palettes per series of type ", type)
			shiny::showModal(shiny::modalDialog(title = "Number of palettes per series (rows) and type (columns)",
												shiny::renderTable(tab_k, na = "", striped = TRUE, hover = TRUE, bordered = TRUE),
												shiny::div(style="font-size: 75%;", shiny::renderTable(.C4A$type_info)),
												footer = modalButton("Close"),
												style = "color: #000000;"))
		})

		get_values = shiny::reactive({
			list(n = input$n,
				 nbiv = input$nbiv,
				 mbiv = input$mbiv,
				 type = input$type,
				 cvd = input$cvd,
				 sort = input$sort,
				 sortRev = input$sortRev,
				 series = input$series,
				 show.scores = input$advanced,
				 columns = if (input$n > 16) 12 else input$n,
				 na = input$na,
				 range = if (input$auto_range == "Automatic") NA else input$range,
				 textcol = input$textcol,
				 format = input$format)
		})
		get_values_d = shiny::debounce(get_values, 300)

		get_cols = shiny::reactive({
			res = table_columns(input$type, input$advanced)
			structure(c("name", res$qn), names = c("Name", res$ql))
		})



		shiny::observeEvent(input$type, {
			type = input$type
			if (type == "bivs") {
				shinyjs::enable("nbiv")
				shinyjs::disable("mbiv")
				shiny::updateSliderInput(session, "nbiv", value = 5)
				shiny::updateSliderInput(session, "mbiv", value = 5)
			} else if (type == "bivc") {
				shinyjs::enable("mbiv")
				shinyjs::disable("nbiv")
				shiny::freezeReactiveValue(input, "nbiv")
				shiny::freezeReactiveValue(input, "mbiv")
				shiny::updateSliderInput(session, "nbiv", value = 3)
				shiny::updateSliderInput(session, "mbiv", value = 5)
			} else  {
				shinyjs::enable("nbiv")
				shinyjs::enable("mbiv")
				shiny::freezeReactiveValue(input, "nbiv")
				shiny::freezeReactiveValue(input, "mbiv")
				shiny::updateSliderInput(session, "nbiv", value = 5)
				shiny::updateSliderInput(session, "mbiv", value = 5)
			}
		})

		shiny::observeEvent(input$nbiv, {
			nbiv = input$nbiv
			type = input$type
			if (type == "bivs") {
				shiny::freezeReactiveValue(input, "mbiv")
				shiny::updateSliderInput(session, "mbiv", value = nbiv)
			}
		})

		# shiny::observe({
		# 	n = input$n
		# 	ac = input$auto_range
		# 	type = input$type
		#
		# 	if (type == "cat") return(NULL)
		# 	if (ac != "Manual") {
		# 		shiny::freezeReactiveValue(input, "range")
		# 		shinyjs::disable("range")
		# 		if (ac == "Maximum") {
		# 			rng = c(0, 1)
		# 		} else {
		# 			fun = paste0("default_range_", type)
		# 			rng = do.call(fun, list(k = n))
		# 		}
		# 		shinyjs::disable("range")
		# 		shiny::updateSliderInput(session, "range", value = c(rng[1], rng[2]))
		# 	} else {
		# 		shinyjs::enable("range")
		# 	}
		# })

		output$range_info = shiny::renderUI({
			#if (input$type == "div") shiny::div(style="text-align:left;", shiny::tagList("middle", shiny::span(stype = "float:right;", "each side"))) else ""
			if (input$type == "div") {
				shiny::HTML("<div style='font-size:70%; color:#111111; text-align:left;'>Middle<span style='float:right;'>Both sides</span></div>")
			} else {
				shiny::HTML("<div style='font-size:70%; color:#111111; text-align:left;'>Left<span style='float:right;'>Right</span></div>")
			}
		})


		output$show = function() {
			shiny::req(get_values_d())
			values = get_values_d()
			sort = paste0({if (values$sortRev) "-" else ""}, values$sort)
			if (is.null(values$series)) {
				tab = NULL
			} else if (values$type %in% c("bivs", "bivc", "bivu")) {
				tab = c4a_table(n = values$nbiv, m = values$mbiv, cvd.sim = values$cvd, sort = sort, columns = values$columns, type = values$type, show.scores = values$show.scores, series = values$series, range = values$range, include.na = values$na, text.col = values$textcol, text.format = values$format, verbose = FALSE)
			} else {
				tab = c4a_table(n = values$n, cvd.sim = values$cvd, sort = sort, columns = values$columns, type = values$type, show.scores = values$show.scores, series = values$series, range = values$range, include.na = values$na, text.col = values$textcol, text.format = values$format, verbose = FALSE)
			}
			if (is.null(tab)) {
				kableExtra::kbl(data.frame("No palettes found. Please change the selection."), col.names = " ")
			} else {
				tab
			}
		}

	}
	shiny::shinyApp(ui = ui, server = server)
}
