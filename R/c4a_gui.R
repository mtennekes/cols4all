def_n = function(npref = NA, type, series, tab_nmin, tab_nmax) {

	nmin = suppressWarnings(min(tab_nmin[series, type], na.rm = TRUE))
	nmax = suppressWarnings(max(tab_nmax[series, type], na.rm = TRUE))
	if (is.infinite(nmin)) return(NULL)

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

check_installed_packages = function(packages) {
	x = vapply(packages, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE)

	r = packages[!x]

	if (any(!x)) {
		message(paste0("The package",
		   ifelse(sum(!x) > 1, "s ", " "),
		   paste(r, collapse = ", "),
		   ifelse(sum(!x) > 1, " are", " is"),
		   " required. Please install ",
		   ifelse(sum(!x) > 1, "them", "it"),
		   " with:\ninstall.packages(",
		   ifelse(sum(!x) > 1,
		   	   paste0("c(\"", paste(r, collapse = "\", \""), "\"))"),
		   	   paste0("\"", paste(r, collapse = "\", \""), "\")"))))

		FALSE
	} else {
		TRUE
	}
}


#' @rdname c4a_gui
#' @name c4a_gui
#' @export
c4a_gui = function(type = "cat", n = NA, series = "all") {
	ani_off = shiny::icon("circle-xmark", "fa-2x fa-solid", verify_fa = FALSE)
	ani_on = shiny::icon("circle-info", "fa-2x fa-light", verify_fa = FALSE)

	if (!check_installed_packages(c("shiny", "shinyjs", "kableExtra", "colorblindcheck"))) return(invisible(NULL))


	shiny::addResourcePath(prefix = "imgResources", directoryPath = system.file("img", package = "cols4all"))

	#############################
	## Catelogue tab
	#############################

	z = .C4A$z

	z = z[order(z$fullname), ]

	tps = unname(.C4A$types)

	tab_nmin = tapply(z$nmin, INDEX = list(z$series, factor(z$type, levels = tps)), FUN = min)
	tab_nmax = tapply(z$nmax, INDEX = list(z$series, factor(z$type, levels = tps)), FUN = max)
	tab_k = as.data.frame(tapply(z$nmin, INDEX = list(z$series, factor(z$type, levels = tps)), FUN = length))
	tab_k$series = rownames(tab_k)
	tab_k$description = ""

	if (!is.null(.C4A$zdes)) {
		mtch = intersect(tab_k$series, names(.C4A$zdes))
		tab_k$description[match(mtch, tab_k$series)] = unname(.C4A$zdes[mtch])
	}

	tab_k = tab_k[, c("series", "description", tps)]


	allseries = sort(unique(z$series))
	if (series[1] == "all") {
		series = allseries
	} else {
		if (!all(series %in% allseries)) message("These series do not exist: \"", paste(setdiff(series, allseries), collapse = "\", \""), "\"")
		series = intersect(series, allseries)
	}
	if (!length(series)) {
		message("No palette series to show. Either restart c4a_gui with different parameters, add palette data with c4a_load or c4a_sysdata_import")
		return(invisible(NULL))
	}

	types_available = names(which(apply(tab_nmin, MARGIN = 2, function(x)any(!is.na(x)))))

	stopifnot(length(types_available) > 0L)
	if (!(type %in% types_available)) {
		warning("type \"", type, "\" is not available/known")
		type = types_available[1]
	}


	ns = def_n(npref = n, type, series, tab_nmin, tab_nmax)

	types = .C4A$types

	types1 = .C4A$types1
	types2 = .C4A$types2

	if (nchar(type) == 3) {
		type1 = type
		type2 = if (type %in% names(types2)) unname(types2[[type]][1]) else ""
	} else {
		type2 = type
		type1 = substr(type, 1, 3)
	}
	type12 = paste0(type1, type2)

	init_pal_list = z$fullname[z$type == type12]


	#############################
	## Contrast tab
	#############################

	palette = z$fullname[1]

	x = c4a_info(palette)

	n_init = x$ndef
	pal_init = c(c4a(palette, n = n_init), "#ffffff", "#000000")


	getNames = function(p) {
		lapply(p, function(pi) {
			shiny::HTML(paste0("<div style='font-size:2em;line-height:0.5em;height:0.5em;color:", pi, "'>&#9632;</div>"))
		})
	}



	.C4A_HASH = new.env(FALSE, parent=environment())
	.C4A_HASH$vals = list()
	.C4A_HASH$tables = list()

	infoBoxUI = function(inp = NULL, title) {
		if (is.null(inp)) {
			# padding to compensate for button
			shiny::div(style="display: inline-block; padding: 5px;", shiny::HTML(paste0("<h4 style='font-weight: bold; display: inline;'>", title ,"</h4>")))
		} else {
			shiny::div(style="display: inline-block", shiny::HTML(paste0("<h4 style='font-weight: bold; display: inline;'>", title ,"</h4>")), shiny::actionButton(inp, "", ani_on, style = "border: none;"))
		}
	}
	plotOverlay = function(outputId, width, height, id, click = NULL) {
		shiny::div(style = paste0("position: relative; width: ", width, "; height: ", height, ";"),
				   shiny::plotOutput(outputId = outputId, width = width, height = height, click = click),
				   shiny::img(id = id, class = "hide", src = "", style = "pointer-events: none; position: absolute; left: 0px; right: 0px"))
	}

	ui = shiny::fluidPage(
		shinyjs::useShinyjs(),
		shiny::tags$script(src = "https://kit.fontawesome.com/f175d6d133.js"),
		shiny::tags$head(shiny::includeCSS(system.file("www/light.css", package = "cols4all"))),
		shiny::tags$head(shiny::includeCSS(system.file("www/dark.css", package = "cols4all"))),
		shiny::tags$head(shiny::includeCSS(system.file("www/misc.css", package = "cols4all"))),
		shiny::absolutePanel(
			top = 25,
			right = 40,
			width = 90,
			shiny::checkboxInput("dark", "Dark mode", value = FALSE)),
		# shiny::absolutePanel(
		# 	top = 10,
		# 	right = 10,
		# 	width = 20,
		# 	shiny::actionButton("info", "", shiny::icon("info"),
		# 						style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
		# ),
		shiny::tabsetPanel(
			id="inTabset",
			shiny::tabPanel("Overview",
							value = "tab_catel",
					 shiny::fluidRow(
					 	shiny::column(width = 3,
					 				  shiny::img(src = "imgResources/cols4all_logo.png", height="200", align = "center", 'vertical-align' = "center")),
					 	shiny::column(width = 9,
					 				  shiny::fluidRow(
					 				  	shiny::column(width = 4,
		 				  				  shiny::radioButtons("type1", "Palette type", choices = types1, selected = type1),
		 				  				  shiny::conditionalPanel(
		 				  				  	condition = "input.type1 == 'biv'",
		 				  				  	shiny::selectizeInput("type2", "Subtype", choices = types2[["biv"]], selected = type2))),
					 				  	shiny::column(width = 4,
					 				  				  shiny::conditionalPanel(
					 				  				  	condition = "input.type1 != 'biv'",
					 				  				  	shiny::sliderInput("n", "Number of colors", min = ns$nmin, max = ns$nmax, value = ns$n, ticks = FALSE)),
					 				  				  shiny::conditionalPanel(
					 				  				  	condition = "input.type1 == 'biv'",
					 				  				  	shiny::fluidRow(
					 				  				  		shiny::column(6,
					 				  				  					  shiny::sliderInput("nbiv", "Number of columns", min = 3, max = 7, value = 3, ticks = FALSE)),
					 				  				  		shiny::column(6,
					 				  				  					  shinyjs::disabled(shiny::sliderInput("mbiv", "Number of rows", min = 3, max = 7, value = 3, ticks = FALSE))))),
					 				  				  shiny::checkboxInput("na", "Color for missing values", value = FALSE),
					 				  				  shiny::conditionalPanel(
					 				  				  	condition = "input.type1 == 'seq' || input.type1 == 'div'",
					 				  				  	shiny::fluidRow(
					 				  				  		shiny::column(4,
					 				  				  					  #shiny::br(),
					 				  				  					  shiny::radioButtons("auto_range", label = "Range", choices = c("Automatic", "Manual"), selected = "Automatic")),
					 				  				  		shiny::conditionalPanel(
					 				  				  			condition = "input.auto_range == 'Manual'",
					 				  				  			shiny::column(8,
					 				  				  						  shiny::div(style = "font-size:0;margin-bottom:-10px", shiny::sliderInput("range", "", min = 0, max = 1, value = c(0,1), step = .05)),
					 				  		shiny::uiOutput("range_info"))
					 				  		)
					 				  ))),
					 				  shiny::column(width = 4,
					 				  			  shiny::radioButtons("cvd", "Color vision", choices = c(Normal = "none", 'Deutan (red-green blind)' = "deutan", 'Protan (also red-green blind)' = "protan", 'Tritan (blue-yellow)' = "tritan"), selected = "none")
					 				  )),
					 	shiny::fluidRow(
					 		shiny::column(width = 4,
					 					  shiny::div(style = "margin-bottom: 5px;", shiny::strong("Palette series")),
					 					  shiny::div(class = 'multicol',
					 					  		   shiny::checkboxGroupInput("series", label = "", choices = allseries, selected = series, inline = FALSE)),
					 					  shiny::fluidRow(
					 					  	shiny::column(12, align="right",
					 					  				  shiny::actionButton("all", label = "All"),
					 					  				  shiny::actionButton("none", label = "None"),
					 					  				  shiny::actionButton("overview", label = "Overview")))),
					 		shiny::column(width = 4,
					 					  shiny::fluidRow(
					 					  	shiny::column(6,
					 					  				  shiny::radioButtons("format", "Text format", choices = c("Hex" = "hex", "RGB" = "RGB", "HCL" = "HCL"), inline = FALSE)
					 					  	),
					 					  	shiny::column(6,
					 					  				  shiny::radioButtons("textcol", "Text color", choices = c("Hide text" = "same", Black = "#000000", White = "#FFFFFF", Automatic = "auto"), inline = FALSE)	))
					 		),
					 		shiny::column(width = 4,
					 					  shiny::selectizeInput("sort", "Sort", choices = structure(c("name", "rank"), names = c("Name", .C4A$labels["cbfriendly"])), selected = "name"),
					 					  shiny::checkboxInput("sortRev", "Reverse sorting", value = FALSE),
					 					  shiny::checkboxInput("advanced", "Show scores", value = FALSE)
					 		)))),

					 shiny::fluidRow(
					 	shiny::column(
					 		width = 12,
					 		shiny::tableOutput("show"))
					 )
			),
			shiny::tabPanel("Color Blind Friendliness",
							value = "tab_cvd",
							shiny::fluidRow(
								shiny::column(width = 12,
											  shiny::selectizeInput("cbfPal", "Palette", choices = init_pal_list),
											  shiny::br(),
											  shiny::br(),
											  infoBoxUI("infoSimu", "Color blindness simulation"),
											  plotOverlay("cbfSimu", width = "800px", height = "150px", "aniSimu"))),
							shiny::fluidRow(
								shiny::column(width = 4,
											  shiny::br(),
											  shiny::br(),
											  infoBoxUI("infoHueLines", "Hue lines")),



					  #### **Hue lines**")),
								shiny::column(width = 6,
											  shiny::br(),
											  shiny::br(),
											  infoBoxUI("infoSimi", "Similarity matrix"))),
							shiny::fluidRow(shiny::column(width = 4, shiny::markdown("Normal color vision")),
											shiny::column(width = 6, shiny::radioButtons("cbfScore", NULL, choices = c("Symbols", "Scores"), inline = TRUE)),
											shiny::column(width = 2, shiny::radioButtons("cbfType", NULL, choices = c("Map", "Lines"), inline = TRUE))),
							shiny::fluidRow(shiny::column(width = 4, plotOverlay("cbfHL", width = "375px", height = "375px", "aniHL")),
											shiny::column(width = 6, plotOverlay("cbfSimi", width = "500px", height = "375px", "aniSimi", click = "cbfSimi_click")),
											shiny::column(width = 2, shiny::plotOutput("cbf_ex1", height = "375px", width = "150px"))),
							shiny::fluidRow(
								shiny::column(width = 4,
											  shiny::br(),
											  shiny::br(),
											  infoBoxUI("infoConf", "Confusion lines")),
								shiny::column(width = 6,
											  shiny::br(),
											  shiny::br(),
											  infoBoxUI("infoPSimi", "Perceived similarity matrices"))),
								#shiny::column(width = 6, shiny::markdown("<br/><br/>
					  #### **Distance matrices**"))),
							shiny::fluidRow(shiny::column(width = 12, shiny::markdown("Deutan (red-green blind)"))),
							shiny::fluidRow(shiny::column(width = 4, plotOverlay("cbfCL1", width = "375px", height = "375px", "aniCL1")),
											shiny::column(width = 6, plotOverlay("cbfPSimi1", width = "500px", height = "375px", "aniPSimi1", click = "cbfPSimi1_click")),
											shiny::column(width = 2, shiny::plotOutput("cbf_ex2", height = "375px", width = "150px"))),
							shiny::fluidRow(shiny::column(width = 12, shiny::markdown("<br/><br/>Protan (also red-green blind)"))),
							shiny::fluidRow(shiny::column(width = 4, plotOverlay("cbfCL2", width = "375px", height = "375px", "aniCL2")),
											shiny::column(width = 6, plotOverlay("cbfPSimi2", width = "500px", height = "375px", "aniPSimi2", click = "cbfPSimi2_click")),
											shiny::column(width = 2, shiny::plotOutput("cbf_ex3", height = "375px", width = "150px"))),
							shiny::fluidRow(shiny::column(width = 12, shiny::markdown("<br/><br/>Tritan (blue-yellow)"))),
							shiny::fluidRow(shiny::column(width = 4, plotOverlay("cbfCL3", width = "375px", height = "375px", "aniCL3")),
											shiny::column(width = 6, plotOverlay("cbfPSimi3", width = "500px", height = "375px", "aniPSimi3", click = "cbfPSimi3_click")),
											shiny::column(width = 2, shiny::plotOutput("cbf_ex4", height = "375px", width = "150px")))),
			shiny::tabPanel("HCL Analysis",
							value = "tab_cl",

							shiny::fluidRow(
								shiny::column(width = 12,
											  shiny::selectizeInput("CLPal", "Palette", choices = init_pal_list))),
							shiny::fluidRow(
								shiny::column(width = 6,
											  infoBoxUI("infoHUE", "Hue necklace"),
											  plotOverlay("anaHUE", width = "400px", height = "400px", "aniHUE")),
								shiny::column(width = 3,
											  infoBoxUI(title = "HCL space"),
											  shiny::img(src = "imgResources/hcl_spacex1.png", srcset = "imgResources/hcl_spacex1.png 1x, imgResources/hcl_spacex2.png 2x"),
											  shiny::markdown("
															**Dimensions**

															Hue - in degrees (0 to 360)

															Chroma - in the range (0 to 100 or above*)

															Luminance - in the range (0 to 100)

															<font size ='1'>*The maximum C depends on H and L</font>
															"))),
							shiny::fluidRow(
								shiny::column(width = 12,
											  infoBoxUI("infoCL", "Chroma-Luminance"),
											  plotOverlay("anaCL", width = "600px", height = "600px", "aniCL")))),
			shiny::tabPanel("Naming",
							value = "tab_name",
							shiny::fluidRow(
								shiny::column(width = 12,
									shiny::selectizeInput("namePal", "Palette", choices = init_pal_list),
									infoBoxUI("infoName", "Naming table"),
									plotOverlay("anaName", width = "1000px", height = "600px", "aniName"))),
									#shiny::plotOutput("namePlot", height = "600px", width = "1000px"))),
							shiny::fluidRow(
								shiny::column(width = 12,
											  shiny::sliderInput("nameAlpha", "Clarity level", min = .5, max = 3, step = .5, value = 2),
											  shiny::actionButton("showWeights", "Show weight calibration"))),
							shiny::conditionalPanel(
								condition = "input.showWeights % 2 == 1",
								shiny::fluidRow(
									shiny::column(width = 3,
											  shiny::sliderInput("w_1", "Green", min = 0, max = 1.2, step = 0.01, value = unname(.C4A$boynton_weights[1])),
											  shiny::sliderInput("w_2", "Blue", min = 0, max = 1.2, step = 0.01, value = unname(.C4A$boynton_weights[2])),
											  shiny::sliderInput("w_3", "Purple", min = 0, max = 1.2, step = 0.01, value = unname(.C4A$boynton_weights[3])),
											  shiny::sliderInput("w_4", "Pink", min = 0, max = 1.2, step = 0.01, value = unname(.C4A$boynton_weights[4]))
									),
									shiny::column(width = 3,
												  shiny::sliderInput("w_5", "Yellow", min = 0, max = 1.2, step = 0.01, value = unname(.C4A$boynton_weights[5])),
												  shiny::sliderInput("w_6", "Brown", min = 0, max = 1.2, step = 0.01, value = unname(.C4A$boynton_weights[6])),
												  shiny::sliderInput("w_7", "Orange", min = 0, max = 1.2, step = 0.01, value = unname(.C4A$boynton_weights[7])),
												  shiny::sliderInput("w_8", "Red", min = 0, max = 1.2, step = 0.01, value = unname(.C4A$boynton_weights[8]))
									),
									shiny::column(width = 3,
												  shiny::sliderInput("w_9", "White", min = 0, max = 1.2, step = 0.01, value = unname(.C4A$boynton_weights[9])),
												  shiny::sliderInput("w_10", "Gray", min = 0, max = 1.2, step = 0.01, value = unname(.C4A$boynton_weights[10])),
												  shiny::sliderInput("w_11", "Black", min = 0, max = 1.2, step = 0.01, value = unname(.C4A$boynton_weights[11])),
												  shiny::actionButton("w_do", "Update naming data")
									)
								)
							)

			),
			shiny::tabPanel("Contrast",
							value = "tab_cont",
							shiny::fluidRow(
								shiny::column(width = 12,
											  shiny::selectizeInput("contrastPal", "Palette", choices = init_pal_list))),
				 	shiny::fluidRow(
				 		shiny::column(width = 6,
				 					  infoBoxUI("infoCR", "Contrast ratio"),
				 					  plotOverlay("table", width = "400px", height = "300px", "aniTable", click = "table_click")),
				 		shiny::column(width = 6,
	 					  shiny::markdown("<br></br>
#### **Text readability**
"),
	 					  shiny::uiOutput("textCR"),
	 					  shiny::plotOutput("textPlot", height = "200", width = "400")
				 		)),

				 	shiny::fluidRow(
				 		shiny::column(width = 12,
				 					  shiny::markdown("<br/><br/>
				 					  #### **Border lines needed?**
				 					  "),
				 					  shiny::uiOutput("bordersCR"))),
				 	shiny::fluidRow(
				 		shiny::column(width = 3,
				 					  shiny::markdown(""),
				 					  shiny::radioButtons("chart", "Example chart", c("Choropleth", "Barchart"), "Choropleth", inline = FALSE),
			 					  	  shiny::sliderInput("lwd", "Line Width", min = 0, max = 3, step = 1, value = 0),
				 					  shiny::selectizeInput("borders", "Borders", choices = c("black", "white"), selected = "black")),
				 		shiny::column(
				 			width = 9,
				 			shiny::plotOutput("ex", height = "300px", width = "600px")
				 		)

				 	),
				 	shiny::fluidRow(
				 		shiny::column(width = 12,
				 					  shiny::markdown("**Optical Illusion Art**"),
				 					  shiny::plotOutput("ex_plus", height = "703", width = "900"),
				 					  shiny::markdown("<br/><br/>_Plus Reversed_ by Richard Anuszkiewicz (1960)"),
				 					  shiny::checkboxInput("plus_rev_original", "Use optical illusion's original colors", value = FALSE),
				 		)
					)

			),
			shiny::tabPanel("3D Blues",
							value = "tab_floating",
							shiny::fluidRow(
								shiny::column(width = 12,
											  shiny::selectizeInput("floatPal", "Palette", choices = init_pal_list))),

							shiny::fluidRow(
								shiny::column(width = 8,
											  infoBoxUI("infoBlues", "Chromostereopsis"),
											  plotOverlay("blues", width = "550px", height = "550px", "aniBlues"),
											  shiny::markdown("<br/><br/>[_Visual illusion by Michael Bach_](https://michaelbach.de/ot/col-chromostereopsis/)"),
											  shiny::checkboxInput("float_original", "Use optical illusion's original colors", value = FALSE),
											  shiny::checkboxInput("float_rev", "Reverse colors", value = FALSE)),
								shiny::column(width = 4,
											  shiny::plotOutput("float_letters", "Float letter", height = 80, width = 300),
											  shiny::uiOutput("float_selection"),
											  shiny::plotOutput("float_letters_AB", "Float letter", height = 150, width = 300)
											  ))),




			shiny::tabPanel("Application",
							value = "tab_app",
				shiny::fluidRow(
					shiny::column(width = 4,
								  shiny::selectizeInput("APPPal", "Palette", choices = init_pal_list)),
					shiny::column(width = 4,
								  shiny::selectizeInput("APPcvd", "Color vision deficinecy", choices = c(Normal = "none", 'Deutan (red-green blind)' = "deutan", 'Protan (also red-green blind)' = "protan", 'Tritan (blue-yellow)' = "tritan"), selected = "none"))),
				shiny::fluidRow(
				  	shiny::column(width = 4, shiny::sliderInput("MAPlwd", "Line Width", min = 0, max = 3, step = 1, value = 1)),
				  	shiny::column(width = 4, shiny::selectizeInput("MAPborders", "Borders", choices = c("black", "white"), selected = "black")),
					shiny::column(width = 4, shiny::radioButtons("MAPdist", "Color distribution", choices = c(Random = "random", Gradient = "gradient"), selected = "random"))),
				shiny::fluidRow(
					shiny::column(width = 12, shiny::plotOutput("MAPplot", "Map", width = 800, height = 400))),
				shiny::fluidRow(
					shiny::column(width = 4, shiny::sliderInput("DOTlwd", "Line Width", min = 0, max = 3, step = 1, value = 1)),
					shiny::column(width = 4, shiny::selectizeInput("DOTborders", "Borders", choices = c("black", "white"), selected = "black")),
					shiny::column(width = 4, shiny::radioButtons("DOTdist", "Color distribution", choices = c(Random = "random", Concentric = "concentric"), selected = "random"))),
				shiny::fluidRow(
					shiny::column(width = 12, shiny::plotOutput("DOTplot", "Scatter plot", width = 800, height = 400))),
				shiny::fluidRow(
					shiny::column(width = 12, shiny::plotOutput("TXTplot1", "Text", width = 800, height = 120))),
				shiny::fluidRow(
					shiny::column(width = 12, shiny::plotOutput("TXTplot2", "Text", width = 800, height = 120)))
		)
	),
	shiny::tags$script(
		shiny::HTML('
      $(document).on("shiny:connected", function() {
        $("#showWeights").on("click", function() {
          var currentLabel = $(this).text();
          if (currentLabel === "Show weight calibration") {
            $(this).text("Hide weight calibration");
          } else {
            $(this).text("Show weight calibration");
          }
        });
      });
    ')
	))


	#################################################################################################################################################################################
	#################################################################################################################################################################################
	#################################################################################################################################################################################
	#################################################################################################################################################################################
	#################################################################################################################################################################################
	##############################################################                                             ######################################################################
	##############################################################           Server                            ######################################################################
	##############################################################                                             ######################################################################
	#################################################################################################################################################################################
	#################################################################################################################################################################################
	#################################################################################################################################################################################
	#################################################################################################################################################################################
	#################################################################################################################################################################################
	#################################################################################################################################################################################


	server = function(input, output, session) {
		#############################
		## Catelogue tab
		#############################

		series_d = shiny::debounce(shiny::reactive(input$series), 300)

		get_type12 = shiny::reactive({
			type1 = input$type1
			type12 = if (type1 %in% names(types2)) {
				input$type2
			} else {
				type1
			}
		})

		anno = shiny::reactiveValues(simu = FALSE, hue_lines = FALSE, cvd = FALSE, simi = FALSE, psimi = FALSE, conf_lines = FALSE, hue_neck = FALSE, cl_plot = FALSE, naming = FALSE, cr = FALSE, blues = FALSE)

		tab_vals = shiny::reactiveValues(pal = pal_init,
										 na = FALSE,
										 palBW = c(pal_init, "#FFFFFF", "#000000"),
										 pal_name = palette,
										 n = n_init,
										 colA1 = pal_init[1], colA2 = pal_init[2],
										 colB1 = pal_init[1], colB2 = pal_init[2],
										 colC1 = pal_init[1], colC2 = pal_init[2],
										 CR = colorspace::contrast_ratio(pal_init[1], pal_init[2]),
										 type = type12,
										 cvd = "none",
										 b = approx_blues(pal_init),
										 r = approx_reds(pal_init))



		shiny::observeEvent(input$all, {
			shiny::freezeReactiveValue(input, "series")
			shiny::updateCheckboxGroupInput(session, "series", selected = allseries)
		})

		shiny::observeEvent(input$none, {
			shiny::freezeReactiveValue(input, "series")
			shiny::updateCheckboxGroupInput(session, "series", selected = character())
		})

		shiny::observeEvent(get_cols(), {
			cols = get_cols()
			sort = shiny::isolate(input$sort)
			shiny::freezeReactiveValue(input, "sort")
			sortNew = if (sort %in% cols) sort else "name"
			shiny::updateSelectizeInput(session, "sort", choices  = cols,selected = sortNew)
		})

		shiny::observeEvent(input$dark, {

			if ( ! input$dark ) {
				shinyjs::removeClass(selector = "body", class = "dark")
			} else {
				shinyjs::addClass(selector = "body", class = "dark")
			}

		})


		shiny::observeEvent(get_type12(), {
			type = get_type12()

			if (!(type %in% types_available)) return(NULL)
			if (type %in% c("cat", "seq", "div")) {
				series = series_d()
				if (is.null(series)) return(NULL)
				ns =  def_n(npref = input$n, type, series, tab_nmin, tab_nmax)
				shiny::freezeReactiveValue(input, "n")
				shiny::updateSliderInput(session, "n", min = ns$nmin, max = ns$nmax, value = ns$n)
			} else {
				ndef = unname(.C4A$ndef[type])

				mdef = unname(.C4A$mdef[type])
				if (is.na(mdef)) {
					shinyjs::disable("mbiv")
				} else {
					shinyjs::enable("mbiv")
				}
				nmin = if (type == "bivd") 3 else 2
				nstep = if (type == "bivd") 2 else 1

				shiny::freezeReactiveValue(input, "nbiv")
				shiny::freezeReactiveValue(input, "mbiv")
				shiny::updateSliderInput(session, "nbiv", value = ndef, min = nmin, step = nstep)
				shiny::updateSliderInput(session, "mbiv", value = mdef, min = nmin, step = nstep)
			}
			# print("/+++++++++")
		})


		shiny::observeEvent(series_d(), {
			type = get_type12()

			if (!(type %in% c("cat", "seq", "div"))) return(NULL)

			series = series_d()

			if (is.null(series)) return(NULL)
			ns =  def_n(npref = input$n, type, series, tab_nmin, tab_nmax)
			# print("-------------")
			# print(ns)
			if (is.null(ns)) return(NULL)
			shiny::freezeReactiveValue(input, "n")
			shiny::updateSliderInput(session, "n", min = ns$nmin, max = ns$nmax, value = ns$n)
			# print("/-------------")
		})

		shiny::observeEvent(input$overview, {
			type = get_type12()#rv$type12
			title = paste0("Overview of palettes per series of type ", type)
			shiny::showModal(shiny::modalDialog(title = "Number of palettes per series (rows) and type (columns)",
												shiny::renderTable(tab_k, na = "", striped = TRUE, hover = TRUE, bordered = TRUE),
												shiny::div(style="font-size: 75%;", shiny::renderTable(.C4A$type_info)),
												footer = shiny::modalButton("Close"),
												style = "color: #000000;",
												size = "l"))
		})

		get_cols = shiny::reactive({
			type = get_type12()
			res = table_columns(type, input$advanced)


			xl = c(res$ql, res$sl)
			xn = c(res$qn, res$sn)

			anyD = duplicated(xl)
			structure(c("name", xn[!anyD]), names = c("Name", xl[!anyD]))
		})



		get_values = shiny::reactive({
			if (input$sort == "") return(NULL)

			type = get_type12()
			n = input$n
			if (is.null(n)) return(NULL)
			lst = list(n = n,
				 nbiv = input$nbiv,
				 mbiv = input$mbiv,
				 type = type,
				 cvd = input$cvd,
				 sort = input$sort,
				 sortRev = input$sortRev,
				 series = series_d(),
				 show.scores = input$advanced,
				 columns = if (n > 16) 12 else n,
				 na = input$na,
				 range = if (input$auto_range == "Automatic") NA else input$range,
				 textcol = input$textcol,
				 format = input$format)

			if (substr(type, 1, 3) == "biv") {
				m = n
			} else {
				m = 1
			}

			sel = z$type == type &
				z$series %in% lst$series &
				lst$n <= z$nmax
			if (!any(sel)) {
				lst$pal_names = character(0)
			} else {
				lst$pal_names = sort(z$fullname[sel])
			}
			lst
		})
		#get_values_d = shiny::debounce(get_values, 300)


		shiny::observeEvent(input$nbiv, {
			nbiv = input$nbiv
			type = get_type12()#rv$type12
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
			type = get_type12()#rv$type12


			if (type == "div") {
				shiny::HTML("<div style='font-size:70%; color:#111111; text-align:left;'>Middle<span style='float:right;'>Both sides</span></div>")
			} else {
				shiny::HTML("<div style='font-size:70%; color:#111111; text-align:left;'>Left<span style='float:right;'>Right</span></div>")
			}
		})

		shiny::observe({
			values = get_values()
			pals = values$pal_names
			n = values$n
			if (length(pals)) {
				tab_vals$pals = pals
				tab_vals$n = n
				if (!length(tab_vals$pal_name) || !(tab_vals$pal_name %in% pals)) {
					tab_vals$pal_name = pals[1]
				}
				cols = as.vector(c4a(tab_vals$pal_name, n = n))
				na = values$na
				tab_vals$na = na
				if (na) cols = c(cols, c4a_na(tab_vals$pal_name))
				tab_vals$pal = cols
				tab_vals$palBW = c(cols, "#FFFFFF", "#000000")
				tab_vals$type = values$type
				tab_vals$colA1 = cols[1]
				tab_vals$colA2 = cols[2]
				tab_vals$colB1 = cols[1]
				tab_vals$colB2 = cols[2]
				tab_vals$CR = colorspace::contrast_ratio(cols[1], cols[2])
				tab_vals$b = approx_blues(cols)
				tab_vals$r = approx_reds(cols)
				tab_vals$colC1 = cols[which.max(tab_vals$b)]
				tab_vals$colC2 = cols[which.max(tab_vals$r)]

			} else {
				tab_vals$pal = character(0)
				tab_vals$na = logical(0)
				tab_vals$pals = character(0)
				tab_vals$pal_name = character(0)
				tab_vals$n = integer(0)
				tab_vals$palBW = character(0)
				tab_vals$colA1 = character(0)
				tab_vals$colA2 = character(0)
				tab_vals$colB1 = character(0)
				tab_vals$colB2 = character(0)
				tab_vals$CR = numeric(0)
				tab_vals$colC1 = character(0)
				tab_vals$colC2 = character(0)
				tab_vals$b = integer(0)
				tab_vals$r = integer(0)
				tab_vals$type = character(0)
			}
		})

		shiny::observe({
			if (length(tab_vals$pal)) {
				shiny::updateSelectizeInput(session, "cbfPal", choices = tab_vals$pals, selected = tab_vals$pal_name)
				shiny::updateSelectizeInput(session, "CLPal", choices = tab_vals$pals, selected = tab_vals$pal_name)
				shiny::updateSelectizeInput(session, "namePal", choices = tab_vals$pals, selected = tab_vals$pal_name)
				shiny::updateSelectizeInput(session, "contrastPal", choices = tab_vals$pals, selected = tab_vals$pal_name)
				shiny::updateSelectizeInput(session, "floatPal", choices = tab_vals$pals, selected = tab_vals$pal_name)
				shiny::updateSelectizeInput(session, "APPPal", choices = tab_vals$pals, selected = tab_vals$pal_name)
				shinyjs::enable("cbfPal")
				shinyjs::enable("CLPal")
				shinyjs::enable("namePal")
				shinyjs::enable("contrastPal")
				shinyjs::enable("floatPal")
				shinyjs::enable("APPPal")
			} else {
				shiny::updateSelectizeInput(session, "cbfPal", choices = character(0))
				shiny::updateSelectizeInput(session, "CLPal", choices = character(0))
				shiny::updateSelectizeInput(session, "namePal", choices = character(0))
				shiny::updateSelectizeInput(session, "contrastPal", choices = character(0))
				shiny::updateSelectizeInput(session, "floatPal", choices = character(0))
				shiny::updateSelectizeInput(session, "APPPal", choices = character(0))

				shinyjs::disable("cbfPal")
				shinyjs::disable("CLPal")
				shinyjs::disable("namePal")
				shinyjs::disable("contrastPal")
				shinyjs::disable("floatPal")
				shinyjs::disable("APPPal")
			}

		})

		shiny::observeEvent(input$cbfPal, update_reactive(input$cbfPal, 1))
		shiny::observeEvent(input$CLPal, update_reactive(input$CLPal, 2))
		shiny::observeEvent(input$namePal, update_reactive(input$namePal, 3))
		shiny::observeEvent(input$contrastPal, update_reactive(input$contrastPal, 4))
		shiny::observeEvent(input$floatPal, update_reactive(input$floatPal, 5))
		shiny::observeEvent(input$APPPal, update_reactive(input$APPPal, 6))

		update_reactive = function(pal_name, pal_nr) {
			pal = pal_name
			if (pal == "") {
				tab_vals$pal = character(0)
				tab_vals$pal_name = character(0)
				tab_vals$n = integer(0)
				tab_vals$palBW = character(0)
				tab_vals$colA1 = character(0)
				tab_vals$colA2 = character(0)
				tab_vals$colB1 = character(0)
				tab_vals$colB2 = character(0)
				tab_vals$CR = numeric(0)
				tab_vals$colC1 = character(0)
				tab_vals$colC2 = character(0)
				tab_vals$b = integer(0)
				tab_vals$b = integer(0)
				tab_vals$type = character(0)

			} else {
				x = c4a_info(pal)

				if (tab_vals$n > x$nmax || tab_vals$n < x$nmin) return(NULL)

				cols = as.vector(c4a(x$fullname, n = tab_vals$n))
				if (tab_vals$na) cols = c(cols, c4a_na(tab_vals$pal_name))

				colsBW = c(cols, "#FFFFFF", "#000000")

				tab_vals$pal = cols
				tab_vals$pal_name = pal_name

				tab_vals$palBW = colsBW

				tab_vals$b = approx_blues(cols)
				tab_vals$r = approx_reds(cols)

				if (pal_nr == 4) {
					# select maximum floating colors
					tab_vals$colC1 = cols[which.max(tab_vals$b)]
					tab_vals$colC2 = cols[which.max(tab_vals$r)]
				} else if (pal_nr == 1) {
					tab_vals$colA1 = cols[1]
					tab_vals$colA2 = cols[2]
				} else {
					tab_vals$colB1 = cols[1]
					tab_vals$colB2 = cols[2]
					tab_vals$CR = colorspace::contrast_ratio(cols[1], cols[2])

				}

				tab_vals$type = x$type
			}
			if (pal_nr != 1) shiny::updateSelectizeInput(session, "cbfPal", choices = tab_vals$pals, selected = pal)
			if (pal_nr != 2) shiny::updateSelectizeInput(session, "CLPal", choices = tab_vals$pals, selected = pal)
			if (pal_nr != 3) shiny::updateSelectizeInput(session, "namePal", choices = tab_vals$pals, selected = pal)
			if (pal_nr != 4) shiny::updateSelectizeInput(session, "contrastPal", choices = tab_vals$pals, selected = pal)
			if (pal_nr != 5) shiny::updateSelectizeInput(session, "floatPal", choices = tab_vals$pals, selected = pal)
			if (pal_nr != 6) shiny::updateSelectizeInput(session, "APPPal", choices = tab_vals$pals, selected = pal)

		}



		output$show = function() {

			values = get_values()#_d()
			if (is.null(values) || is.null(values$series)) {
				tab = NULL
			} else {

				if (length(.C4A_HASH$vals)) {
					iden = vapply(.C4A_HASH$vals, function(cv) {
						identical(cv, values)
					}, FUN.VALUE = logical(1))
					if (any(iden)) {
						return(.C4A_HASH$tables[[which(iden)[1]]])
					}
				}



				# Create a Progress object
				progress <- shiny::Progress$new()
				# Make sure it closes when we exit this reactive, even if there's an error
				on.exit(progress$close())

				progress$set(message = "Colors in progress...", value = 0)

				sort = paste0({if (values$sortRev) "-" else ""}, values$sort)

				if (substr(values$type, 1, 3) == "biv") {
					tab = c4a_table(n = values$nbiv, m = values$mbiv, cvd.sim = values$cvd, sort = sort, columns = values$columns, type = values$type, show.scores = values$show.scores, series = values$series, range = values$range, include.na = values$na, text.col = values$textcol, text.format = values$format, verbose = FALSE)
				} else {
					tab = c4a_table(n = values$n, cvd.sim = values$cvd, sort = sort, columns = values$columns, type = values$type, show.scores = values$show.scores, series = values$series, range = values$range, include.na = values$na, text.col = values$textcol, text.format = values$format, verbose = FALSE)
				}
			}

			if (is.null(tab)) {
				kableExtra::kbl(data.frame("No palettes found. Please change the selection."), col.names = " ")
			} else {
				.C4A_HASH$vals = c(.C4A_HASH$vals, list(values))
				.C4A_HASH$tables = c(.C4A_HASH$tables, tab)
				tab
			}
		}

		#############################
		## CBF tab
		#############################

		output$cbfSimu = shiny::renderPlot({
			if (!length(tab_vals$pal)) return(NULL)

			pal = tab_vals$pal
			c4a_plot_cvd(pal, dark = input$dark, include.na = tab_vals$na)
		})

		output$cbfHL = shiny::renderPlot({
			if (!length(tab_vals$pal)) return(NULL)

			pal = tab_vals$pal
			c4a_plot_confusion_lines(pal, cvd = "none", dark = input$dark)
		})

		output$cbfCL1 = shiny::renderPlot({
			if (!length(tab_vals$pal)) return(NULL)

			pal = tab_vals$pal
			c4a_plot_confusion_lines(pal, cvd = "deutan", dark = input$dark)
		})

		output$cbfCL2 = shiny::renderPlot({
			if (!length(tab_vals$pal)) return(NULL)

			pal = tab_vals$pal
			c4a_plot_confusion_lines(pal, cvd = "protan", dark = input$dark)
		})

		output$cbfCL3 = shiny::renderPlot({
			if (!length(tab_vals$pal)) return(NULL)

			pal = tab_vals$pal
			c4a_plot_confusion_lines(pal, cvd = "tritan", dark = input$dark)
		})

		output$cbfSimi = shiny::renderPlot({
			if (!length(tab_vals$pal)) return(NULL)

			pal = tab_vals$pal
			col1 = tab_vals$colA1
			col2 = tab_vals$colA2
			id1 = which(col1 == pal)
			id2 = which(col2 == pal)

			c4a_plot_dist_matrix(pal, cvd = "none", id1 = id1, id2 = id2, dark = input$dark, advanced = (input$cbfScore == "Scores"))
		})

		output$cbfPSimi1 = shiny::renderPlot({
			if (!length(tab_vals$pal)) return(NULL)

			pal = tab_vals$pal
			col1 = tab_vals$colA1
			col2 = tab_vals$colA2
			id1 = which(col1 == pal)
			id2 = which(col2 == pal)

			c4a_plot_dist_matrix(pal, cvd = "deutan", id1 = id1, id2 = id2, dark = input$dark, advanced = (input$cbfScore == "Scores"))
		})

		output$cbfPSimi2 = shiny::renderPlot({
			if (!length(tab_vals$pal)) return(NULL)

			pal = tab_vals$pal
			col1 = tab_vals$colA1
			col2 = tab_vals$colA2
			id1 = which(col1 == pal)
			id2 = which(col2 == pal)

			c4a_plot_dist_matrix(pal, cvd = "protan", id1 = id1, id2 = id2, dark = input$dark, advanced = (input$cbfScore == "Scores"))
		})

		output$cbfPSimi3 = shiny::renderPlot({
			if (!length(tab_vals$pal)) return(NULL)

			pal = tab_vals$pal
			col1 = tab_vals$colA1
			col2 = tab_vals$colA2
			id1 = which(col1 == pal)
			id2 = which(col2 == pal)

			c4a_plot_dist_matrix(pal, cvd = "tritan", id1 = id1, id2 = id2, dark = input$dark, advanced = (input$cbfScore == "Scores"))
		})

		cbf_map = function(cols, cvd) {
			if (!length(cols)) return(NULL)

			hcl = get_hcl_matrix(cols)

			cols_cvd = sim_cvd(cols, cvd)

			borders = ifelse(mean(hcl[,3]>=50), "#000000", "#FFFFFF")

			c4a_plot_map(col1 = cols_cvd[1], col2 = cols_cvd[2], borders = borders, lwd = 1, crop = TRUE, dark = input$dark)
		}

		cbf_lines = function(cols, cvd) {
			if (!length(cols)) return(NULL)

			hcl = get_hcl_matrix(cols)

			cols_cvd = sim_cvd(cols, cvd)
			c4a_plot_lines(col1 = cols_cvd[1], col2 = cols_cvd[2], lwd = 3, asp = .9)
		}



		output$cbf_ex1 = shiny::renderPlot({
			if (!length(tab_vals$colA1)) return(NULL)
			fun = paste0("cbf_", tolower(input$cbfType))
			do.call(fun, list(cols = c(tab_vals$colA1, tab_vals$colA2), cvd = "none"))
		})
		output$cbf_ex2 = shiny::renderPlot({
			if (!length(tab_vals$colA1)) return(NULL)
			fun = paste0("cbf_", tolower(input$cbfType))
			do.call(fun, list(cols = c(tab_vals$colA1, tab_vals$colA2), cvd = "deutan"))
		})
		output$cbf_ex3 = shiny::renderPlot({
			if (!length(tab_vals$colA1)) return(NULL)
			fun = paste0("cbf_", tolower(input$cbfType))
			do.call(fun, list(cols = c(tab_vals$colA1, tab_vals$colA2), cvd = "protan"))
		})
		output$cbf_ex4 = shiny::renderPlot({
			if (!length(tab_vals$colA1)) return(NULL)
			fun = paste0("cbf_", tolower(input$cbfType))
			do.call(fun, list(cols = c(tab_vals$colA1, tab_vals$colA2), cvd = "tritan"))
		})


		#############################
		## HCL analysis tab
		#############################


		output$anaHUE = shiny::renderPlot({
			if (!length(tab_vals$pal)) return(NULL)

			width = switch(tab_vals$type,
						   seq = "total",
						   div = "halves",
						   "none")

			pal = tab_vals$pal
			c4a_plot_hues(pal, dark = input$dark, width = width)
		})
		# output$anaRGB1 = shiny::renderPlot({
		# 	if (!length(tab_vals$pal)) return(NULL)
		#
		# 	pal = tab_vals$pal
		# 	c4a_plot_rgb_space(pal, cvd = "none", dark = input$dark, L = paste0("L", input$rgbL))
		# })

		output$anaCL = shiny::renderPlot({
			pal = tab_vals$pal

			if (!length(pal)) return(NULL)

			type = tab_vals$type

			c4a_plot_CL(pal, Lrange = (type == type), dark = input$dark)
		})


		#############################
		## Contrast tab
		#############################





		output$ex_plus = shiny::renderPlot({
			borders = input$borders
			lwd = input$lwd

			if (input$plus_rev_original) {
				c4a_plot_Plus_Reversed(orientation = "landscape", borders = borders, lwd = lwd)
			} else {
				col1 = tab_vals$colB1
				if (!length(col1)) return(NULL)
				col2 = tab_vals$colB2

				c4a_plot_Plus_Reversed(col1, col2, orientation = "landscape", borders = borders, lwd = lwd)

			}

		})

		output$ex = shiny::renderPlot({

			col1 = tab_vals$colB1
			if (!length(col1)) return(NULL)
			col2 = tab_vals$colB2

			borders = input$borders
			lwd = input$lwd
			if (input$chart == "Barchart") {
				c4a_plot_bars(col1 = col1, col2 = col2, borders = borders, lwd = lwd, dark = input$dark)
			} else {
				c4a_plot_map(col1 = col1, col2 = col2, borders = borders, lwd = lwd, dark = input$dark)
			}
		})

		output$table = shiny::renderPlot({

			col1 = tab_vals$colB1
			if (!length(col1)) return(NULL)
			col2 = tab_vals$colB2
			pal = tab_vals$palBW


			id1 = which(col1 == pal)
			id2 = which(col2 == pal)
			c4a_plot_CR_matrix(pal, id1 = id1, id2 = id2, dark = input$dark)
		})

		#############################
		## Naming tab
		#############################

		observeEvent(input$w_do, {
			shinyjs::runjs("$('#container').prop('disabled', true);")

			w = c4a_options("boynton_weights")[[1]]
			w[1:11] = vapply(1:11, function(i) {
				input[[paste0("w_", i)]]
			}, FUN.VALUE = numeric(1))
			c4a_options(boynton_weights = w)
			updatePlot(w)
			shinyjs::runjs("$('#container').prop('disabled', false);")

		})

		output$anaName = shiny::renderPlot({
			pal = tab_vals$pal
			c4a_plot_names2(pal, dark = input$dark, a = input$nameAlpha)
		})

		updatePlot <- function(result) {
			output$anaName = shiny::renderPlot({
				pal = tab_vals$pal
				c4a_plot_names2(pal, dark = input$dark, a = input$nameAlpha)
			})
		}

		output$textCR = shiny::renderUI({
			cr = tab_vals$CR
			if (!length(cr)) return(NULL)

			txt = if (cr >= 7) {
				"safe to print text according to [WCAG 2.1](https://www.w3.org/TR/WCAG21/) level **AAA**"
			} else if (cr >= 4.5) {
				"safe to print text according to the [WCAG 2.1](https://www.w3.org/TR/WCAG21/) level **AA**"
			} else if (cr >= 3) {
				"safe to print text according to the [WCAG 2.1](https://www.w3.org/TR/WCAG21/) level **A**"
			} else {
				"not safe to print text according to the [WCAG 2.1](https://www.w3.org/TR/WCAG21/)"
			}

			shiny::markdown(paste0("**Contrast ratio** (", sprintf("%.2f", round(cr, 1)), "): ", txt))
		})

		output$bordersCR = shiny::renderUI({
			cr = tab_vals$CR
			if (!length(cr)) return(NULL)

			txt = if (cr <= 1.2) {
				"strongly recommended to use border lines when plotting these colors next to each other"
			} else if (cr <= 1.5) {
				"recommended to use border lines when plotting these colors next to each other"
			} else if (cr <= 2) {
				"consider to use border lines when plotting these colors next to each other"
			} else {
				"colors can be plot next to each other without border lines"
			}
			shiny::markdown(paste0("**Contrast ratio** (", sprintf("%.2f", round(cr, 1)), "): ", txt))
		})

		output$textPlot = shiny::renderPlot({
			col1 = tab_vals$colB1
			if (!length(col1)) return(NULL)
			col2 = tab_vals$colB2

			c4a_plot_text2(c(col1, col2), dark = input$dark)
		})

		get_click_id = function(pal, x, y) {
			n = length(pal)

			brks_x = seq(0.04, 0.76, length.out = n + 2)
			brks_y = seq(0, 1, length.out = n + 2)

			x_id = as.integer(cut(x, breaks = brks_x)) - 1
			y_id = n + 1 - as.integer(cut(y, breaks = brks_y))

			if (!is.na(x_id) && (x_id < 1 || x_id > n)) x_id = NA
			if (!is.na(y_id) && (y_id < 1 || y_id > n)) y_id = NA

			list(x = x_id, y = y_id)
		}

		shiny::observeEvent(input$cbfSimi_click, {
			pal = tab_vals$pal
			if (!length(pal)) return(NULL)

			ids = get_click_id(pal, input$cbfSimi_click$x, input$cbfSimi_click$y)

			if (!is.na(ids$x)) tab_vals$colA2 = pal[ids$x]
			if (!is.na(ids$y)) tab_vals$colA1 = pal[ids$y]
		})

		shiny::observeEvent(input$cbfPSimi1_click, {
			pal = tab_vals$pal
			if (!length(pal)) return(NULL)

			ids = get_click_id(pal, input$cbfPSimi1_click$x, input$cbfPSimi1_click$y)

			if (!is.na(ids$x)) tab_vals$colA2 = pal[ids$x]
			if (!is.na(ids$y)) tab_vals$colA1 = pal[ids$y]

		})

		shiny::observeEvent(input$cbfPSimi2_click, {
			pal = tab_vals$pal
			if (!length(pal)) return(NULL)

			ids = get_click_id(pal, input$cbfPSimi2_click$x, input$cbfPSimi2_click$y)

			if (!is.na(ids$x)) tab_vals$colA2 = pal[ids$x]
			if (!is.na(ids$y)) tab_vals$colA1 = pal[ids$y]
		})

		shiny::observeEvent(input$cbfPSimi3_click, {
			pal = tab_vals$pal
			if (!length(pal)) return(NULL)

			ids = get_click_id(pal, input$cbfPSimi3_click$x, input$cbfPSimi3_click$y)

			if (!is.na(ids$x)) tab_vals$colA2 = pal[ids$x]
			if (!is.na(ids$y)) tab_vals$colA1 = pal[ids$y]
		})


		shiny::observeEvent(input$table_click, {
			pal = tab_vals$palBW
			if (!length(pal)) return(NULL)

			ids = get_click_id(pal, input$table_click$x, input$table_click$y)

			if (!is.na(ids$x)) tab_vals$colB2 = pal[ids$x]
			if (!is.na(ids$y)) tab_vals$colB1 = pal[ids$y]

			if (!is.na(ids$x) || !is.na(ids$y)) tab_vals$CR = colorspace::contrast_ratio(tab_vals$colB1, tab_vals$colB2)
		})




		##############################
		## Floating tab
		##############################



		output$float_letters = shiny::renderPlot({
			pal = tab_vals$pal
			if (!length(pal)) return(NULL)

			c4a_plot_text(pal, dark = input$dark, size = 1.25, frame = TRUE)
		})


		output$float_selection = shiny::renderUI({
			pal = tab_vals$pal
			if (!length(pal)) return(NULL)
			b = tab_vals$b
			r = tab_vals$r

			ids = c(which.max(b)[1], which.max(r)[1])

			#br = get_blue_red()

			if (max(b) > .C4A$Blues && max(r) > 100) {
				btext = paste0("This illusion will likely occur with blue color ", c(LETTERS, letters)[ids[1]], " and a red color (e.g. ", c(LETTERS, letters)[ids[2]], "), at least with a dark background")
			} else if (max(b) > 100 && max(r) > 100) {
				btext = paste0("This illusion could occur with blue color ", c(LETTERS, letters)[ids[1]], " and a red color (e.g. ", c(LETTERS, letters)[ids[2]], "), at least with a dark background")
			} else if (max(b) > 100 && max(r) <= 100) {
				btext = paste0("This illusion will probably not occur, because the palette does not contain a red(dish) color")
			} else {
				btext = paste0("This illusion will probably not occur, because the palette does not contain any blue color")
			}

			shiny::tagList(
				shiny::markdown(paste(btext)),
				shiny::selectizeInput("float_col1", "Color 1", choices = c(LETTERS, letters)[1:length(pal)], selected = c(LETTERS, letters)[ids[1]]),
				shiny::selectizeInput("float_col2", "Color 2", choices = c(LETTERS, letters)[1:length(pal)], selected = c(LETTERS, letters)[ids[2]])
			)
		})

		shiny::observeEvent(input$float_col1, {
			tab_vals$colC1 = tab_vals$pal[which(c(LETTERS, letters) == input$float_col1)]
		})

		shiny::observeEvent(input$float_col2, {
			tab_vals$colC2 = tab_vals$pal[which(c(LETTERS, letters) == input$float_col2)]
		})

		output$float_letters_AB = shiny::renderPlot({
			pal = tab_vals$pal
			if (!length(pal)) return(NULL)

			cols = c(tab_vals$colC1, tab_vals$colC2)


			lL = c(LETTERS,letters)[c(which(pal == cols[1]),
					which(pal == cols[2]))]
			if (length(lL) == 2) {
				c4a_plot_text(cols, words = lL, dark = input$dark, size = 1.25, frame = TRUE)
			}
		})

		output$blues = shiny::renderPlot({
			if (!length(tab_vals$pal)) return(NULL)
			if (!input$float_original) {
				pal = tab_vals$pal


				cols = c(tab_vals$colC1, tab_vals$colC2)

				if (input$float_rev) cols = rev(cols)
				c4a_plot_floating_rings(col1 = cols[2], col2 = cols[1], dark = input$dark)
			} else {
				cols = c("#FF0000", "#0000FF")
				if (input$float_rev) cols = rev(cols)
				c4a_plot_floating_rings(cols[1], cols[2])
			}
		})




		##############################
		## Application tab
		##############################

		output$MAPplot = shiny::renderPlot({
			pal = tab_vals$pal
			if (!length(pal)) return(NULL)
			pal2 = sim_cvd(pal, input$APPcvd)

			c4a_plot_map(pal2, borders = input$MAPborders, lwd = input$MAPlwd, include.na = tab_vals$na, dark = input$dark, dist = input$MAPdist)
		})

		output$DOTplot = shiny::renderPlot({
			pal = tab_vals$pal
			if (!length(pal)) return(NULL)
			pal2 = sim_cvd(pal, input$APPcvd)

			c4a_plot_scatter(pal2,  borders = input$DOTborders, lwd = input$DOTlwd, dark = input$dark, dist = input$DOTdist)
		})

		output$TXTplot1 = shiny::renderPlot({
			pal = tab_vals$pal
			if (!length(pal)) return(NULL)
			pal2 = sim_cvd(pal, input$APPcvd)

			c4a_plot_text(pal2, dark = input$dark)
		})

		output$TXTplot2 = shiny::renderPlot({
			pal = tab_vals$pal
			if (!length(pal)) return(NULL)
			pal2 = sim_cvd(pal, input$APPcvd)
			c4a_plot_text(pal2, dark = input$dark, frame = TRUE)
		})


		##############################
		## Application tab
		##############################


		#observeEvent(input$infoHueLines, infoBoxDialog("Hue Lines", "markdown/infoHueLines.md"))

		oE = function(i, a, id, gif) {
			shiny::observeEvent(input[[i]], {
				anno[[a]] = !anno[[a]]
				if (anno[[a]]) {

					for (j in 1:length(id)) {
						if (length(id) == 1) {
							d = id[1]
						} else {
							d = id[j]
						}
						if (length(gif) == 1) {
							g = gif[1]
						} else {
							g = gif[j]
						}
						gs = paste(paste0("imgResources/", g, 1:2,"x.gif ", 1:2, "x"), collapse = ", ")
						g1 = paste0("imgResources/", g, "x1.gif")
						shinyjs::removeClass(id = d, class = "hide")
						shinyjs::runjs(paste0("
							var logo = document.getElementById('", d, "');
							logo.src = '", g1, "';
							logo.srcset = '", gs, "';
						"))
					}

					shiny::updateActionButton(session, i, icon = ani_off)
				} else {
					for (d in id) {
						shinyjs::addClass(id = d, class = "hide")
						shinyjs::runjs(paste0("
						var logo = document.getElementById('", d, "');
						logo.src = '';
						logo.srcset = '';
					"))
					}
					shinyjs::addClass(id = id, class = "hide")
					shiny::updateActionButton(session, i, icon = ani_on)
				}

			})
		}

		oE("infoHueLines", "hue_lines", "aniHL", "hue_lines")
		oE("infoConf", "conf_lines", c("aniCL1", "aniCL2", "aniCL3"), c("conf_linesD", "conf_linesP", "conf_linesT"))
		oE("infoSimi", "simi", "aniSimi", "simi")
		oE("infoPSimi", "psimi", c("aniPSimi1", "aniPSimi2", "aniPSimi3"), c("simiD", "simiP", "simiT"))
		oE("infoSimu", "simu", "aniSimu", "simu")
		oE("infoHUE", "hue_neck", "aniHUE", "hue_neck")
		oE("infoCL", "cl_plot", "aniCL", "cl_plot")
		oE("infoName", "naming", "aniName", "naming")

		oE("infoCR", "cr", "aniTable", "table")
		oE("infoBlues", "blues", "aniBlues", "blues")

	}
	shiny::shinyApp(ui = ui, server = server)
}
