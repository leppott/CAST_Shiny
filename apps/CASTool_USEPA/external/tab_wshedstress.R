# Watershed Stressors

function() {
	tabPanel(
	# conditionalPanel(
	# 			condition = "input.rad_setup_explore == 'Yes'", # JavaScript condition
				title = "Watershed Stressors",
				value = "tab_wshedstress",
				mainPanel(
					p("import data from GitHub to Shiny"),
					bsButton("but_github_wshedstress_data",
								"Import watershed stressor data and metadata from GitHub"),
					br(),
					selectInput("wshed_select",
									"Select watershed variable",
									choices = c(NULL,
													"A", 
													"B"),
									selected = NULL),
					p("Select Date Range"),
					fluidRow(column(3,
										 selectInput("wshed_yr_start",
										 				"Start Year",
										 				choices = c(2000, 2001, 2002),
										 				selected = NULL)),
								column(3, 
										 selectInput("wshed_yr_end",
										 				"End Year",
										 				choices = c(2000, 2001, 2002),
										 				selected = NULL))
								),## fluidRow
					br(),
					bsButton("but_display_fig",
								"Display figure"),
					br(),
					shinyjs::disabled(downloadButton("but_dload_wshed_app_fig",
								"Download watershed stressor appendix and all figures"))
							)## mainPanel
				)## tabPanel
}## FUNCTION