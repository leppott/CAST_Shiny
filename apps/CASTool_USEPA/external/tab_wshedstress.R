# Watershed Stressors

function() {
	tabPanel(
	# conditionalPanel(
	# 			condition = "input.rad_setup_explore == 'Yes'", # JavaScript condition
				title = "Watershed Stressors",
				value = "tab_wshedstress",
				mainPanel(
					fluidRow(
						column(4,
								p("..some explanation text.."),
								br(),
								p(strong("Reaches to display: ")),
								textOutput("txt_wshed_reach"),
								br(),
								selectInput("si_wshed_var",
												"Select watershed variable",
												choices = "..Run report to create plots..",
												multiple = FALSE),
								br(),
								hr(),
								shinyjs::disabled(downloadButton("but_dload_wshed_figs",
											"Download all watershed stressor figures")),
								shinyBS::bsTooltip(id = "but_dload_wshed_figs",
														 title = paste0("Disabled until report is created."),
														 placement = "right")
						),## column
						column(8,
								 imageOutput("plot_wshed", # size set in plot_wshed
								 				width = "100%",
								 				height = "100%"))
					)## fluidRow
					
							)## mainPanel
				)## tabPanel
}## FUNCTION