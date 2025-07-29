# Watershed Stressors

function() {
	tabPanel("Watershed Stressors",
				mainPanel(
					bsButton("but_dload_wshed_data",
								"Download watershed stressor data and metadata"),
					br(),
					selectInput("wshed_select",
									"Select watershed variable",
									choices = c(NULL,
													"A", 
													"B"),
									selected = NULL),
					br(),
					bsButton("but_display_fig",
								"Display figure"),
					br(),
					bsButton("but_dload_wshed_app_fig",
								"Download watershed stressor appendix and all figures")
							)## mainPanel
				)## tabPanel
}## FUNCTION