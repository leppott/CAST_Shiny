# Watershed Stressors

function() {
	tabPanel(
	# conditionalPanel(
	# 			condition = "input.rad_setup_explore == 'Yes'", # JavaScript condition
				title = "Watershed Stressors",
				value = "tab_wshedstress",
				tags$head(tags$style(HTML("
				  .pill {
				    background: #f5f5f5;
				    padding: 8px 12px;
				    border-radius: 4px;
				    margin-bottom: 10px;
				  }
						 "))),
				#mainPanel(
					h2("Explore watershed stressor figures"),
					fluidRow(
						column(4,
								p(strong("Reaches to display: ")),
								div(class = "pill", textOutput("txt_wshed_reach")),
								br(),
								selectInput("si_wshed_var",
												"Select watershed variable",
												choices = "..Run report to create plots..",
												multiple = FALSE),
								# br(),
								# hr(),
								# shinyjs::disabled(downloadButton("but_dload_wshed_figs",
								# 			"Download all watershed stressor figures")),
								# shinyBS::bsTooltip(id = "but_dload_wshed_figs",
								# 						 title = paste0("Disabled until report is created."),
								# 						 placement = "right")
						),## column
						column(8,
								 imageOutput("plot_wshed", # size set in plot_wshed
								 				width = "100%",
								 				height = "100%"))
					)## fluidRow
					
				#			)## mainPanel
				)## tabPanel
}## FUNCTION


