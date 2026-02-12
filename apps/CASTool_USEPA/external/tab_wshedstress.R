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
				
				use_bs_popover(),
					h2("Explore watershed stressor figures"),
					p(em("All watershed stressor figures are available in the SiteInfo subfolder in the downloaded results zipped folder.")),
					fluidRow(
						column(4,
								p(tagList(
									strong("Reaches to display: "),
									icon("info-circle", 
										  style = "color: #2fa4e7", 
										  id="reachesInfo") |>
										bs_embed_popover(title = "Helpful Hints",
															  content = "To modify, change the useAllCompReaches parameter in '_CASTool_Metadata.xlsx'.",
															  placement = "right",
															  trigger = "hover"))),
								# bsPopover(id="reachesInfo", 
								# 			 title = HTML("<b>Helpful Hints</b>"), 
								# 			 content = HTML("To modify, change the useAllCompReaches parameter in _CASTool_Metadata.xlsx."),
								# 			 placement = "right", 
								# 			 trigger = "hover"),
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
								 p(tagList(strong("Watershed stressor figure"),
								 			 actionLink("helpWSStrFig", label = NULL, icon = icon("circle-info"), class = "help-btn"))
								   ),
								 imageOutput("plot_wshed", # size set in plot_wshed
								 				width = "100%",
								 				height = "100%")
								 )
					), ## fluidRow
				fluidRow(
					column(12, 
							 p(strong("Table of watershed stressor variables elevated at the target reach")),
							 DT::dataTableOutput("ws_stress_high")))
					
				#			)## mainPanel
				)## tabPanel
}## FUNCTION


