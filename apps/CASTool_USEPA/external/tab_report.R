# Report

function() {
	tabPanel("Run Report",
				tags$head(tags$style(HTML("
				  .pill {
				    background: #f5f5f5;
				    padding: 8px 12px;
				    border-radius: 4px;
				    margin-bottom: 10px;
				  }
						 "))),
				mainPanel(
					# doesn't work unless in include this line (even though it is in UI.R)
					useShinyjs(), 
					fluidRow(
						column(4,
								 p(strong("Report delivery format:")),
								 div(class = "pill", textOutput("txt_setup_format")),
								 # selectInput("report_format",
								 # 				"Report delivery format",
								 # 				choices = c("HTML", 
								 # 								"PDF"),
								 # 				selected = "HTML"),
								 br(),
								 p(strong("Target site:")),
								 div(class = "pill", textOutput("txt_rep_siteid")),
								 br(),
								 shinyjs::disabled(bsButton("but_report_run",
								 									"Generate report")),
								 shinyBS::bsTooltip(id = "but_report_run",
								 						 title = paste0("Disabled until checked files uploaded."),
								 						 placement = "right"),
								 br(),
								 hr()
								 )
						),
					shinyjs::disabled(radioButtons("rad_report_tabs",
															 "Show report summary tabs",
															 choices = c("Yes", "No"),
															 selected = "No")),
					shinyBS::bsTooltip(id = "rad_report_tabs",
											 title = paste0("Disabled until report is created."),
											 placement = "right"),
					
					br(),
					br(),
					shinyjs::disabled(
						shiny::downloadButton("but_report_dload",
												"Download report and supplemental files")
						),
					shinyBS::bsTooltip(id = "but_report_dload",
											 title = paste0("Disabled until report is created."),
											 placement = "right"),
							)## mainPanel
				)## tabPanel
}## FUNCTION