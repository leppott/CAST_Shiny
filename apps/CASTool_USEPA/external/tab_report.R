# Report

function() {
	tabPanel("Run Report",
				mainPanel(
					# doesn't work unless in include this line (even though it is in UI.R)
					useShinyjs(), 
					selectInput("report_format",
									"Report delivery format",
									choices = c("HTML", 
													"PDF"),
									selected = "HTML"),
					br(),
					shinyjs::disabled(bsButton("but_report_run",
														"Generate report")),
					shinyBS::bsTooltip(id = "but_report_run",
											 title = paste0("Disabled until checked files uploaded."),
											 placement = "right"),
					br(),
					shinyjs::disabled(radioButtons("rad_report_tabs",
															 "Show report summary tabs",
															 choices = c("Yes", "No"),
															 selected = "No")),
					shinyBS::bsTooltip(id = "rad_report_tabs",
											 title = paste0("Disabled until report is created."),
											 placement = "right"),
					
					br(),
					br(),
					shinyjs::disabled(bsButton("but_report_dload",
												"Download report and supplemental files")),
					shinyBS::bsTooltip(id = "but_report_dload",
											 title = paste0("Disabled until report is created."),
											 placement = "right"),
							)## mainPanel
				)## tabPanel
}## FUNCTION