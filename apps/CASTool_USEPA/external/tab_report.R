# Report

function() {
	tabPanel("Run Report",
				mainPanel(
					selectInput("report_format",
									"Select watershed variable",
									choices = c("PDF", 
													"HTML"),
									selected = "PDF"),
					br(),
					bsButton("but_report_run",
								"Generate report"),
					br(),
					p("Add a progress bar for the report"),
					p("Could do as a display in the lower right"),
					p("Uses code in server.R, shiny::withProgress()"),
					br(),
					shinyjs::disabled(bsButton("but_report_dload",
												"Download report and supplemental files")),
					shinyBS::bsTooltip(id = "but_report_dload",
											 title = paste0("Disabled until report is created."),
											 placement = "right"),
							)## mainPanel
				)## tabPanel
}## FUNCTION