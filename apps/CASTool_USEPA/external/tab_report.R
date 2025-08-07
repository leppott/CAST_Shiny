# Report

function() {
	tabPanel("Run Report",
				mainPanel(
					selectInput("report_format",
									"Report delivery format",
									choices = c("PDF", 
													"HTML"),
									selected = "PDF"),
					br(),
					bsButton("but_report_run",
								"Generate report"),
					br(),
					radioButtons("rad_report_tabs",
									 "Show Report Summary Tabs",
									 choices = c("Yes", "No"),
									 selected = "No"),
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