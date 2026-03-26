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
								 p(strong("Target site:")),
								 div(class = "pill", textOutput("txt_rep_siteid_report")),
								 br(),
								 p(strong("Report delivery format user selection:")),
								 div(class = "pill", textOutput("txt_setup_format")),
								 # selectInput("report_format",
								 # 				"Report delivery format",
								 # 				choices = c("HTML", 
								 # 								"PDF"),
								 # 				selected = "HTML"),
								 br(),
								 p(tagList(
								 	strong("Explore watershed stressor data user selection: "),
								 	icon("info-circle", 
								 		  style = "color: #67c1f5", 
								 		  id="wsStressorInfo") |>
								 		bs_embed_popover(title = "Helpful Hints",
								 							  content = "To modify, change the exploreWSStressor parameter in '_CASTool_Metadata.xlsx'.",
								 							  placement = "right",
								 							  trigger = "hover"))),
								 div(class = "pill", textOutput("txt_setup_explore")),
								 br(),
								 shinyjs::disabled(bsButton("but_report_run",
								 									"Generate report")),
								 shinyBS::bsTooltip(id = "but_report_run",
								 						 title = paste0("Disabled until checked files uploaded."),
								 						 placement = "right"),
								 # p(em("Report should take approximately 5 minutes to generate for every biological community analyzed. A popup window will appear when the report is completed.")),
								 htmlOutput("txt_rep_time_est"),
								 br(),
								 p(em("Runtimes depend on the size of the input data, number of biological communities, number of stressors selected, number of responses selected, and whether the watershed stressor analyses are requested. CASTool results are generated faster using the R console program than the Shiny application.")),
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
					
					div(em(textOutput("report_tab_msg")), style = "color: #62c342"),
					
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