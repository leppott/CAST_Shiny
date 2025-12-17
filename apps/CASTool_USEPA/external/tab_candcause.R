# Check Files

function() {
	tabPanel("Candidate Causes",
				value = "tab_candcause",
				mainPanel(
					p(strong("User-specified thresholds")),
					textOutput("txt_candcause_thresh_ph"),
					textOutput("txt_candcause_thresh_do"),
					br(),
					p(strong("Stressor(s) initially evaluated")),
					p("No current output."),
					br(),
					p(strong(paste0("Stressor(s) eliminated by comparison of ",
										 "\ntarget and comparator sample values"))),
					DT::dataTableOutput("df_candcause_DT")
					
					)## mainPanel
				)## tabPanel
}## FUNCTION