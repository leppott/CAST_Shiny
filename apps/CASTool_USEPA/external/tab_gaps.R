# Gaps

function() {
	tabPanel("Data Gaps",
				value = "tab_gaps",
				mainPanel(
					h2("Data Gaps"),
					p(paste0("Refer to the data gaps file for a summary of ",
							          "observations identified as outliers.")),
					p(strong("Target site, report:")),
					div(class = "pill", textOutput("txt_rep_siteid_gaps")),
					br(),
					shinycssloaders::withSpinner(
						DT::dataTableOutput("df_gaps_DT")
					)## withSpinner
							)## mainPanel
				)## tabPanel
}## FUNCTION