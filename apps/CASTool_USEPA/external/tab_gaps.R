# Gaps

function() {
	tabPanel("Data Gaps",
				value = "tab_gaps",
				mainPanel(
					h2("Data Gaps"),
					p(paste0("Refer to the data gaps file for a summary of ",
							          "observations identified as outliers.")),
					DT::dataTableOutput("df_gaps_DT")
							)## mainPanel
				)## tabPanel
}## FUNCTION