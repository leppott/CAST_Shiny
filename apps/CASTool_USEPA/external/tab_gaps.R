# Gaps

function() {
	tabPanel("Data Gaps",
				value = "tab_gaps",
				mainPanel(h1("Data Gaps"),
							 p(paste0("Refer to the data gaps file for a summary of ",
							          "oberservations identified as outliers.")),
							 p("Stressors eliminated due to insufficient number of samples")
					
							)## mainPanel
				)## tabPanel
}## FUNCTION