# Check Files

function() {
	tabPanel("Candidate Causes",
				mainPanel(
					p("User-specified thresholds"),
					p("Stressor(s) initially evaluated"),
					p(paste0("Stressor(s) eliminated by comparison of ",
					         "\ntarget and comparator sample values"))
					
					)## mainPanel
				)## tabPanel
}## FUNCTION