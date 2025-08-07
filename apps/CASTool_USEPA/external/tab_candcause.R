# Check Files

function() {
	tabPanel("Candidate Causes",
				value = "tab_candcause",
				mainPanel(
					p("User-specified thresholds"),
					p("WHAT FILE?"),
					p("Stressor(s) initially evaluated"),
					p("WHAT FILE?"),
					p(paste0("Stressor(s) eliminated by comparison of ",
					         "\ntarget and comparator sample values")),
					p("WHAT FILE?")
					
					)## mainPanel
				)## tabPanel
}## FUNCTION