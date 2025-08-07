# Stressor Summaries

function() {
	tabPanel("Stressor Summaries",
				value = "tab_stresssumm",
				mainPanel(
					p("example images, not plots of data"),
					imageOutput("img_stressors",
									width = "50%",
									height = "auto")
							)## mainPanel
				)## tabPanel
}## FUNCTION