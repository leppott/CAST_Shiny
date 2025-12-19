# Stressor Summaries

function() {
	tabPanel("Stressor Summaries",
				value = "tab_stresssumm",
				mainPanel(
					h2("Stressor Summaries"),
					# p("example images, not plots of data"),
					# imageOutput("img_stressors",
					# 				width = "50%",
					# 				height = "auto")
					# does't update when change contents of html
					# includeHTML(file.path("www",
					# 							 "RMD_HTML",
					# 							 "ShinyHTML_StressSumm.html"))
					htmlOutput("stresssum_html")
							)## mainPanel
				)## tabPanel
}## FUNCTION