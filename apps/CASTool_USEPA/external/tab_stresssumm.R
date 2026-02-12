# Stressor Summaries

function() {
	tabPanel("Stressor Summaries",
				value = "tab_stresssumm",
				#mainPanel(
					h2("Stressor Summaries"),
				div(style = "margin-bottom: 20px", 
					 p(em("Figures may take up to a minute to display."))),
					# p("example images, not plots of data"),
					# imageOutput("img_stressors",
					# 				width = "50%",
					# 				height = "auto")
					# doesn't update when change contents of html
					# includeHTML(file.path("www",
					# 							 "RMD_HTML",
					# 							 "ShinyHTML_StressSumm.html"))
					# file not found
					# tags$iframe(
					# 	src = file.path("www",
					# 						 "RMD_HTML",
					# 						 "ShinyHTML_StressSumm.html"))
					# app crashes
					# includeHTML("stresssum_html") 
					# works with both fragment (no toc) and document
					
					#htmlOutput("stresssum_html") #LCN commentedout
					uiOutput("stresssum_frame")
					 
					
				#			)## mainPanel
				)## tabPanel
}## FUNCTION

