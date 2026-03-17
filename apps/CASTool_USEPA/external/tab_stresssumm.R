# Stressor Summaries

function() {
	tabPanel("Stressor Summaries",
				value = "tab_stresssumm",
				#mainPanel(
					h2("Stressor Summaries"),
				div(style = "margin-bottom: 20px", 
					 p(em("Figures may take up to a minute to display."))),
				
				p(strong("Target site, report:")),
				div(class = "pill", textOutput("txt_rep_siteid_stresssumm")),
				
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
					shinycssloaders::withSpinner(
						uiOutput("stresssum_frame")
					) ## withSpinner
					 
					
				#			)## mainPanel
				)## tabPanel
}## FUNCTION

