# Set Up

function() {
	tabPanel("Set Up Tool",
				mainPanel(
					h2("Select target site and analysis parameters"),
					fileInput('fn_input_setup_checked_uload', 
								 'Upload checked zip file',
								 multiple = FALSE,
								 accept = c(
								 	'zip',
								 	'.zip')
					), ##fileInput
					# shinyBS::bsTooltip(id = "fn_input_setup_checked_uload",
					# 						 title = paste0("Add all files to a single zip file"),
					# 						 placement = "right"),
					# doesn't work on fileinput
					br(),
					fluidRow(
						# width = 12
						column(4,
								 selectInput("report_format",
								 				"Select target site",
								 				choices = c("A", 
								 								"B"),
								 				selected = ""),
								 radioButtons("rad_setup_explore",
								 				 "Explore watershed stressor data",
								 				 choices = c("Yes", "No"),
								 				 selected = "No")
								 ),
						column(4,
								 radioButtons("rad_setup_assign",
								 				 "Comparator assignment method",
								 				 choices = c("Abiotic clustering", "Custom"),
								 				 selected = "Abiotic clustering"),
								 
								 selectInput("report_nclusters",
								 				"Number of clusters",
								 				choices = c("Default", 
								 								"1",
								 								"2", 
								 								"3"),
								 				selected = "Default"),
								 fileInput('fn_input_setup_custom', 
								 			 'Upload custom clusters',
								 			 accept = c(
								 			 	'text/csv',
								 			 	'text/comma-separated-values',
								 			 	'text/tab-separated-values',
								 			 	'text/plain',
								 			 	'.csv',
								 			 	'.tab',
								 			 	'.tsv',
								 			 	'.txt')
								 ), ##fileInput
								 bsButton("but_setup_comp",
								 			"Get comparators")
								 ),
						column(4,
								 p("text showing cluster method and number."),
								 br(),
								 p("if choose abiotic method get some plots"),
								 br(),
								 p("map")
								 )
						)## fluidRow
					
					
							)## mainPanel
				)## tabPanel
}## FUNCTION