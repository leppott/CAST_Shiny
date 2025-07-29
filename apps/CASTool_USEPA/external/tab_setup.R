# Set Up

function() {
	tabPanel("Set Up",
				mainPanel(
					fileInput('fn_input_setup_checked_uload', 
								 'Upload checked files',
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
					shinyBS::bsTooltip(id = "fn_input_setup_checked_uload",
											 title = paste0("Add some info here."),
											 placement = "right"),
					br(),
					selectInput("report_format",
									"Select target site",
									choices = c("A", 
													"B"),
									selected = ""),
					br(),
					radioButtons("rad_setup_explore",
					             "Explore watershed stressor data",
									 choices = c("Yes", "No"),
									 selected = "Yes"),
					br(),
					radioButtons("rad_setup_assign",
									 "Comparator assignment method",
									 choices = c("Abiotic clustering", "Custom"),
									 selected = "Abiotic clustering"),
					br(),
					p("appears with abiotic"),
					selectInput("report_nclusters",
									"Number of clusters",
									choices = c("Default", 
													"1",
													"2", 
													"3"),
									selected = "Default"),
					br(),
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
					br(),
					bsButton("but_setup_comp",
								"Get comparators"),
					p("text showing cluster method and number."),
					br(),
					p("if choose abiotic method get some plots"),
					br(),
					p("map")
							)## mainPanel
				)## tabPanel
}## FUNCTION