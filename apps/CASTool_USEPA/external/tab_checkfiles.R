# Check Files

function() {
	tabPanel("Check File Inputs",
				mainPanel(
					p("File set up"),
					checkboxGroupInput("check_check_comm",
									 "Biotic communities available",
									 choices = c("Algae", "Macroinvertebrates", "Fish"),
									 selected = NULL
									 ),
					checkboxGroupInput("rad_check_tol",
									 "Stressor specific tolerance values available",
									 choices = c("Algae", "Macroinvertebrates", "Fish"),
									 selected = NULL
					),
					radioButtons("rad_check_stress",
									 "Stressor types",
									 choices = c("Measured", "Modeled"),
									 selected = "Measured"
					),
					radioButtons("rad_check_outliers",
									 "Exclude outliers",
									 choices = c("Yes", "No"),
									 selected = "Yes"
					),
					fileInput('fn_input_check_uload', 
								 'Upload files',
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
					bsButton("but_check_check",
								"Check input files"),
					bsButton("but_check_mismatch",
								"Download mismatches"),
					bsButton("but_check_dload",
								"Download checked files"),
					
							)## mainPanel
				)## tabPanel
}## FUNCTION