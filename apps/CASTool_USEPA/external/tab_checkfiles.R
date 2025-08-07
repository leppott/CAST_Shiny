# Check Files

function() {
	tabPanel("Check File Inputs",
				mainPanel(
					
					# Load Files ----
					h2("Load Files"),
					fileInput("fn_input_check_uload", 
								 'Upload files',
								 multiple = FALSE,
								 accept = c(
								 	'zip',
								 	'.zip')
					), ##fileInput
					
					# Define Scenario ----
					h2("Define File Contents"),
					fluidRow(
						# width = 12
						column(3,
								 checkboxGroupInput("check_check_comm",
								 						 "Biotic communities available",
								 						 choices = c("Algae", "Macroinvertebrates", "Fish"),
								 						 selected = NULL
								 ),
								 infoBox(title = "More info",
								 		  value = "",
								 		  icon = icon("info-circle", class = "clickable-icon"),
								 		  color = "light-blue",
								 		  fill = TRUE),),
						column(3,
								 checkboxGroupInput("rad_check_tol",
								 						 "Stressor specific tolerance values available",
								 						 choices = c("Algae", "Macroinvertebrates", "Fish"),
								 						 selected = NULL
								 )),
						column(3,
								 radioButtons("rad_check_stress",
								 				 "Stressor types",
								 				 choices = c("Measured", "Modeled"),
								 				 selected = "Measured"
								 )),
						column(3,
								 radioButtons("rad_check_outliers",
								 				 "Exclude outliers",
								 				 choices = c("Yes", "No"),
								 				 selected = "Yes"
								 ))
					),## fluidRow
					
					
		
					
					# shinyBS::bsTooltip(id = "fn_input_check_uload",
					# 						 title = paste0("Add all files to a single zip file"),
					# 						 placement = "right"),
					# doesn't work on fileinput
					
					# Define Files----
					h2("Define Filenames"),
					p("Select below the input files to be checked for each data type."),
					fluidRow(
						# width = 12
						column(4,
								 selectInput("si_fn_input_check_cast_metadata",
								 				"CAST metadata",
								 				choices = NULL,
								 				multiple = FALSE),
								 selectInput("si_fn_input_check_sites",
								 				"Sites",
								 				choices = NULL,
								 				multiple = FALSE),
								 selectInput("si_fn_input_check_mstress_d",
								 				"Measured stress data",
								 				choices = NULL,
								 				multiple = FALSE),
								 selectInput("si_fn_input_check_mstress_md",
								 				"Measured stress metadata",
								 				choices = NULL,
								 				multiple = FALSE)),
						column(4,
								 selectInput("si_fn_input_check_bmi_met_d",
								 				"Benthicmacroinvertebrate metrics data",
								 				choices = NULL,
								 				multiple = FALSE),
								 selectInput("si_fn_input_check_bmi_met_md",
								 				"Benthicmacroinvertebrate metrics metadata",
								 				choices = NULL,
								 				multiple = FALSE),
								 selectInput("si_fn_input_check_bmi_cnt",
								 				"Benthicmacroinvertebrate counts",
								 				choices = NULL,
								 				multiple = FALSE),
								 selectInput("si_fn_input_check_bmi_tax",
								 				"Benthicmacroinvertebrate taxa list",
								 				choices = NULL,
								 				multiple = FALSE))
					), ## fluidRow
					
					h2("Check Files"),
					bsButton("but_check_check",
								"Check input files"),
					
					h2("File QC"),
					h3("Input File Check"),
					p("table of inputs and some checking"),
					h3("Input Files Matchups"),
					p("table of matches"),
					bsButton("but_check_mismatch",
								"Download mismatches"),
					bsButton("but_check_dload",
								"Download checked files"),
					bsTooltip(id = "but_check_dload",
								 title = paste0("Only visible after files pass check"),
								 placement = "right")

							)## mainPanel
				)## tabPanel
}## FUNCTION