# Check Files

function() {
	tabPanel("Check File Inputs",
				mainPanel(
					
					# Load Files ----
					h2("Load Files"),
					fileInput("fn_input_check_uload", 
								 'Upload zipped folder',
								 multiple = FALSE,
								 accept = c(
								 	'zip',
								 	'.zip')
								) |>
						bs_embed_tooltip(title = "Maximum 300 MB"), ##fileInput
					
					# Define Scenario ----
					h2("Define Contents of Uploaded Files"),
					fluidRow(
						# width = 12
						column(3,
								 checkboxGroupInput("chk_check_comm",
								 						 "Biotic communities available",
								 						 choices = c("Algae", "Macroinvertebrates", "Fish"),
								 						 selected = NULL
								 ),
								 infoBox(title = "More info",
								 		  value = "",
								 		  icon = icon("info-circle", class = "clickable-icon"),
								 		  color = "light-blue",
								 		  fill = TRUE),),
						# column(3,
						# 		 radioButtons("rad_check_stress",
						# 		 				 "Stressor data available",
						# 		 				 choices = c("Measured", "Modeled"),
						# 		 				 selected = "Measured",
						# 		 )),
						column(3,
								 checkboxGroupInput("chk_check_stress",
								 						 "Stressor data available",
								 						 choices = c("Measured", "Modeled"),
								 						 selected = NULL
								 )),
						column(3,
								 checkboxGroupInput("chk_check_tol",
								 						 "Stressor-specific tolerance values available",
								 						 choices = c("Algae", "Macroinvertebrates", "Fish"),
								 						 selected = NULL
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
					h2("Identify Files"),
					p("Select the input file to be checked for each data type."),
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
								 				"Measured/Modeled stressor data",
								 				choices = NULL,
								 				multiple = FALSE),
								 selectInput("si_fn_input_check_mstress_md",
								 				"Measured/Modeled stressor metadata",
								 				choices = NULL,
								 				multiple = FALSE)),
						column(4,
								 selectInput("si_fn_input_check_bmi_met_d",
								 				"Macroinvertebrate/Algae/Fish metrics data",
								 				choices = NULL,
								 				multiple = FALSE),
								 selectInput("si_fn_input_check_bmi_met_md",
								 				"Macroinvertebrate/Algae/Fish metrics metadata",
								 				choices = NULL,
								 				multiple = FALSE),
								 selectInput("si_fn_input_check_bmi_cnt",
								 				"Macroinvertebrate/Algae/Fish count data",
								 				choices = NULL,
								 				multiple = FALSE),
								 selectInput("si_fn_input_check_bmi_tax",
								 				"Macroinvertebrate/Algae/Fish taxa list",
								 				choices = NULL,
								 				multiple = FALSE))
					), ## fluidRow
					
					h2("Check Files"),
					bsButton("but_check_check",
								"Check input files"),
		
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