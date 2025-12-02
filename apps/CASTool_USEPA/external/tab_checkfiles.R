# Check Files

function() {
	tabPanel("Check File Inputs",
				mainPanel(
					useShinyjs(),
					# Load Files ----
					h2("Load Files"),
					fileInput("fn_input_check_uload", 
								 'Upload zipped folder',
								 multiple = FALSE,
								 accept = c(
								 	'zip',
								 	'.zip')
								) |>
						bs_embed_tooltip(title = "Maximum 300 MB",
											  placement = "right"), ##fileInput
					
					# shinyBS::bsTooltip(id = "fn_input_check_uload",
					# 						 title = paste0("Add all files to a single zip file"),
					# 						 placement = "right"),
					# doesn't work on fileinput
					
					# Define Files----
					h2("Identify Files"),
					p(paste0("Files for each data type are specified in the metadata file '",
								fn_default_check_input_cast_metadata,
								"'")),
					# table for imported files
					h4("Matching Files"),
					DT::dataTableOutput("df_import_files_DT"),
					h4("Missing Files"),
					p("Any files listed in metadata but not present in the loaded files are displayed below."),
					pre(textOutput("txt_import_files_missing")),
					h4("Extra Files"),
					p("Any files in the loaded files but not listed in the metadata are displayed below."),
					pre(textOutput("txt_import_files_extra")),
					  
					# Define Scenario ----
					h2("Show Contents of Uploaded Files"),
					p("**variables from metadata file**"),
					p("**not selectable, skeleton code runs on all available data**"),
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
					
					h2("Check Files"),
					shinyjs::disabled(bsButton("but_check_check",
														"Check input files")),
					bsTooltip(id = "but_check_check",
								 title = paste0("Enabled after files uploaded"),
								 placement = "right"),
		
					h3("Input File Check"),
					#
					h4("Summary of file inputs"),
					DT::dataTableOutput("df_check_qctable1_DT"),
					#
					h4("Relational integrity"),
					DT::dataTableOutput("df_check_qctable2_DT"),
					
					h3("Input Files Matchups"),
					p("Download file evaluation qc tables"),
					shinyjs::disabled(shiny::downloadButton(
						"but_check_dload_qctables",
						"Download file check tables")),
					bsTooltip(id = "but_check_dload_qctables",
								 title = paste0("Only enabled after files checked."),
								 placement = "right"),
					
					h3("Checked Data"),
					shinyjs::disabled(shiny::downloadButton(
						"but_check_dload_rds",
						"Download checked files")),
					bsTooltip(id = "but_check_dload_rds",
								 title = paste0("Only enabled after files checked"),
								 placement = "right")

							)## mainPanel
				)## tabPanel
}## FUNCTION
