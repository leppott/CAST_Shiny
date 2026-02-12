# Check Files

function() {
	tabPanel("Check File Inputs",
				tags$head(tags$style(HTML("
				  .pill {
				    background: #f5f5f5;
				    padding: 8px 12px;
				    border-radius: 4px;
				    margin-bottom: 10px;
				  }
						 "))),
				
				mainPanel(
					useShinyjs(),
					#h2("Template"),
					#p("download template as a zip file."),
					# shiny::a(href = paste0(url_github_castshiny,
					# 							  "/",
					# 							  "CASTool_Templates.zip"),
					# 			target = "_blank",
					# 			class = "btn bnh-primary",
					# 			download = "CASTool_Templates.zip",
					# 			"Download Templates (zip)"),
					# Load Files ----
					h2("Load files"),
					fileInput("fn_input_check_uload", 
								 'Upload zipped folder with input data files',
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
					h2("Identify files"),
					div(style = "margin-bottom: 20px", 
						 p(em(paste0("Names for each data input file are specified in the metadata file '",
								fn_default_check_input_cast_metadata,
								".'")))),
					# table for imported files
					h4("Matching files"),
					DT::dataTableOutput("df_import_files_DT"),
					h4("Missing files"),
					p(em("Files included in the metadata but not present in the uploaded zipped folder.")),
					div(style = "margin-bottom: 20px", pre(textOutput("txt_import_files_missing"))),
					h4("Extra files"),
					p(em("Files in the uploaded zipped folder but not included in the metadata.")),
					div(style = "margin-bottom: 20px", pre(textOutput("txt_import_files_extra"))),
					  
					# Define Scenario ----
					h4("Contents of uploaded files"),
					# p("**variables from metadata file**"),
					#fluidRow(
						# width = 12
						fluidRow(
							column(6,
								 p(strong("Biotic communities available: ")),
								 div(class = "pill", textOutput("txt_chk_check_comm"))),
								 # checkboxGroupInput("chk_check_comm",
								 # 						 "Biotic communities available",
								 # 						 choices = choices_chk_check_comm,
								 # 						 selected = NULL
								 # ),
								 # infoBox(title = "More info",
								 # 		  value = "",
								 # 		  icon = icon("info-circle", class = "clickable-icon"),
								 # 		  color = "light-blue",
								 # 		  fill = TRUE),
								 ),
						# column(3,
						# 		 radioButtons("rad_check_stress",
						# 		 				 "Stressor data available",
						# 		 				 choices = c("Measured", "Modeled"),
						# 		 				 selected = "Measured",
						# 		 )),
						fluidRow(
							column(6,
								 p(strong("Stressor data available: ")),
								 div(class = "pill", textOutput("txt_chk_check_stress"))),
								 # checkboxGroupInput("chk_check_stress",
								 # 						 "Stressor data available",
								 # 						 choices = choices_chk_check_stress,
								 # 						 selected = NULL
								 # )
								 ),
						fluidRow(
							column(6,
								 p(strong("Stressor-specific tolerance values available: ")),
								 div(class = "pill", textOutput("txt_chk_check_tol"))),
								 # checkboxGroupInput("chk_check_tol",
								 # 						 "Stressor-specific tolerance values available",
								 # 						 choices = choices_chk_check_tol,
								 # 						 selected = NULL
								 # )
								 ),
						fluidRow(
							column(6,
									 p(tagList(
									 	strong("Exclude outliers: "),
									 	icon("info-circle", style = "color: #2fa4e7", id="outlierInfo",
									 	))),
									 bsPopover(id="outlierInfo", title = HTML("<b>Helpful Hints</b>"), content = HTML("To modify, change the removeOutliers parameter in _CASTool_Metadata.xlsx."),
									 			 placement = "right", trigger = "hover"),
								 div(class = "pill", textOutput("txt_check_outliers"))
								 )
								 # radioButtons("rad_check_outliers",
								 # 				 "Exclude outliers",
								 # 				 choices = c("Yes", "No"),
								 # 				 selected = "Yes"
								 # )
								 ),
					#),## fluidRow
					
					# 
					h2("Check files"),
					shinyjs::disabled(bsButton("but_check_check",
														"Check input files")),
					bsTooltip(id = "but_check_check",
								 title = paste0("Enabled after files uploaded"),
								 placement = "right"),
					div(style = "width: 50%", p(em("Generate tables checking that input files contain expected columns with expected datatypes and evaluating match ups between paired input data files."))),
		
					#h4("Input File Check"),
					#
					h4("Summary of file inputs"),
					DT::dataTableOutput("df_check_qctable1_DT"),
					#
					h4("Relational integrity"),
					DT::dataTableOutput("df_check_qctable2_DT"),
					
					h4("Download file check tables"),
					# p("Download file check tables"),
					shinyjs::disabled(shiny::downloadButton(
						"but_check_dload_qctables",
						"Download file check tables")),
					bsTooltip(id = "but_check_dload_qctables",
								 title = paste0("Only enabled after files checked."),
								 placement = "right"),
					div(style = "width: 50%", 
							 p(em("Download a zipped folder with the Summary of file inputs and Relational integrity table."))),
					
					h2("Download checked data"),
					div(shinyjs::disabled(shiny::downloadButton(
						"but_check_dload_rds",
						"Download checked data"))),
					bsTooltip(id = "but_check_dload_rds",
								 title = paste0("Only enabled after files checked"),
								 placement = "right"),
					div(style = "width: 50%", 
							 p(em("Download a zipped folder with all checked data files. This folder is required to run the next step of the CASTool.")))

							)## mainPanel
				)## tabPanel
}## FUNCTION
