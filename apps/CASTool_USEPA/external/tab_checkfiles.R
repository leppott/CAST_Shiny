# Check Files

function() {
	tabPanel("Upload and Check Data",
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
					use_bs_popover(),
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
						 p(em(paste0("Specific names for each data input file in the metadata file '",
								fn_default_check_input_cast_metadata,
								".'")))),
					# table for imported files
					# h4("Matching files"),
					# DT::dataTableOutput("df_import_files_DT"),
					h4(tagList("Missing files",
								  icon("info-circle", 
								  	  style = "color: #67c1f5", 
								  	  id="missingFilesInfo") |>
								  	bs_embed_popover(
								  		title = "Helpful Hints",
								  		content = "Box will be empty if no missing files are found. If a file is listed as missing, ensure that the file name listed in the metadata is correct and corresponds to a file included in the uploaded zipped folder.",
								  		placement = "right",
								  		trigger = "hover"))),
					p(em("Files included in the metadata but not present in the uploaded zipped folder.")),
					div(style = "margin-bottom: 20px", pre(textOutput("txt_import_files_missing"))),
					h4(tagList("Extra files",
								  icon("info-circle", 
								  	  style = "color: #67c1f5", 
								  	  id="extraFilesInfo") |>
								  	bs_embed_popover(
								  		title = "Helpful Hints",
								  		content = "Box will be empty if no extra files are found. If a file is listed as extra, ensure that it does not correspond to a required file input and that all file names are correctly listed in the metadata.",
								  		placement = "right",
								  		trigger = "hover"))),
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
									 	strong("Exclude outliers user selection: "),
									 	icon("info-circle", 
									 		  style = "color: #67c1f5", 
									 		  id="outlierInfo") |>
									 		bs_embed_popover(
									 			title = "Helpful Hints",
									 			content = "To modify, change the removeOutliers parameter in '_CASTool_MetaData.xlsx'.",
									 			placement = "right",
									 			trigger = "hover"))),
									 	
									 # bsPopover(id="outlierInfo", 
									 # 			 title = HTML("<b>Helpful Hints</b>"), 
									 # 			 content = HTML("To modify, change the removeOutliers parameter in _CASTool_Metadata.xlsx."),
									 # 			 placement = "right", 
									 # 			 trigger = "hover"),
									 
									 
								 div(class = "pill", textOutput("txt_check_outliers"))
								 )
								 # radioButtons("rad_check_outliers",
								 # 				 "Exclude outliers",
								 # 				 choices = c("Yes", "No"),
								 # 				 selected = "Yes"
								 # )
								 ),

					h2("Check files"),
					shinyjs::disabled(bsButton("but_check_check",
														"Check input files")),
					bsTooltip(id = "but_check_check",
								 title = paste0("Enabled after files uploaded"),
								 placement = "right"),
					div(style = "width: 50%", p(em("Generate and review check file tables to ensure that input files contain expected columns with expected datatypes and paired input data files contain expected matchups."))),
		
					#h4("Input File Check"),
					#
					h4("Summary of file inputs"),
					DT::dataTableOutput("df_check_qctable1_DT"),
					#
					h4("Relational integrity"),
					DT::dataTableOutput("df_check_qctable2_DT"),
					
					h4(tagList(
						"Download file check tables",
						icon("info-circle", 
							  style = "color: #67c1f5", 
							  id="checkFileTabInfo") |>
							bs_embed_popover(title = "Helpful Hints",
												  content = "Downloading the file check tables is not required, but allows users to reference identified issues in the input data after the app has timed out.",
												  placement = "right",
												  trigger = "hover")
						)),
					# p("Download file check tables"),
					shinyjs::disabled(shiny::downloadButton(
						"but_check_dload_qctables",
						"Download file check tables")),
					bsTooltip(id = "but_check_dload_qctables",
								 title = paste0("Only enabled after files checked."),
								 placement = "right"),
					div(style = "width: 50%", 
							 p(em("(Optional) Download a zipped folder with the Summary of file inputs and Relational integrity table."))),
					
					h2(tagList("Download checked data",
								  icon("info-circle", 
								  	  style = "color: #67c1f5", 
								  	  id="dwnldCheckInfo") |>
								  	bs_embed_popover(title = "Helpful Hints",
								  						  content = "The zipped folder downloaded here is required for the Set Up Tool tab. Users should not need to view files in the checked inputs folder and may skip the Upload and Check Data tab for subsequent CASTool runs by uploading a previously saved checked inputs folder if the input data have not changed.",
								  						  placement = "right",
								  						  trigger = "hover"))),
					div(shinyjs::disabled(shiny::downloadButton(
						"but_check_dload_rds",
						"Download checked data"))),
					bsTooltip(id = "but_check_dload_rds",
								 title = paste0("Only enabled after files checked"),
								 placement = "right"),
					div(style = "width: 50%", 
							 p(em("Download a zipped folder with all checked data files. This folder is required to run the next step of the CASTool."))),
					
					div(em(textOutput("but_check_msg")), style = "color: #62c342")

							)## mainPanel
				)## tabPanel
}## FUNCTION
