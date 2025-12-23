#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Define server logic required to draw a histogram
function(input, output, session) {

	# Sections headers by tab

## ABOUT ----
	
# CHECK ----
	
	# file_watch <- reactive({
	# 	# trigger for df_import()
	# 	input$fn_input_check_uload
	# 	#paste(input$fn_input, input$but_radio_load)
	# })## file_watch
	
	## Import, Files, Input ----
	
	# Reactive value to store zip file contents
	zip_contents_input <- reactiveVal(NULL)
	check_files_fails <- reactiveVal(NULL)
	
	observeEvent(input$fn_input_check_uload, {
	# fn_input_check <- eventReactive(file_watch(), {

		shiny::withProgress({
			### 00, Initialize----
			prog_detail <- "Import New Files..."
			message(paste0("\n", prog_detail))
			# Number of increments
			prog_n <- 4
			prog_sleep <- 0.25
			
			### 01, Import ----
			prog_detail <- "Import Data, User"
			message(paste0("\n", prog_detail))
			# Increment the progress bar, and update the detail text.
			incProgress(1/prog_n, detail = prog_detail)
			Sys.sleep(prog_sleep)
			
			# Ensure a file is uploaded
			# req(input$file_upload)  
			
			inFile <- input$fn_input_check_uload
			
			if (is.null(inFile)) {
				return(NULL)
			}
			
			# Define file
			fn_inFile <- inFile$datapath
			
			
			### 02, Clean Directory ----
			prog_detail <- "Remove Old Files"
			message(paste0("\n", prog_detail))
			# Increment the progress bar, and update the detail text.
			incProgress(1/prog_n, detail = prog_detail)
			Sys.sleep(prog_sleep)
			
			# Clean Directory
			clean_dir(file.path(dn_data, dn_import))
			
			### 03, Unzip ----
			prog_detail <- "Unzip Files"
			message(paste0("\n", prog_detail))
			# Increment the progress bar, and update the detail text.
			incProgress(1/prog_n, detail = prog_detail)
			Sys.sleep(prog_sleep)
			
			# Testing
			# print(file.exists(fn_inFile))
			# print(getwd())
			# print(dir.exists(file.path(dn_data, dn_import)))
			
			# Unzip (remove any zip file directories)
			zip::unzip(fn_inFile,
							 overwrite = TRUE,
							 exdir = normalizePath(file.path(dn_data, dn_import)),
							 junkpaths = TRUE)
			
			### 04, Catalog ----
			prog_detail <- "Catalog Files"
			message(paste0("\n", prog_detail))
			# Increment the progress bar, and update the detail text.
			incProgress(1/prog_n, detail = prog_detail)
			Sys.sleep(prog_sleep)
			
			# List Files
			fn_import <- sort(list.files(file.path(dn_data, dn_import),
															recursive = TRUE,
															full.names = FALSE))
			# add blank so 1st file isn't auto-selected
			import_filenames <- c("", fn_import)
			
		
			# Shiny Alert if metadata file missing
			
			
			
			# cat(import_filenames)
			
			### button, enable, calc 
			# shinyjs::enable("b_calc_taxatrans")
			# shinyjs::enable("b_markexcl_run")
			# shinyjs::enable("b_subsample_run")
			# shinyjs::enable("b_calcmet_run")
			# shinyjs::enable("b_taxamaps_run")
			
			# shinyjs::enable("b_calc_indexclass")
			# shinyjs::enable("b_calc_indexclassparam")
			# shinyjs::enable("b_calc_bcg")
			# shinyjs::enable("b_calc_ibi")
			# shinyjs::enable("b_calc_met_therm")
			# shinyjs::enable("b_calc_modtherm")
			# shinyjs::enable("b_calc_mtti")
			# shinyjs::enable("b_calc_bdi")
			
			
			# # Update Select Input for zip contents
			# updateSelectInput(session,
			# 						"si_fn_input_check_cast_metadata",
			# 						choices = import_filenames,
			# 						selected = fn_default_check_input_cast_metadata)
			# updateSelectInput(session,
			# 						"si_fn_input_check_sites",
			# 						choices = import_filenames,
			# 						selected = NULL)
			# updateSelectInput(session,
			# 						"si_fn_input_check_mstress_d",
			# 						choices = import_filenames,
			# 						selected = NULL)
			# updateSelectInput(session,
			# 						"si_fn_input_check_mstress_md",
			# 						choices = import_filenames,
			# 						selected = NULL)
			# updateSelectInput(session,
			# 						"si_fn_input_check_cast_metadata",
			# 						choices = import_filenames,
			# 						selected = NULL)
			# updateSelectInput(session,
			# 						"si_fn_input_check_bmi_met_d",
			# 						choices = import_filenames,
			# 						selected = NULL)
			# updateSelectInput(session,
			# 						"si_fn_input_check_bmi_met_md",
			# 						choices = import_filenames,
			# 						selected = NULL)
			# updateSelectInput(session,
			# 						"si_fn_input_check_bmi_cnt",
			# 						choices = import_filenames,
			# 						selected = NULL)
			# updateSelectInput(session,
			# 						"si_fn_input_check_bmi_tax",
			# 						choices = import_filenames,
			# 						selected = NULL)
			
			
			# return list of files
			zip_contents_input <- import_filenames
			
			### 05, Info Pop Up ----
			msg <- paste("Import of files is complete.\n",
							 "'Verify' all files are included.",
							 "Then 'Check' the files.",
							 sep = "\n")
			shinyalert::shinyalert(title = "Import Files",
										  text = msg,
										  type = "info",
										  closeOnEsc = TRUE,
										  closeOnClickOutside = TRUE)
			
			### 06, Update UI ----
			shinyjs::enable("but_check_check")
			
		},
		message = "Load Files")## withProgress
	})## observeEvent
	
	## Import, Reactives, Data Types ----
	react_chk_check_comm   <- reactiveVal("..No file uploaded..")
	react_chk_check_stress <- reactiveVal("..No file uploaded..")
	react_chk_check_tol    <- reactiveVal("..No file uploaded..")
	react_check_outliers   <- reactiveVal("..No file uploaded..")
	
	output$txt_chk_check_comm  <- renderText({
		react_chk_check_comm()
	})
	
	output$txt_chk_check_stress <- renderText({
		react_chk_check_stress()
	})
	
	output$txt_chk_check_tol <- renderText({
		react_chk_check_tol()
	})
	
	output$txt_check_outliers <- renderText({
		react_check_outliers()
	})
	
	## Import, Files, present ----
	output$df_import_files_DT <- DT::renderDT({

		inFile <- input$fn_input_check_uload
		
		# Blank if no data
		if (is.null(inFile)) {
			return(NULL)
		} ## IF ~ is.null(inFile)
		
		
		# List Files
		fn_import <- sort(list.files(file.path(dn_data, dn_import),
											  recursive = TRUE,
											  full.names = FALSE))
		
		# User, CASTool MetaData
		df_user_metadata <- readxl::read_excel(
			file.path(dn_data, 
						 dn_import, 
						 fn_default_check_input_cast_metadata))
		# "_CASTool_Metadata.xlsx"
		
		# User, Region
		df_user_region <- df_user_metadata |>
			dplyr::filter(Variable == "region") |>
			dplyr::pull(Value)
		
		# User, Files
		df_user_files <- df_user_metadata |>
			# filter for filename
			dplyr::filter(Domain == "filename") |>
			# select only some columns
			dplyr::select(Variable,
							  Definition,
							  Required,
							  Value) |>
			# Populate if file present
			dplyr::mutate(Present = 
							  	dplyr::case_when(is.na(Value) ~ NA,
							  						  Value %in% fn_import ~ TRUE,
							  						  .default = FALSE)) 
		
		df_user_files_present <- df_user_files |>
			# only TRUE
			dplyr::filter(Present == TRUE) |> 
			dplyr::pull(Value)
		
		# user files not listed in MetaData
		df_user_files_extra <- fn_import[!fn_import %in% 
														c(fn_default_check_input_cast_metadata,
														  df_user_files_present)]

		# Populate check boxes for data files present
		df_user_files_present_fn <- df_user_files |>
			# only TRUE
			dplyr::filter(Present == TRUE) |> 
			dplyr::pull(Variable)

		user_files_alg <- "fn.alg.metrics" %in% df_user_files_present_fn
		user_files_bmi <- "fn.bmi.metrics" %in% df_user_files_present_fn
		user_files_fish <- "fn.fish.metrics" %in% df_user_files_present_fn
		user_files_meas <- "fn.meas.info" %in% df_user_files_present_fn
		user_files_model <- "fn.model.info" %in% df_user_files_present_fn
		
		df_user_outliers <- df_user_metadata |>
			dplyr::filter(Variable == "removeOutliers") |>
			dplyr::pull(Value) 

		choices_chk_check_comm_sel <- choices_chk_check_comm[c(user_files_alg,
																				 user_files_bmi,
																				 user_files_fish)]
		choices_chk_check_stress_sel <- choices_chk_check_stress[c(user_files_meas,
																					  user_files_model)]
		choices_chk_check_tol_sel <- choices_chk_check_comm_sel
		
		# update boxes
		shiny::updateCheckboxGroupInput(session,
												  "chk_check_comm",
												  selected = choices_chk_check_comm_sel)
		shiny::updateCheckboxGroupInput(session,
												  "chk_check_stress",
												  selected = choices_chk_check_stress_sel)
		shiny::updateCheckboxGroupInput(session,
												  "chk_check_tol",
												  selected = choices_chk_check_tol_sel)
		
		### update Reactives----
		react_chk_check_comm(choices_chk_check_comm[c(user_files_alg,
																			user_files_bmi,
																			user_files_fish)])
		react_chk_check_stress(choices_chk_check_stress[c(user_files_meas,
																			  user_files_model)])
		react_chk_check_tol(choices_chk_check_comm_sel)
		react_check_outliers(df_user_outliers)
		
		# Show table
		# Show text if any "missing"files
		# Show text if any "extra" files

		DT::datatable(df_user_files,
						  filter = "top",
						  caption = "Loaded files present in metadata.",
						  options = list(
						  	scrollX=TRUE,
						  	lengthMenu = c(5, 10, 25, 50, 100),
						  	autoWidth = TRUE)
						  ) |>
			DT::formatStyle("Present",
								 target = "row",
								 backgroundColor = DT::styleEqual(
								 	c(TRUE, FALSE),
								 	c(color_good, color_bad)
								 	)
								 ) |>
			# bold cells
			DT::formatStyle("Present",
								 target = "cell",
								 fontWeight = DT::styleEqual(
								 	c(TRUE, FALSE),
								 	c("normal", "bold")))
		
		
		# return(df_user_files)

	}##expression

	)## df_data_DT
	
	
	## Import, Files, missing ----
	output$txt_import_files_missing <- renderText({
		
		inFile <- input$fn_input_check_uload
		
		# Blank if no data
		if (is.null(inFile)) {
			return(NULL)
		} ## IF ~ is.null(inFile)
		
		
		# List Files
		fn_import <- sort(list.files(file.path(dn_data, dn_import),
											  recursive = TRUE,
											  full.names = FALSE))
		
		# User, CASTool MetaData
		df_user_metadata <- readxl::read_excel(
			file.path(dn_data, 
						 dn_import, 
						 fn_default_check_input_cast_metadata))
		# "_CASTool_Metadata.xlsx"
		
		# User, Region
		df_user_region <- df_user_metadata |>
			dplyr::filter(Variable == "region") |>
			dplyr::pull(Value)
		
		# User, Files
		df_user_files <- df_user_metadata |>
			# filter for filename
			dplyr::filter(Domain == "filename") |>
			# select only some columns
			dplyr::select(Variable,
							  Definition,
							  Required,
							  Value) |>
			# Populate if file present
			dplyr::mutate(Present = 
							  	dplyr::case_when(is.na(Value) ~ NA,
							  						  Value %in% fn_import ~ TRUE,
							  						  .default = FALSE)) 
		
		df_user_files_present <- df_user_files |>
			# only TRUE
			dplyr::filter(Present == TRUE) |> 
			dplyr::pull(Value)
		
		# user files not listed in MetaData
		df_user_files_extra <- fn_import[!fn_import %in% 
														c(fn_default_check_input_cast_metadata,
														  df_user_files_present)]
		
		df_user_files_missing <- df_user_files |>
			# only TRUE
			dplyr::filter(Present == FALSE) |> 
			dplyr::pull(Value) |>
			paste(collapse = "\n")
		
		# QC trigger
		if (df_user_files_missing == "") {
			check_files_fails(FALSE)
		} else {
			check_files_fails(TRUE)
		}# IF ~ df_user_files_missing
		
		return(df_user_files_missing)
	})
	
	## Import, Files, extra ----
	output$txt_import_files_extra <- renderText({
		
		inFile <- input$fn_input_check_uload
		
		# Blank if no data
		if (is.null(inFile)) {
			return(NULL)
		} ## IF ~ is.null(inFile)
		
		
		# List Files
		fn_import <- sort(list.files(file.path(dn_data, dn_import),
											  recursive = TRUE,
											  full.names = FALSE))
		
		# User, CASTool MetaData
		df_user_metadata <- readxl::read_excel(
			file.path(dn_data, 
						 dn_import, 
						 fn_default_check_input_cast_metadata))
		# "_CASTool_Metadata.xlsx"
		
		# User, Region
		df_user_region <- df_user_metadata |>
			dplyr::filter(Variable == "region") |>
			dplyr::pull(Value)
		
		# User, Files
		df_user_files <- df_user_metadata |>
			# filter for filename
			dplyr::filter(Domain == "filename") |>
			# select only some columns
			dplyr::select(Variable,
							  Definition,
							  Required,
							  Value) |>
			# Populate if file present
			dplyr::mutate(Present = 
							  	dplyr::case_when(is.na(Value) ~ NA,
							  						  Value %in% fn_import ~ TRUE,
							  						  .default = FALSE)) 

		df_user_files_present <- df_user_files |>
			# only TRUE
			dplyr::filter(Present == TRUE) |> 
			dplyr::pull(Value)
		
		# user files not listed in MetaData
		df_user_files_extra <- paste(fn_import[!fn_import %in% 
														c(fn_default_check_input_cast_metadata,
														  df_user_files_present)],
											  collapse = "\n")
		
		return(df_user_files_extra)
	})
	
	## Check Files ----
	# Reactive values / flags
	## zip file contents
	zip_contents_checked <- reactiveVal(NULL)
	## boo to indicate check qctables files saved
	check_qctable_1_save <- reactiveVal(FALSE)
	check_qctable_2_save <- reactiveVal(FALSE)
	
	observeEvent(input$but_check_check, {
		shiny::withProgress({

			### 00, Intialize, QC ----
			prog_detail <- "Check Files..."
			message(paste0("\n", prog_detail))
			# Number of increments
			prog_n <- 5
			prog_sleep <- 0.25
			
			# QC, missing files
			# trigger created when save table
			# req(check_files_fails())
			
			if(check_files_fails()) {
				msg <- paste("Some files set as required in metadata are missing.",
								 "\n",
								 "Review 'Matching Files' table for rows marked as FALSE and",
								 " the 'Missing Files' box.  Both are in the 'Identify Files' section on this page.",
								 "\n",
								 "Add the files to your zip file or modify the metadata file.",
								 sep = "\n")
				shinyalert::shinyalert(title = "Check Files",
											  text = msg,
											  type = "error",
											  closeOnEsc = TRUE,
											  closeOnClickOutside = TRUE)
				validate(msg)
			}## IF ~ check_files_fails
			
			
			### 01, Setup ----
			prog_detail <- "Create QC Tables One and Two"
			message(paste0("\n", prog_detail))
			# Increment the progress bar, and update the detail text.
			incProgress(1/prog_n, detail = prog_detail)
			Sys.sleep(prog_sleep)
			
			# define checkInputs parameters
			in.dir <- file.path(getwd(), dn_data, dn_import)
			out.dir <- file.path(getwd(), dn_data, dn_checked)
			fn.inputcheck <- system.file("extdata", 
												  "CASTool_InputCheck.xlsx",
												  package = "CASTfxn")
			# get from metadata file
			# df_targets <- data.frame(TargetSiteID = input$si_checked_sites_targ)
			path_meta <- file.path(in.dir, fn_default_check_input_cast_metadata)
			df_meta <- readxl::read_excel(path_meta,
													sheet = "Sheet1",
													skip = 0)
			fn_targets <- df_meta |>
				dplyr::filter(Variable == "fn.targets") |>
				dplyr::pull(Value)

			ext_fn_targets <- tolower(tools::file_ext(fn_targets))
			if (ext_fn_targets == "csv") {
				df_targets <- read.csv(file.path(in.dir, fn_targets))
			} else if (ext_fn_targets == "xlsx") {
				df_targets <- as.data.frame(
					readxl::read_excel(file.path(in.dir, fn_targets),
															sheet = 1, #first sheet
															skip = 0))
			} else {
				msg <- paste("Targets file should be '.csv' or '.xlsx'.",
								 "If '.xlsx' the data should be on the first worksheet.",
								 sep = "\n")
				shinyalert::shinyalert(title = "Check Files",
											  text = msg,
											  type = "error",
											  closeOnEsc = TRUE,
											  closeOnClickOutside = TRUE)
				validate(msg)
			}## IF ~ ext_fn_targets
			# region
			region <- df_meta |>
				dplyr::filter(Variable == "region") |>
				dplyr::pull(Value)
			
			### 02, Check, Tables ----
			prog_detail <- "Create QC Tables One and Two"
			message(paste0("\n", prog_detail))
			# Increment the progress bar, and update the detail text.
			incProgress(1/prog_n, detail = prog_detail)
			Sys.sleep(prog_sleep)
		
			list.Tables <- checkInputs(dir.uploaded = in.dir,
												dir.out = out.dir,
												fn.inputcheck = fn.inputcheck)
			# 20251216, no longer have df_targets as input
												# df_targets = df_targets)
			
			###03, Save, Tables----
			prog_detail <- "Create Tables One and Two"
			message(paste0("\n", prog_detail))
			# Increment the progress bar, and update the detail text.
			incProgress(1/prog_n, detail = prog_detail)
			Sys.sleep(prog_sleep)
		
			TableOne    <- list.Tables$TableOne
			write.table(TableOne, 
							file.path(out.dir, 
										 region, 
										 dn_checked_sk, 
										 "TableOne.tab"),
							sep = "\t", 
							col.names = TRUE, 
							row.names = FALSE, 
							append = FALSE)
			
			check_qctable_1_save(TRUE) # trigger for table
			
			TableTwo    <- list.Tables$TableTwo
			write.table(TableTwo, 
							file.path(out.dir, 
										 region, 
										 dn_checked_sk,
										 "TableTwo.tab"),
							sep = "\t", 
							col.names = TRUE, 
							row.names = FALSE, 
							append = FALSE)
			
			check_qctable_2_save(TRUE) # trigger for table
			
			rm(list.Tables, TableOne, TableTwo)
		
			### 04, Create Zip ----
			prog_detail <- "Create ZIP"
			message(paste0("\n", prog_detail))
			# Increment the progress bar, and update the detail text.
			incProgress(1/prog_n, detail = prog_detail)
			Sys.sleep(prog_sleep)
			
			### Find "extra" files not used in Check Files
			# List Files
			fn_import <- sort(list.files(file.path(dn_data, dn_import),
												  recursive = TRUE,
												  full.names = FALSE))
			# User, Files
			df_user_files <- df_meta |>
				# filter for filename
				dplyr::filter(Domain == "filename") |>
				# select only some columns
				dplyr::select(Variable,
								  Definition,
								  Required,
								  Value) |>
				# Populate if file present
				dplyr::mutate(Present = 
								  	dplyr::case_when(is.na(Value) ~ NA,
								  						  Value %in% fn_import ~ TRUE,
								  						  .default = FALSE)) 
			
			df_user_files_present <- df_user_files |>
				# only TRUE
				dplyr::filter(Present == TRUE) |> 
				dplyr::pull(Value)
			
			# user files not listed in MetaData
			fn_import_full <- normalizePath(list.files(file.path(dn_data, dn_import),
														 recursive = TRUE,
														 full.names = TRUE))
			fn_user_files_extra <- fn_import[!fn_import %in% 
															c(fn_default_check_input_cast_metadata,
															  df_user_files_present)]
			path_4zip_extra <- file.path(dn_data, dn_import)
			fn_4zip_extra <- fn_import_full[basename(fn_import_full) %in% 
													  	fn_user_files_extra]
			
			### Tables
			path_4zip <- file.path(out.dir, 
										  region, 
										  dn_checked_sk)
			fn_4zip <- list.files(path = path_4zip,
										 full.names = TRUE,
										 pattern = "\\.tab$")
			zip::zip(file.path(getwd(), dn_data, "check_qctables.zip"), 
						files = fn_4zip,
						mode = "cherry-pick")
			### RDS
			path_4zip_rds <- file.path(out.dir, 
										  region,  
										  dn_checked_sk)
			fn_4zip_rds <- list.files(path = path_4zip_rds,
										 full.names = TRUE)
										 # pattern = "\\.rds$") # use all files
			
			fn_4zip <- c(fn_4zip_rds, fn_4zip_extra)
			
			zip::zip(file.path(getwd(), dn_data, "check_rds.zip"), 
						fn_4zip,
						mode = "cherry-pick")
			
			### 05, Update UI----
			prog_detail <- "Update UI"
			message(paste0("\n", prog_detail))
			# Increment the progress bar, and update the detail text.
			incProgress(1/prog_n, detail = prog_detail)
			Sys.sleep(prog_sleep)
			
			# enable download
			shinyjs::enable("but_check_dload_qctables")
			shinyjs::enable("but_check_dload_rds")
			
			### 06, Info Pop Up ----
			prog_detail <- "Inform User"
			message(paste0("\n", prog_detail))
			# Increment the progress bar, and update the detail text.
			incProgress(1/prog_n, detail = prog_detail)
			Sys.sleep(prog_sleep)
			
			msg <- paste("Checking of files is complete.",
							 "Download QC tables 1 and 2 (if needed).",
							 "Then download checked files (RDS) for use in 'Set Up'.",
							 sep = "\n")
			shinyalert::shinyalert(title = "Check Files",
										  text = msg,
										  type = "info",
										  closeOnEsc = TRUE,
										  closeOnClickOutside = TRUE)
	
			
		},
		message = "Check Files")## withProgress
		
	})## oE ~ but_check_check
	
	## Check, Files, Table1 ----
	output$df_check_qctable1_DT <- DT::renderDT({

		# trigger created when save table
		req(check_qctable_1_save())
 
		# region
		dn_data <- "Data"
		dn_results <- "Results"
		in.dir <- file.path(getwd(), dn_data, "input")
		out.dir <- file.path(getwd(), dn_data, "checked")
		path_meta <- file.path(in.dir, fn_default_check_input_cast_metadata)

		# Blank no metadata file
		if (!file.exists(path_meta)) {
			return(NULL)
		} ## IF ~ !file.exits(path_meta)
		
		df_meta <- readxl::read_excel(path_meta,
												sheet = "Sheet1",
												skip = 0)
		region <- df_meta |>
			dplyr::filter(Variable == "region") |>
			dplyr::pull(Value)

		path_table <- file.path(out.dir, 
										region,
										dn_checked_sk)

		inFile <- file.path(path_table, "TableOne.tab")

		# Blank if no data
		if (!file.exists(inFile)) {
			return(NULL)
		} ## IF ~ is.null(inFile)

		# import file
		df_table <- read.delim(inFile,
									  header = TRUE,
									  sep = "\t")

		# QC_Passed
		df_table <- df_table |>
			dplyr::mutate(QC_EC = grepl("No missing columns", ExpectedColumns),
							  QC_ED = grepl("All expected datatypes confirmed|all columns were generated by code", ExpectedDatatypes)) |>
			dplyr::mutate(QC_Passed = as.logical(QC_EC * QC_ED))


		DT::datatable(df_table,
						  filter = "top",
						  caption = "Table 1.",
						  options = list(
						  	scrollX=TRUE,
						  	lengthMenu = c(5, 10, 25, 50, 100),
						  	autoWidth = TRUE)) |>
			# color rows
			DT::formatStyle("QC_Passed",
								 target = "row",
								 backgroundColor = DT::styleEqual(
								 	c(TRUE, FALSE),
								 	c(color_good, color_bad))) |>
			# bold cells
			DT::formatStyle("QC_EC",
								 target = "cell",
								 fontWeight = DT::styleEqual(
								 	c(TRUE, FALSE),
								 	c("normal", "bold"))) |>
			DT::formatStyle("QC_ED",
								 target = "cell",
								 fontWeight = DT::styleEqual(
								 	c(TRUE, FALSE),
								 	c("normal", "bold"))) |>
			DT::formatStyle("QC_Passed",
								 target = "cell",
								 fontWeight = DT::styleEqual(
								 	c(TRUE, FALSE),
								 	c("normal", "bold")))
	}##expression
	)## df_check_table1_DT
	
	## Check, Files, Table2 ----
	output$df_check_qctable2_DT <- DT::renderDT({
		# trigger created when save table
		req(check_qctable_2_save())
		
		# region
		dn_data <- "Data"
		dn_results <- "Results"
		in.dir <- file.path(getwd(), dn_data, "input")
		out.dir <- file.path(getwd(), dn_data, "checked")
		path_meta <- file.path(in.dir, fn_default_check_input_cast_metadata)
		
		# Blank no metadata file
		if (!file.exists(path_meta)) {
			return(NULL)
		} ## IF ~ !file.exits(path_meta)
		
		df_meta <- readxl::read_excel(path_meta,
												sheet = "Sheet1",
												skip = 0)
		region <- df_meta |>
			dplyr::filter(Variable == "region") |>
			dplyr::pull(Value)
		
		path_table <- file.path(out.dir, 
										region,
										dn_checked_sk)
		
		inFile <- file.path(path_table, "TableTwo.tab")
		
		# Blank if no data
		if (!file.exists(inFile)) {
			return(NULL)
		} ## IF ~ is.null(inFile)
		
		# import file
		df_table <- read.delim(inFile,
									  header = TRUE,
									  sep = "\t")
		
		# QC_Passed
		df_table <- df_table |>
			dplyr::mutate(QC_II = grepl("^All", IntegrityIssues),
							  QC_OC = grepl("^All", OtherConditions)) |>
			dplyr::mutate(QC_Passed = as.logical(QC_II * QC_OC))
		
		
		DT::datatable(df_table,
						  filter = "top",
						  caption = "Table 2.",
						  options = list(
						  	scrollX=TRUE,
						  	lengthMenu = c(5, 10, 25, 50, 100),
						  	autoWidth = TRUE)) |>
			# color rows
			DT::formatStyle("QC_Passed",
								 target = "row",
								 backgroundColor = DT::styleEqual(
								 	c(TRUE, FALSE),
								 	c(color_good, color_bad))) |>
			# bold cells
			DT::formatStyle("QC_II",
								 target = "cell",
								 fontWeight = DT::styleEqual(
								 	c(TRUE, FALSE),
								 	c("normal", "bold"))) |>
			DT::formatStyle("QC_OC",
								 target = "cell",
								 fontWeight = DT::styleEqual(
								 	c(TRUE, FALSE),
								 	c("normal", "bold"))) |>
			DT::formatStyle("QC_Passed",
								 target = "cell",
								 fontWeight = DT::styleEqual(
								 	c(TRUE, FALSE),
								 	c("normal", "bold")))
	}##expression
	)## df_check_table2_DT
	
	
	## b_dload_check_Tables ----
	output$but_check_dload_qctables <- shiny::downloadHandler(
		filename = function() {
			paste0("CASTool_check_qctables_",
					 format(Sys.time(), "%Y%m%d_%H%M%S"),
					 ".zip")
		} ,
		content = function(fname) {
			file.copy(file.path(dn_data, "check_qctables.zip"), fname)
		}##content~END
		#, contentType = "application/zip"
	)##download ~ check files
	
	
	## b_dload_check_RDS ----
	output$but_check_dload_rds <- shiny::downloadHandler(
		filename = function() {
			paste0("CASTool_check_files_",
					 format(Sys.time(), "%Y%m%d_%H%M%S"),
					 ".zip")
		} ,
		content = function(fname) {
			file.copy(file.path(dn_data, "check_rds.zip"), fname)
		}##content~END
		#, contentType = "application/zip"
	)##download ~ check files
	
	
	
# SET UP ----
	## Set Up, Reactives ----
	sel_targsite <- reactive(input$si_checked_sites_targ)
	react_setup_explore <- reactiveVal("..No file uploaded..")
	react_setup_region <- reactiveVal(NULL) # triggers once
	react_setup_format <- reactiveVal("..No file uploaded..")
	
	output$txt_setup_explore <- renderText({
		react_setup_explore()
	})
	
	output$txt_setup_format <- renderText({
		react_setup_format()
	})
	
	## Set Up, Files, Checked ----
	observeEvent(input$fn_input_setup_checked_uload, {
		shiny::withProgress({

			### 00, Initialize----
			prog_detail <- "Import Checked Files..."
			message(paste0("\n", prog_detail))
			# Number of increments
			prog_n <- 6
			prog_sleep <- 0.25
			
			## 01, Import ----
			prog_detail <- "Import Data, Checked"
			message(paste0("\n", prog_detail))
			# Increment the progress bar, and update the detail text.
			incProgress(1/prog_n, detail = prog_detail)
			Sys.sleep(prog_sleep)
			
			# Ensure a file is uploaded
			# req(input$file_upload)  
			
			inFile <- input$fn_input_setup_checked_uload
			
			if (is.null(inFile)) {
				return(NULL)
			}
			
			# Define file
			fn_inFile <- inFile$datapath
			
			## 02, Clean Directory ----
			prog_detail <- "Remove Old Files"
			message(paste0("\n", prog_detail))
			# Increment the progress bar, and update the detail text.
			incProgress(1/prog_n, detail = prog_detail)
			Sys.sleep(prog_sleep)
			
			# Clean Directory
			clean_dir(file.path(dn_data, dn_checked))
			
			## 03, Unzip ----
			prog_detail <- "Unzip Files"
			message(paste0("\n", prog_detail))
			# Increment the progress bar, and update the detail text.
			incProgress(1/prog_n, detail = prog_detail)
			Sys.sleep(prog_sleep)
			
			fn_metadata <- "CASTmetadata.rds"
			
			# Unzip (remove any zip file directories)
			## only the one file so is faster
			zip::unzip(fn_inFile,
							 overwrite = TRUE,
							 exdir = file.path(tempdir(), dn_checked_sk),
							 files = fn_metadata, 
							 junkpaths = TRUE)
			
			## 04, Catalog ----
			prog_detail <- "Catalog Files"
			message(paste0("\n", prog_detail))
			# Increment the progress bar, and update the detail text.
			incProgress(1/prog_n, detail = prog_detail)
			Sys.sleep(prog_sleep)
			
			# open metadata
			data_CASTmeta_temp <- readRDS(file.path(tempdir(),
																 dn_checked_sk, 
																 "CASTmetadata.rds"))
			# get region
			data_region <- data_CASTmeta_temp |>
				dplyr::filter(Variable == "region") |>
				dplyr::pull(Value)
			
			# ensure directory exists
			path_check_sk <- file.path(dn_results,
												data_region,
												# dn_results,
												dn_checked_sk)
			if(!dir.exists(path_check_sk)) {
				dir.create(path_check_sk, recursive = TRUE)
			}## IF ~ !dir.exists
			
			# copy files from temp to app directory
			## Copy should be faster than unzip
			# but only extracted one file
			# file.copy(from = file.path(tempdir(), dn_checked),
			# 			 to = file.path(dn_data, dn_checked, data_region),
			# 			 overwrite = TRUE)
			
			
			# Unzip all files to app dir
			# (remove any zip file directories)
			zip::unzip(fn_inFile,
							 overwrite = TRUE,
							 exdir = file.path(path_check_sk),
							 junkpaths = TRUE)
			
			# List Files
			fn_checked <- sort(list.files(file.path(path_check_sk),
													recursive = TRUE,
													full.names = FALSE))
			# add blank so 1st file isn't auto-selected
			checked_filenames <- c("", fn_checked)
			
			# updateSelectInput(session,
			# 						"si_fn_input_check_sites",
			# 						choices = import_filenames,
			# 						selected = NULL)
			
			
			# return list of files
			zip_contents_checked <- checked_filenames
			
			## 05, Update UI ----
			prog_detail <- "Update SelectInputs"
			message(paste0("\n", prog_detail))
			# Increment the progress bar, and update the detail text.
			incProgress(1/prog_n, detail = prog_detail)
			Sys.sleep(prog_sleep)
			
			# loaded RDS files so import them
			# Load CASTool_Metadata 
			data_CASTmeta <- readRDS(file.path(path_check_sk,
														  "CASTmetadata.rds"))
			data_CASTmeta <- data_CASTmeta %>%
				tidyr::pivot_wider(names_from = Variable, values_from = Value)
		
			# User, CASTool MetaData
			# df_user_metadata <- readxl::read_excel(
			# 	file.path(path_check_sk, 
			# 				 fn_default_check_input_cast_metadata))
			# # "_CASTool_Metadata.xlsx"
			df_user_metadata <- readRDS(file.path(path_check_sk,
															  "CASTmetadata.rds"))
			
			fn_targets <- df_user_metadata |>
				# filter for filename
				dplyr::filter(Variable == "fn.targets") |>
				dplyr::pull(Value)
			
			# Targeted Sites File
			df_targets <- readRDS(
				file.path(path_check_sk, "df_targets.rds"))
			
			target_sites <- df_targets |>
				dplyr::pull(TargetSiteID) |>
				sort()
			
			# SelectInput - target sites
			shiny::updateSelectInput(session,
											 "si_checked_sites_targ",
											 choices = c("", target_sites),
											 selected = NULL)
	
			### update Reactives----
			
			#### explore_wsstressor_val
			meta_explore_wsstressor_val <- df_user_metadata |>
				dplyr::filter(Variable == "exploreWSStressor") |>
				dplyr::pull(Value)
			# if(meta_explore_wsstressor_val %in% c(TRUE, FALSE)) {
			# 	# shiny::updateRadioButtons(session,
			# 	# 						 "rad_setup_explore",
			# 	# 						 selected = meta_explore_wsstressor_val)
				react_setup_explore(meta_explore_wsstressor_val)
			# } else {
			# 	# Shiny alert - bad input
			# }## IF ~ explore_wsstressor_val
				
				if (react_setup_explore() == "TRUE") {
					showTab(inputId = "navbar",
							  target = "tab_wshedstress")
				} else {
					hideTab(inputId = "navbar",
							  target = "tab_wshedstress")
				}## IF
				
				#### region, reactive
				react_setup_region(data_region)
		
				#### report format
				meta_setup_format <- df_user_metadata |>
					dplyr::filter(Variable == "report_format") |>
					dplyr::pull(Value) |>
					toupper()
				react_setup_format(meta_setup_format)
				
				#### cand cause
				##### ph lo
				meta_lim_ph_lo <- df_user_metadata |>
					dplyr::filter(Variable == "pHlimLow") |>
					dplyr::pull(Value)
				react_candcause_thresh_ph_lo(meta_lim_ph_lo)
				##### ph hi
				meta_lim_ph_hi <- df_user_metadata |>
					dplyr::filter(Variable == "pHlimHigh") |>
					dplyr::pull(Value)
				react_candcause_thresh_ph_hi(meta_lim_ph_hi)
				##### do
				meta_lim_do <- df_user_metadata |>
					dplyr::filter(Variable == "DOlim") |>
					dplyr::pull(Value)
				react_candcause_thresh_do(meta_lim_do)
				
				#### wshed stress
				##### reach
				meta_wshed_reach <- df_user_metadata |>
					dplyr::filter(Variable == "useAllCompReaches") |>
					dplyr::pull(Value)
				react_wshed_reach(meta_wshed_reach)
				##### variables
				
				# 
				# fn_targets <- df_user_metadata |>
				# 	# filter for filename
				# 	dplyr::filter(Variable == "fn.targets") |>
				# 	dplyr::pull(Value)
				# 
				# # Targeted Sites File
				# df_streamcatvar <- readRDS(
				# 	file.path(path_check_sk, "df_targets.rds"))
				# 
				# wshed_var <- df_streamcatvar |>
				# 	dplyr::pull(TargetSiteID) |>
				# 	sort()
				# 
				# # SelectInput - target sites
				# shiny::updateSelectInput(session,
				# 								 "si_checked_sites_targ",
				# 								 choices = c("", wshed_var),
				# 								 selected = NULL)
				
			
			# Enable Buttons
			shinyjs::enable("but_report_run")
			
			## 06, Info Pop Up ----
			msg <- paste("Checked files uploaded.",
							 "Select a target site before running the report.",
							 sep = "\n")
			shinyalert::shinyalert(title = "Set Up",
										  text = msg,
										  type = "info",
										  closeOnEsc = TRUE,
										  closeOnClickOutside = TRUE)
			
		})## withProgress
	})## import, checked files
	
	# Reactive value to store zip file contents
	zip_contents_clusters <- reactiveVal(NULL)
	
	observeEvent(input$fn_input_setup_custom, {
		
		# Ensure a file is uploaded
		# req(input$file_upload)  
		
		inFile <- input$fn_input_setup_custom
		
		if (is.null(inFile)) {
			return(NULL)
		}
		
		# Define file
		fn_inFile <- inFile$datapath
		
		# Clean Directory
		clean_dir(file.path(dn_data, dn_clusters))
		
		# # Unzip (remove any zip file directories)
		# zip::unzip(fn_inFile,
		# 				 overwrite = TRUE,
		# 				 exdir = file.path(dn_data, dn_clusters),
		# 				 junkpaths = TRUE)
		# CSV not ZIP
		
		# Copy file
		file.copy(fn_inFile,
					 file.path(dn_data, dn_clusters),
					 overwrite = TRUE)
		
		# List Files
		fn_clusters <- sort(list.files(file.path(dn_data, dn_clusters),
												recursive = TRUE,
												full.names = FALSE))
		# add blank so 1st file isn't auto-selected
		clusters_filenames <- c("", fn_clusters)
		
		# return list of files
		zip_contents_clusters <- clusters_filenames
		
	})## import, checked files
	
	## unhide wshed summ ----
	observeEvent(react_setup_explore, {
		if (react_setup_explore() == "TRUE") {
			showTab(inputId = "navbar",
					  target = "tab_wshedstress")
		} else {
			hideTab(inputId = "navbar",
					  target = "tab_wshedstress")
		}## IF
	})## oE ~ rad_setup_explore
	
	## UI, clusters ----
	output$ui_setup_clust <- renderUI({
		if(input$rad_setup_assign == "Abiotic clustering") {
			selectInput("report_nclusters",
							"Number of clusters",
							choices = c("Default", 
											"1",
											"2", 
											"3"),
							selected = "Default")
		} else if(input$rad_setup_assign == "Custom") {
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
						 ) ##fileInput
		} else {
			NULL
		}## IF
	})## ui_setup
	
	## State, Selected Site ----
	output$txt_target_site_state <- renderText({

		str_site <- input$si_checked_sites_targ
		
		if (str_site == "") {
			return("..No file uploaded or site selected...")
		} else {
			
			# example
			# Sample data
			my_points <- data.frame(
				id = 1,
				longitude = c(-117.13055),
				latitude = c(47.88913)
			)
			
			# Real 
			## Read Metadata to get sites file
			# Read Sites File
			# Filter for selected site
			# Lat and long
			
			# Get US state boundaries
			US_states <- sf::st_as_sf(maps::map("state",
															plot = FALSE, 
															fill = TRUE))
			
			# Create spatial points
			my_points_sf <- sf::st_as_sf(my_points, 
												  coords = c("longitude", "latitude"), 
												  crs = sf::st_crs(US_states))
			
			# Spatial join
			sf::sf_use_s2(FALSE)
			points_with_states <- sf::st_join(my_points_sf, 
														 US_states, 
														 join = sf::st_intersects)
			
			# Extract state names
			print(points_with_states$ID)
			
			return(paste0("'", points_with_states[1, "ID"], "'"))
		}##IF~is.null~END
		
		
	})## fn_input_display_indexclass
	
	## Clusters, Laura ----
	observeEvent(input$but_setup_cluster_laura, {
		
		
		
	})## oE ~ Report
	
	## fig, clust ----
	output$map_sites <- renderImage({
		# ensure files uploaded
		req(input$fn_input_setup_checked_uload)

		# map file name
		# don't have dir so use temp version
		data_CASTmeta_temp <- readRDS(file.path(tempdir(),
															 dn_checked_sk, 
															 "CASTmetadata.rds"))
		# get region
		data_region <- data_CASTmeta_temp |>
			dplyr::filter(Variable == "region") |>
			dplyr::pull(Value)

		# dir of 'checked' files
		path_check_sk <- file.path(dn_results,
											data_region,
											dn_checked_sk)

		## Get map file	
		# fn_map <- data_CASTmeta_temp |>
		# 	# filter for filename
		# 	dplyr::filter(Variable == "fn.map") |>
		# 	dplyr::pull(Value)
		fn_map <- "cluster_graphic.png"
		path_map <- file.path(path_check_sk, fn_map)
		
		# QC
		ext <- tools::file_ext(path_map)
		validate(need(tolower(ext) %in% 
						  	c("png", "jpg", "jpeg"),
						  "Please use PNG or JPG graphic."))
		
		# Display
		list(src = path_map,
			  contentType = if (tolower(ext) == "png") "image/png" else "image/jpeg",
			  alt = fn_map,
			  width = 7200 / 10, #orig size /  scale value
			  height = 4800 / 10)
	}, deleteFile = FALSE)## map_sites
	
# REPORT----
	
	## reactives----
	react_report_run <- reactiveVal(FALSE)
	
	## siteid ----
	output$txt_rep_siteid <- renderText({
		# The expression here is reactive; it updates when input$text_input_id changes
		if (is.null(sel_targsite()) | sel_targsite() == "") {
			txt_siteid <- "..No target site selected.."
		} else {
			txt_siteid <- sel_targsite()
		}## IF
		paste0("Target Site: ", txt_siteid)
	})
	
	## button, report ----
	observeEvent(input$but_report_run, {
		# launch skeleton code
		
		# QC, site not null or ""
			if(is.null(sel_targsite()) | sel_targsite() == "") {
			msg <- paste("No target site selected!",
							 "\n",
							 "Return to 'Set Up Tool' and 'Select target site' to make selection.",
							 sep = "\n")
			shinyalert::shinyalert(title = "Run Report",
										  text = msg,
										  type = "error",
										  closeOnEsc = TRUE,
										  closeOnClickOutside = TRUE)
			validate(msg)
		}## IF ~ check_files_fails
		
		### Global Variables ----
		# define for sourced Skeleton Code file
		
		# borrow from SetUp (04, Catalog)
		# open metadata
		data_CASTmeta_temp <- readRDS(file.path(tempdir(), 
															 dn_checked_sk, 
															 "CASTmetadata.rds"))
		# get region
		data_region <- data_CASTmeta_temp |>
			dplyr::filter(Variable == "region") |>
			dplyr::pull(Value)
		# path
		path_check_sk <- file.path(dn_results, 
											data_region,
											dn_checked_sk)
		
		# Set variables used in Skeleton code
		boo_Shiny <- TRUE
		boo.debug <- FALSE
		debug.person <- "Shiny" # Ann, Erik, Laura
		#  
		region <- data_region
		#
		in.dir   <- file.path(path_check_sk)
		out.dir  <- file.path(dn_results)
		boo.plot.user <- TRUE
		
		wd <- getwd()
		gitpath <- NULL # Not needed for Shiny
		dir_rmd <- system.file("rmd", 
									  package = "CASTfxn")

		
		## Skeleton Code ----
		# code (wrap with progress pop up)
		shiny::withProgress({
			
			
		# browser()
		# 	cat("debug here")
		# 	
		# # debugging
		# source("C:/Users/Erik.Leppo/Documents/GitHub/CASTfxn/inst/shiny-examples/CASTool/CASTool.r",
		# 		 local = TRUE)
		# 	
		# 	
			
			# Skeleton Code
			source(path_skelcode, local = TRUE)
		}, message = "Skeleton Code"
		)## withProgress
		
		# Enabled buttons ----
		shinyjs::enable("rad_report_tabs")
		shinyjs::enable("but_report_dload")
		shinyjs::enable("but_dload_wshed_figs")
		
		## create zip ----

		# get files 4 zip
		stat_id <- sel_targsite() # reactive value
		dn_zip_stat <- file.path(dn_results,
										 react_setup_region(),
										 stat_id)
		dn_zip_hist <- file.path(dn_results,
										 react_setup_region(),
										 "_Histograms")
		fn_zip_stat <- list.files(path = dn_zip_stat,
										  recursive = TRUE,
										  full.names = TRUE)
		fn_zip_hist <- list.files(path = dn_zip_hist,
										  recursive = TRUE,
										  full.names = TRUE)
		# combined files
		fn_zip <- c(fn_zip_stat, fn_zip_hist)
		# create zip
		tic <- Sys.time()
		zip::zip(file.path(dn_results, "report_results.zip"),
					files = fn_zip)
		toc <- Sys.time()
		msg <- paste0("zip time (sec): ",
						  round(difftime(toc, tic, units = "secs"), 2))
		message(msg)

		## reactiveval updates ----
		# used to indicate report run
		react_report_run(TRUE) # gaps
	
		## Stress Summ RMD ----
		# rend_input <- file.path("external",
		# 								"RMD",
		# 								"display_images_StressSumm.Rmd")
		#
		# copy RMD so works in Shiny
		## same issue for stress summ as getReport in skeleton code
		## render switches working directory to location of RMD
		rmd2copy <- list.files(file.path("external",
													"RMD"),
									  pattern = "display_images_StressSumm\\.Rmd$",
									  full.names = TRUE)
		file.copy(rmd2copy, ".", overwrite = TRUE)
		rend_input <- "display_images_StressSumm.Rmd"
		path_shiny_www <- file.path("www",
											 "RMD_HTML")
		# browser()
		rmarkdown::render(input = rend_input,
								# output_dir = path_shiny_www,
								output_file = file.path(path_shiny_www,
																"ShinyHTML_StressSumm.html"),
								params = list(react_setup_region = react_setup_region)
		)
		
		## Wshed Stress Fig Zip ----

		# get files 4 zip
		stat_id <- sel_targsite() # reactive value
		dn_zip_ws <- file.path(dn_results,
									  "Washington", #react_setup_region(),
									  stat_id,
									  "SiteInfo")
		fn_zip_ws <- list.files(path = dn_zip_ws,
										pattern = "_WSstress_.*\\.png$",
										full.names = TRUE,
										recursive = FALSE)
		# create zip
		tic <- Sys.time()
		zip::zip(file.path(dn_results, "WSstress_figs.zip"),
					files = fn_zip_ws,
					mode = "cherry-pick")
		toc <- Sys.time()
		msg <- paste0("zip time (sec): ",
						  round(difftime(toc, tic, units = "secs"), 2))
		message(msg)
		

		# get watershed variables
		## fn_zip_ws above
		# extract watershed variables
		fn_zip_ws_var <- sub("^.*_WSstress_([^/]+)\\.png$", "\\1", 
									basename(fn_zip_ws))
		
		
		# have data_stressorinfoWS in env so use it
		df_plots_data_stressorinfoWS <- data_stressorinfoWS %>%
			# keep columns need
			dplyr::select(StreamCatVar, Label) %>%
			# remove duplicates
			unique() %>%
			# note if have plot
			dplyr::mutate(plot_present = StreamCatVar %in% fn_zip_ws_var)
		
		react_wshed_var(df_plots_data_stressorinfoWS)
		
	})## oE ~ Report
	
	
	
	# unhide report tabs ----
	observeEvent(input$rad_report_tabs, {
		if (input$rad_report_tabs == "Yes") {
			showTab(inputId = "navbar",
					  target = "tab_candcause")
			showTab(inputId = "navbar",
					  target = "tab_woesumm")
			showTab(inputId = "navbar",
					  target = "tab_stresssumm")
			showTab(inputId = "navbar",
					  target = "tab_gaps")
		} else {
			hideTab(inputId = "navbar",
					  target = "tab_candcause")
			hideTab(inputId = "navbar",
					  target = "tab_woesumm")
			hideTab(inputId = "navbar",
					  target = "tab_stresssumm")
			hideTab(inputId = "navbar",
					  target = "tab_gaps")
		}## IF
		
		
	})## oE ~ rad_report_tabs
	
	# button, report, download ----
	
	output$but_report_dload <- shiny::downloadHandler(
		filename = function() {
			paste0("CASTool_report_results_",
					 format(Sys.time(), "%Y%m%d_%H%M%S"),
					 ".zip")
		} ,
		content = function(fname) {
			# zip file created when report run
			#
			# file for download
			file.copy(file.path(dn_results, "report_results.zip"), fname)
		}##content~END
		#, contentType = "application/zip"
	)##download ~ check files
	
	
	# WSHED STRESS ----

	## reactives----
	react_wshed_reach <- reactiveVal("..No file uploaded..")
	react_wshed_var <- reactiveVal("..No file uploaded..")
	
	output$txt_wshed_reach <- renderText({
		# updated from meta data in Set Up
		if (react_wshed_reach() == "TRUE") {
			react_wshed_reach("All reaches in cluster")
		} else if (react_wshed_reach() == "FALSE") {
			react_wshed_reach("All reaches in cluster with a sampled site")
		} else {
			react_wshed_reach()
		}## IF		
	})
	
	## select input ----
	# update select input when value changes
	observeEvent(react_wshed_var(), {
		# updated in run report
		req(react_report_run())
		
		# if (react_wshed_var() == "..No file uploaded..") {
		# 	# do nothing
		# } else {
			# prep data
			df <- as.data.frame(react_wshed_var())
			# filter for true
			df_filt <- df[df$plot_present == TRUE, "Label"]
			# update
			updateSelectInput(
				session, 
				"si_wshed_var",
				choices = c("", df_filt),
				selected = ""
				)
		# }## IF
	}, ignoreInit = TRUE)## oE ~ react_wshed_var
	
	## plot, wshed ----
	output$plot_wshed <- renderImage({
		# report run
		req(react_report_run())

		stat_id <- sel_targsite()
		dn_plots <- file.path(dn_results,
										react_setup_region(),
										stat_id,
										"SiteInfo")
		
		wss_lab <- input$si_wshed_var
		
		df <- as.data.frame(react_wshed_var())
		wss_scv <- df %>%
			dplyr::filter(Label == wss_lab) %>%
			dplyr::pull(StreamCatVar)
			
		fn_plot <- paste0(stat_id,
								"_WSstress_",
								wss_scv,
								".png")
		
		path_plot <- file.path(dn_plots, fn_plot)

		# QC
		ext <- tools::file_ext(basename(path_plot))
		validate(need(tolower(ext) %in% 
						  	c("png", "jpg", "jpeg"),
						  "Please use PNG or JPG graphic."))
		
		# Display
		list(src = path_plot,
			  contentType = if (tolower(ext) == "png") "image/png" else "image/jpeg",
			  alt = fn_plot,
			  width = "80%"#, #orig size /  scale value
			  # height = "80%" / 10
			  # width = 4800 / 10, #orig size /  scale value
			  # height = 5400 / 10
			  )
	}, deleteFile = FALSE)## map_sites
	
	
	## button, wshed, download ----
	output$but_dload_wshed_figs <- shiny::downloadHandler(
		filename = function() {
			paste0("CASTool_WSstress_figs_",
					 format(Sys.time(), "%Y%m%d_%H%M%S"),
					 ".zip")
		} ,
		content = function(fname) {
			# zip file created when report run
			#
			# file for download
			file.copy(file.path(dn_results, "WSstress_figs.zip"), fname)
		}##content~END
		#, contentType = "application/zip"
	)##download ~ check files
	
# CAND CAUSE ----
	
	## reactives ----
	react_candcause_thresh_ph_lo <- reactiveVal(NULL)
	react_candcause_thresh_ph_hi <- reactiveVal(NULL)
	react_candcause_thresh_do <- reactiveVal(NULL)
	
	## text ----
	output$txt_candcause_thresh_ph <- renderText({
		paste0("pH: < ", 
				 react_candcause_thresh_ph_lo(),
				 ", > ",
				 react_candcause_thresh_ph_hi())
	})
	
	output$txt_candcause_thresh_do <- renderText({
		paste0("DO: < ", react_candcause_thresh_do())
	})
	
	## Table, Elim ----
	output$df_candcause_elim_DT <- DT::renderDT({

		# trigger created when save table
		req(react_report_run())
		
		stat_id <- sel_targsite()
		fn_gaps <- paste0(stat_id, "_BMI_DetectsNotEvalFurther.tab")
		
		path_table <- file.path(dn_results, 
										react_setup_region(),
										stat_id,
										"BMI",
										"_WoE")
		
		inFile <- file.path(path_table, fn_gaps)
		
		# Blank if no data
		if (!file.exists(inFile)) {
			return(NULL)
		} ## IF ~ is.null(inFile)
		
		# import file
		df_table <- read.delim(inFile,
									  header = TRUE,
									  sep = "\t")
		
		# # keep certain columns
		# df_table <- df_table 
		
		dt_cap <- paste0("Stressors, Eliminated")
		
		DT::datatable(df_table,
						  filter = "top",
						  caption = dt_cap,
						  options = list(
						  	scrollX = TRUE,
						  	lengthMenu = c(5, 10, 25, 50, 100),
						  	autoWidth = TRUE))
		
	}##expression
	)## df_cc_elim_DT
	
	## Table, All ----
	output$df_candcause_all_DT <- DT::renderDT({
		
		# trigger created when save table
		req(react_report_run())
	
		stat_id <- sel_targsite()
		fn_detects <- paste0(stat_id, "_DetectsAll.tab")
		
		path_table <- file.path(dn_results, 
										react_setup_region(),
										stat_id
										)
		
		inFile <- file.path(path_table, fn_detects)
		
		# Blank if no data
		if (!file.exists(inFile)) {
			return(NULL)
		} ## IF ~ is.null(inFile)
		
		# import file
		df_table <- read.delim(inFile,
									  header = TRUE,
									  sep = "\t")
		
		# # keep certain columns
		# df_table <- df_table 
		
		dt_cap <- paste0("Stressors, All")
		
		DT::datatable(df_table,
						  filter = "top",
						  caption = dt_cap,
						  options = list(
						  	scrollX = TRUE,
						  	lengthMenu = c(5, 10, 25, 50, 100),
						  	autoWidth = TRUE))
		
	}##expression
	)## df_cc_all_DT
	
# WOE SUMM ----
	
	## woe loe summary table ----
	df_woe_summ <- reactive({
		# check if data exists
		
		fn_data <- file.path(dn_results, 
									react_setup_region(),
									sel_targsite(), 
									dn_bmi, 
									dn_woe,
									paste0(sel_targsite(), "_LoESummary.tab"))
		if (file.exists(fn_data)) {
			df <- read.table(fn_data, 
								  header = TRUE, 
								  sep = "\t", 
								  stringsAsFactors = FALSE)
		} else {
			showNotification("WoE LoE Summary file not found.",
								  type = "warning",
								  duration = 5)
		}## IF ~ exists
		return(df)
	})## data
	
	# render LoE summary table
	output$tbl_woe_summ <- DT::renderDataTable({
		DT::datatable(
			df_woe_summ(),
			options = list(
				scrollX = TRUE,
				pageLength = 5,
				lengthMenu = c(5, 10, 25, 50, 100, 1000),
				autoWidth = TRUE
			), 
			rownames = FALSE,
			filter = "top"
		)
	})## table
	
	## woe table ----
	df_woe <- reactive({
		# check if data exists
		# dn_site <- basename(list.dirs(file.path(dn_results), 
		# 										recursive = FALSE))
		fn_data <- file.path(dn_results,
									react_setup_region(),
									sel_targsite(),
									dn_bmi, 
									dn_woe,
									paste0(sel_targsite(), "_LoEs.tab"))
		if (file.exists(fn_data)) {
			df <- read.table(fn_data, 
								  header = TRUE, 
								  sep = "\t", 
								  stringsAsFactors = FALSE)
		} else {
			showNotification("WoE file not found.",
								  type = "warning",
								  duration = 5)
		}## IF ~ exists
		return(df)
	})## data
	
	# render WoE table
	output$tbl_woe <- DT::renderDataTable({
		
		DT::datatable(
			df_woe(),
			options = list(
				scrollX = TRUE,
				pageLength = 5,
				lengthMenu = c(5, 10, 25, 50, 100, 1000),
				autoWidth = TRUE
			), 
			rownames = FALSE,
			filter = "top"
		)
	})## table
	
	## woe fig, bio index----
	output$img_bio_index <- renderImage({
		# return path
		list(
			src = file.path(dn_results,
								 react_setup_region(),
								 sel_targsite(), 
								 "SiteInfo", 
								 paste0(sel_targsite(),
								 		 "_BMI_IndexBoxPlotsByCase.png")
								 ),
			contentType = "image/png",
			alt = "Biological Index Distributions",
			width = 7200 / 8, #orig size /  scale value
			height = 4800 / 8
		)
	}, deleteFile = FALSE)
	
	## woe fig, loe----
	output$img_loe_summ <- renderImage({
		# return path
		list(
			src = file.path(dn_results, 
								 react_setup_region(),
								 sel_targsite(),
								 "BMI",
								 "_WoE",
								 paste0(sel_targsite(),
								 		 "_BMI_LoESummaryFig.png")),
			contentType = "image/png",
			alt = "Lines of Evidence Summary",
			width = 4800 / 5, #orig size /  scale value
			height = 3600 / 5
		)
	}, deleteFile = FALSE)
	
# STRESS SUMM ----
	# RMD created in Run Report section
	
	# Watch the file for changes. intervalMillis defines check frequency.
	ss_html_content <- reactiveFileReader(
		intervalMillis = 1000,  # check every 1 second (adjust as needed)
		session = session,
		filePath = file.path("www", "RMD_HTML", "ShinyHTML_StressSumm.html"),
		readFunc = function(path) {
			paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
		}
	)## ss_html_content
	
	
	# Render the current HTML content
	output$stresssum_html <- renderUI({
		HTML(ss_html_content())
	})
	
	
# GAPS ----
	
	## Gaps, Table ----
	output$df_gaps_DT <- DT::renderDT({
	
		# trigger created when save table
		req(react_report_run())
		
		stat_id <- sel_targsite()
		fn_gaps <- paste0(stat_id, "_datagaps.tab")
		
		path_table <- file.path(dn_results, 
										react_setup_region(),
										stat_id)
		
		inFile <- file.path(path_table, fn_gaps)
		
		# Blank if no data
		if (!file.exists(inFile)) {
			return(NULL)
		} ## IF ~ is.null(inFile)
		
		# import file
		df_table <- read.delim(inFile,
									  header = TRUE,
									  sep = "\t")
		
		# # keep certain columns
		# df_table <- df_table 
		
		dt_cap <- paste0("Data gap (Site ID = ",
							  stat_id,
							  ").")
		
		DT::datatable(df_table,
						  filter = "top",
						  caption = dt_cap,
						  options = list(
						  	scrollX = TRUE,
						  	lengthMenu = c(5, 10, 25, 50, 100),
						  	autoWidth = TRUE))
			
	}##expression
	)## df_gaps_DT
	
	
}## main