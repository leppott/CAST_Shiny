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
	
	# IMPORT ----
	
	# file_watch <- reactive({
	# 	# trigger for df_import()
	# 	input$fn_input_check_uload
	# 	#paste(input$fn_input, input$but_radio_load)
	# })## file_watch
	
	## Import, Files, Input ----
	
	# Reactive value to store zip file contents
	zip_contents_input <- reactiveVal(NULL)
	
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
			utils::unzip(fn_inFile,
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
		},
		message = "Load Files")## withProgress
	})## observeEvent
	
	## Import, ID Files, present ----
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
								 	c("cyan", "magenta")
								 	)
								 )
		
		# return(df_user_files)

	}##expression

	)## df_data_DT
	
	
	## Import, ID Files, missing ----
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
		
		return(df_user_files_missing)
	})
	
	## Import, ID Files, extra ----
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
	
	
	# CHECK----
	## Import, Files, Checked ----
	
	# Reactive value to store zip file contents
	zip_contents_checked <- reactiveVal(NULL)
	
	observeEvent(input$fn_input_setup_checked_uload, {
		shiny::withProgress({
			
			### 00, Initialize----
			prog_detail <- "Import Checked Files..."
			message(paste0("\n", prog_detail))
			# Number of increments
			prog_n <- 5
			prog_sleep <- 0.25

			### 01, Import ----
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
			
			### 02, Clean Directory ----
			prog_detail <- "Remove Old Files"
			message(paste0("\n", prog_detail))
			# Increment the progress bar, and update the detail text.
			incProgress(1/prog_n, detail = prog_detail)
			Sys.sleep(prog_sleep)
			
			# Clean Directory
			clean_dir(file.path(dn_data, dn_checked))
			
			### 03, Unzip ----
			prog_detail <- "Unzip Files"
			message(paste0("\n", prog_detail))
			# Increment the progress bar, and update the detail text.
			incProgress(1/prog_n, detail = prog_detail)
			Sys.sleep(prog_sleep)
		
			# Unzip (remove any zip file directories)
			utils::unzip(fn_inFile,
							 overwrite = TRUE,
							 exdir = file.path(dn_data, dn_checked),
							 junkpaths = TRUE)
			
			### 04, Catalog ----
			prog_detail <- "Catalog Files"
			message(paste0("\n", prog_detail))
			# Increment the progress bar, and update the detail text.
			incProgress(1/prog_n, detail = prog_detail)
			Sys.sleep(prog_sleep)
			
			# List Files
			fn_checked <- sort(list.files(file.path(dn_data, dn_checked),
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
			
			### 04, Update SelectInputs ----
			prog_detail <- "Update SelectInputs"
			message(paste0("\n", prog_detail))
			# Increment the progress bar, and update the detail text.
			incProgress(1/prog_n, detail = prog_detail)
			Sys.sleep(prog_sleep)
			
			# User, CASTool MetaData
			df_user_metadata <- readxl::read_excel(
				file.path(dn_data, 
							 dn_checked, 
							 fn_default_check_input_cast_metadata))
			# "_CASTool_Metadata.xlsx"
			
			fn_targets <- df_user_metadata |>
				# filter for filename
				dplyr::filter(Variable == "fn.targets") |>
				dplyr::pull(Value)
			
			# Targeted Sites File
			df_targets <- read.csv(
				file.path(dn_data, dn_checked, fn_targets))
			
			target_sites <- df_targets |>
				dplyr::pull(TargetSiteID) |>
				sort()
			
			# SelectInput - target sites
			updateSelectInput(session,
									"si_checked_sites_targ",
									choices = target_sites,
									selected = NULL)
			
		})## withProgress
	})## import, checked files
	
	# SET UP ----
	## Import, Files, Clusters ----
	
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
		# utils::unzip(fn_inFile,
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
	observeEvent(input$rad_setup_explore, {
		if (input$rad_setup_explore == "Yes") {
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
	
	
	# REPORT----
	
	## button, report ----
	observeEvent(input$but_report_run, {
		
		
		
	})## oE ~ Report
	
	## unhide report tabs ----
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
	
	# WSHED STRESS ----

	## button, get GitHub files ----
	observeEvent(input$but_github_wshedstress_data, {
		shiny::withProgress({
		
			### 00, Initialize ----
			prog_detail <- "GitHub Copy, Get Files..."
			message(paste0("\n", prog_detail))
			
			# Number of increments
			prog_n <- 4
			prog_sleep <- 0.25
			
			### 01, Clean Dir----
			prog_detail <- "Clean Directory"
			message(paste0("\n", prog_detail))
			# Increment the progress bar, and update the detail text.
			incProgress(1/prog_n, detail = prog_detail)
			Sys.sleep(prog_sleep)
			
			# remove existing files
			clean_dir(file.path(dn_data, dn_ws_stress))
			
			### 01, Download ----
			prog_detail <- "Download Files"
			message(paste0("\n", prog_detail))
			# Increment the progress bar, and update the detail text.
			incProgress(1/prog_n, detail = prog_detail)
			Sys.sleep(prog_sleep)
			
			# zip
			httr::GET(url_ws_stress_zip
						 , httr::write_disk(temp_ws_stress_zip <- tempfile(fileext = ".zip")))
			# csv
			httr::GET(url_ws_stress_info
						 , httr::write_disk(temp_ws_stress_info <- tempfile(fileext = ".csv")))
			
			
			### 02, Copy ----
			prog_detail <- "Copy and Unzip Files"
			message(paste0("\n", prog_detail))
			# Increment the progress bar, and update the detail text.
			incProgress(1/prog_n, detail = prog_detail)
			Sys.sleep(prog_sleep)
			
			# zip
			utils::unzip(temp_ws_stress_zip,
							 exdir = file.path(dn_data, dn_ws_stress),
							 junkpaths = TRUE,
							 overwrite = TRUE)
			
			# csv
			file.copy(temp_ws_stress_info,
						 file.path(dn_data, dn_ws_stress, "stressor-info.csv"),
						 overwrite = TRUE)
			
			### 03, Verify Files ----
			prog_detail <- "Verify Files"
			message(paste0("\n", prog_detail))
			# Increment the progress bar, and update the detail text.
			incProgress(1/prog_n, detail = prog_detail)
			Sys.sleep(prog_sleep)
			
			n_files <- length(list.files(file.path(dn_data, dn_ws_stress)))
			
			msg <- paste0("Files copied from GitHub = ", n_files)
			message(msg)
			
			# # Inform user about number of files
			# ## calc number of mismatch
			# df_mismatch <- data.frame(taxatrans_results$nonmatch)
			# n_taxa_mismatch <- nrow(df_mismatch)
			# msg <- paste0("Number of mismatch taxa = ", n_taxa_mismatch, "\n\n"
			# 				  , "Any mismatched taxa in 'mismatch' file in results download.")
			# shinyalert::shinyalert(title = "Taxa Translate, Non Matching Taxa"
			# 							  , text = msg
			# 							  , type = "info"
			# 							  , closeOnEsc = TRUE
			# 							  , closeOnClickOutside = TRUE)
			# #validate(msg)
			
			
			### 04, Finish ----
			prog_detail <- "Finish"
			message(paste0("\n", prog_detail))
			# Increment the progress bar, and update the detail text.
			incProgress(1/prog_n, detail = prog_detail)
			Sys.sleep(prog_sleep)
			
			# button, enable, download
			shinyjs::enable("but_dload_wshed_app_fig")

			
		}, message = "GitHub files")## progress
	})## but_github_wshedstress_data
	
	## button, download ----
	# but_dload_wshed_app_fig
	
	# CAND CAUSE ----
	
	# WOE SUMM ----
	
	## woe loe summary table ----
	df_woe_summ <- reactive({
		# check if data exists
		dn_site <- basename(list.dirs(file.path(dn_results), 
												recursive = FALSE))
		fn_data <- file.path(dn_results, 
									dn_site, 
									dn_bmi, 
									dn_woe,
									paste0(dn_site, "_LoESummary.tab"))
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
		dn_site <- basename(list.dirs(file.path(dn_results), 
												recursive = FALSE))
		fn_data <- file.path(dn_results, 
									dn_site, 
									dn_bmi, 
									dn_woe,
									paste0(dn_site, "_LoEs.tab"))
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
	
	output$img_bio_index <- renderImage({
		# return path
		list(
			src = file.path(dn_data, dn_temp, "bio_index.png"),
			contentType = "image/png",
			alt = "Stressors Visualization"
		)
	}, deleteFile = FALSE)
	

	
	# STRESS SUMM ----
	
	output$img_stressors <- renderImage({
		# return path
		list(
			src = file.path(dn_data, dn_temp, "stressors.png"),
			contentType = "image/png",
			alt = "Stressors Visualization"
		)
	}, deleteFile = FALSE)
	
	# GAPS ----

}## main