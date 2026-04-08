# Custom Region Boundary

function() {
	tabPanel("Custom Region",
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
					h2("Load region boundary"),
					fileInput("fn_region_uload", 
								 'Upload zipped folder with boundary region files',
								 multiple = FALSE,
								 accept = c(
								 	'zip',
								 	'.zip')
								) |>
						bs_embed_tooltip(title = "Maximum 300 MB",
											  placement = "right"), ##fileInput
					
				
					# regionName
					# custom text.  used as folder so no spaces allowed
					# add character count?
					# trimws
					shiny::textInput(
						inputId = "txt_region_name",
						label = "Region name",
						placeholder = "Enter Region Name (max 20 and no spaces)"
					),
					bsTooltip(id = "txt_region_name",
								 title = paste0("Maximum length of 20.  No spaces."),
								 placement = "right"),
					
					# states
					# select multiple states
					shiny::selectInput(
						inputId = "sel_region_states",
						label = "States (multiple allowed) :", 
						choices = conus_names, 
						selected = character(0),
						multiple = TRUE
						),
					
					# radio, clustering (TRUE/FALSE)
					shiny::radioButtons(
						inputId = "rad_region_clustering",
						label = "Run clustering algorithm to generate comparator assignments",
						choices = c("Yes", "No"),
						selected = "No"),
					
					# clustering, yes, default num clusters
					# user_numclust
					# YES = NULL
					shiny::radioButtons(
						inputId = "rad_region_clust_default",
						label = "Generate default number of clusters (1-5)",
						choices = c("Yes", "No"),
						selected = "Yes"),
					
					# clustering, yes, default No
					# number of clusters, integer, user_numclust
					shiny::numericInput(
						inputId = "num_region_clust_num",
						label = "Desired number of clusters",
						value = NULL
					),
					
					# min prop, default = 0.2, minCOMIDsCluster
					shiny::numericInput(
						inputId = "num_region_clust_mincomid",
						label = "Minimum proportion of COMIDs per cluster",
						value = 0.2
					),
					
					# cum pct, default = 60, pct_var
					shiny::numericInput(
						inputId = "num_region_clust_pctvar",
						label = "Cumulative percent variation for PCA",
						value = 60
					),
					
					# gen files for wshed, wsStressorData (TRUE/FALSE)
					# radio
					shiny::radioButtons(
						inputId = "rad_region_wsstress",
				      label = "Generate files for watershed stressor analysis",
					   choices = c("Yes", "No"),
					   selected = "Yes"),
					
					# run button
					h2("Generate Boundaries"),
					shinyjs::disabled(bsButton("but_region_run",
														"Generate boundary region")),
					bsTooltip(id = "but_region_run",
								 title = paste0("Enabled after files uploaded"),
								 placement = "right"),
					div(style = "width: 50%",
						 p(em("Generate boundary region file based on user selections."))),
					
					
					# progress bar
					# 3 steps, 1 setup, 2 = actual calc, 3 = clean up
					# use sleep time
					
					# download output
					# hidden until run is clicked
					
					div(shinyjs::disabled(shiny::downloadButton(
						"but_region_dload",
						"Download boundary region"))),
					bsTooltip(id = "but_region_dload",
								 title = paste0("Only enabled after files checked"),
								 placement = "right"),
					div(style = "width: 50%", 
						 p(em("Download a zipped folder boundary region.")))
					
					
					)## mainPanel
			)## tabPanel
}## FUNCTION
