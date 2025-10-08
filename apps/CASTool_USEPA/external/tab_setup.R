# Set Up

function() {
	tabPanel("Set Up Tool",
				mainPanel(
					h2("Select target site and analysis parameters"),
					fileInput('fn_input_setup_checked_uload', 
								 'Upload checked zip file',
								 multiple = FALSE,
								 accept = c(
								 	'zip',
								 	'.zip')
					) |>
						bs_embed_tooltip(title = "Maximum 300 MB"), ##fileInput
					# shinyBS::bsTooltip(id = "fn_input_setup_checked_uload",
					# 						 title = paste0("Add all files to a single zip file"),
					# 						 placement = "right"),
					# doesn't work on fileinput
					br(),
					fluidRow(
						# width = 12
						column(4,
								 selectInput("si_checked_sites_targ",
								 				"Select target site",
								 				choices = NULL,
								 				multiple = FALSE),
								 radioButtons("rad_setup_explore",
								 				 "Explore watershed stressor data",
								 				 choices = c("Yes", "No"),
								 				 selected = "No")
								 ),
						column(4,
								 radioButtons("rad_setup_assign",
								 				 "Comparator assignment method",
								 				 choices = c("Abiotic clustering", "Custom"),
								 				 selected = "Abiotic clustering"),
								 # reactive to radio
								 # n_cluster or upload
								 uiOutput("ui_setup_clust"),
								 
								 bsButton("but_setup_comp",
								 			"Get comparators")
								 ),
						column(4,
								 p("text showing cluster method and number."),
								 br(),
								 p("if choose abiotic method get some plots"),
								 br(),
								 p("map")
								 )
						),## fluidRow
					br(),
					h2("testing"),
					p("state of target site"),
					p(textOutput("txt_target_site_state")),
					br(),
					bsButton("but_setup_cluster_laura",
								"Get Clusters (Laura)")
					
							)## mainPanel
				)## tabPanel
}## FUNCTION