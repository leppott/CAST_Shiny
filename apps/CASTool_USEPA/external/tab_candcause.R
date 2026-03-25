# Check Files

function() {
	tabPanel("Candidate Causes",
				value = "tab_candcause",
				mainPanel(
					use_bs_popover(),
					h2("Candidate Causes"),
					
					p(strong("Target site, report:")),
					div(class = "pill", textOutput("txt_rep_siteid_candcause")),
					br(),
					
					h4(tagList("User-specified thresholds for evaluating specific candidate causes",
								 actionLink("helpCandCause", label = NULL, icon = icon("question-circle", class = "fas"), class = "help-btn"))
					),
					
					textOutput("txt_candcause_thresh_ph"),
					textOutput("txt_candcause_thresh_do"),
					br(),
					#h4("Stressor(s) initially evaluated"),
					
					h4(tagList(
						"Stressor(s) initially evaluated",
						icon("info-circle", 
							  style = "color: #67c1f5", 
							  id="stressorInitInfo") |>
							bs_embed_popover(title = "Helpful Hints",
												  content = "Stressors marked for inclusion in the stressor metadata (UseInStressorID) and present in the stressor data.",
												  placement = "right",
												  trigger = "hover")
						)),
					# bsPopover(id="stressorInitInfo", 
					# 			 title = HTML("<b>Helpful Hints</b>"), 
					# 			 content = HTML("Stressors included in the measured and/or modeled stressor metadata, sampled at the target site, and marked for inclusion by the user (UseInStressorID = 1 in the stressor metadata)"),
					# 			 placement = "right", 
					# 			 trigger = "hover"),
					
					#DT::dataTableOutput("df_candcause_all_DT"),
					pre(textOutput("df_candcause_all_DT")),
					br(),
					uiOutput("elimUI")
					# h4(paste0("Stressor(s) eliminated by comparison of ",
					# 					 "\ntarget and comparator sample values")),
					# #DT::dataTableOutput("df_candcause_elim_DT")
					# pre(textOutput("df_candcause_elim_DT"))
					)## mainPanel
				)## tabPanel
}## FUNCTION