# WoE Summary

function() {
	tabPanel("WoE Summary",
				value = "tab_woesumm",
				#mainPanel(
					# h2("Biological Index Distributions"),
					# imageOutput("img_bio_index",
					# 				width = "100%",
					# 				height = "100%"),
					# h2("Lines of Evidence Summary"),
					# DT::dataTableOutput("tbl_woe_summ"),
					# br(),
					# imageOutput("img_loe_summ",
					# 				width = "100%",
					# 				height = "100%"),
					# h2("Weight of Evidence Table"),
					# DT::dataTableOutput("tbl_woe")
				
				uiOutput("woe_tab_ui")
				#	)## mainPanel
				)## tabPanel
}## FUNCTION