# WoE Summary

function() {
	tabPanel("WoE Summary",
				value = "tab_woesumm",
				mainPanel(
					h2("Biological Index Distributions"),
					imageOutput("img_bio_index",
									width = "50%",
									height = "auto"),
					h2("Lines of Evidence Summary"),
								 DT::dataTableOutput("tbl_woe_summ"),
					br(),
					br(),
					p("New LOE summary fig, results/biocommunity _WOE folder, 20251120"),
					br(),
					h2("Weight of Evidence Table"),
								 DT::dataTableOutput("tbl_woe")
					)## mainPanel
				)## tabPanel
}## FUNCTION