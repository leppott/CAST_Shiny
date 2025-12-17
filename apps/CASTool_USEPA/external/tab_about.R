# About

function() {
	tabPanel("Overview",
				mainPanel(
					p(paste0("Version ", pkg_version, ".")),
					includeHTML("www/RMD_HTML/txt_about.html"),
					shiny::hr(),
					# download user guide
					a(href = url_github_castfxn,
					  class = "btn bnh-primary",
					  target = "_blank",
					  download = "901SJSJC9_CoOccurrence_20180802_123825.pdf",
					  "Download User Guide")
					)## mainPanel
				)## tabPanel
}## FUNCTION