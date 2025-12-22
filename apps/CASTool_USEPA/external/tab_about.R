# About

function() {
	tabPanel("Overview",
				mainPanel(
					p(paste0("Version ", pkg_version, ".")),
					includeHTML("www/RMD_HTML/txt_about.html"),
					shiny::hr(),
					# download user guide
					a(href = url_github_castshiny,
					  class = "btn bnh-primary",
					  target = "_blank",
					  download = "CASTool_UserGuide.pdf",
					  "Download User Guide")
					)## mainPanel
				)## tabPanel
}## FUNCTION