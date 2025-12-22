# About

function() {
	tabPanel("Overview",
				mainPanel(
					p(paste0("Version ", pkg_version, ".")),
					includeHTML("www/RMD_HTML/txt_about.html"),
					shiny::hr(),
					# downloadButton("dload_about_userguide", "Download User Guide"),
					# download user guide
					shiny::a(href = paste0(url_github_castshiny,
										 "/",
										 "CASTool_UserGuide.pdf"),
					  target = "_blank",
					  class = "btn bnh-primary",
					  download = "CASTool_UserGuide.pdf",
					  "Download User Guide")
					)## mainPanel
				)## tabPanel
}## FUNCTION