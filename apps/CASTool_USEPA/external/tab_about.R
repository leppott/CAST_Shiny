# About

function() {
	tabPanel("Conceptual Foundation",
				mainPanel(
					p(paste0("Version ", pkg_version, ".")),
					includeHTML("www/RMD_HTML/txt_about.html")
					)## mainPanel
				)## tabPanel
}## FUNCTION