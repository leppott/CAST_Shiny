# About

function() {
	tabPanel("Overview",
				mainPanel(
					p(paste0("Version ", pkg_version, ".")),
					h1("Causal Assessment Screening Tool"),
					shiny::a(href = paste0(url_github_castshiny,
												  "/",
												  "CASTool_UserGuide.pdf"),
								target = "_blank",
								class = "btn bnh-primary",
								download = "CASTool_UserGuide.pdf",
								"Download User Guide"),
					shiny::a(href = paste0(url_github_castshiny,
												  "/",
												  "CASTool_Templates.zip"),
								target = "_blank",
								class = "btn bnh-primary",
								download = "CASTool_Templates.zip",
								"Download Templates (zip)"),
					shiny::a(href = paste0(url_github_castshiny,
												  "/",
												  "CASTool_TestData.zip"),
								target = "_blank",
								class = "btn bnh-primary",
								download = "CASTool_TestData.zip",
								"Download Test Data (zip)"),
					shiny::hr(),
					includeHTML("www/RMD_HTML/txt_about.html"),

					# downloadButton("dload_about_userguide", "Download User Guide"),
					# download user guide
				
					)## mainPanel
				)## tabPanel
}## FUNCTION