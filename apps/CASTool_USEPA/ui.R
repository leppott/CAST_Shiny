#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Define UI for application
navbarPage("CASTool",
			  id = "navbar",
			  useShinyjs(),     # activate Shinyjs commands
			  use_bs_tooltip(), # activate bsplus tooltips
			  # theme = shinytheme("spacelab"),
			  tab_code_about(),
			  tab_code_checkfiles(),
			  tab_code_setup(),
			  tab_code_report(),
			  tab_code_wshedstress(),
			  tab_code_candcause(),
			  tab_code_woesumm(),
			  tab_code_stresssumm(),
			  tab_code_gaps()
)## navbarPage ~ END
