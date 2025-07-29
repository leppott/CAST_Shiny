# Global
#
# Erik.Leppo@tetratech.com
# 2025-06-03
#~~~~~~~~~~~~~~~~~~~~~~~~

# Version ----
pkg_version <- "0.0.0.9005"

# Packages ----
library(shiny)
library(shinyjs) # hide buttons
library(shinyBS) # nice buttons

# library(shinydashboard)
# library(shinydashboardPlus) # only using for footer
# library(ggplot2)
# library(shinycssloaders)
# library(bslib)
# library(shinythemes)

# Max File Upload Size ----
#Maximum individual file size that can be uploaded is 70 MB
options(shiny.maxRequestSize = 70 * 1024^2)

# Template Code ----

# options(spinner.color.background = "#ffffff", spinner.size = 1)

# app_jscode <-
# 	"shinyjs.disableTab = function(name) {
#     var tab = $('.nav li a[data-value=' + name + ']');
#     $(tab).css({'visibility' : 'hidden' })
#    // $(tab).hide();
#   }
#   shinyjs.enableTab = function(name) {
#     var tab = $('.nav li a[data-value=' + name + ']');
#     $(tab).css({'visibility' : 'visible' })
#     // $(tab).show();
#   }"

# Functions ----

# Tabs ----
tab_code_about       <- source("external/tab_about.R", local = TRUE)$value
tab_code_checkfiles  <- source("external/tab_checkfiles.R", local = TRUE)$value
tab_code_setup       <- source("external/tab_setup.R", local = TRUE)$value
tab_code_report      <- source("external/tab_report.R", local = TRUE)$value
# # not always visible
tab_code_wshedstress <- source("external/tab_wshedstress.R", local = TRUE)$value
tab_code_candcause   <- source("external/tab_candcause.R", local = TRUE)$value
tab_code_woesumm     <- source("external/tab_woesumm.R", local = TRUE)$value
tab_code_stresssumm  <- source("external/tab_stresssumm.R", local = TRUE)$value
tab_code_gaps        <- source("external/tab_gaps.R", local = TRUE)$value

# Global ----
## Directories ----
dn_data    <- "data"
dn_results <- "results"

## Colors ----