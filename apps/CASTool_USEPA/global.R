# Global
#
# Erik.Leppo@tetratech.com
# 2025-06-03
#~~~~~~~~~~~~~~~~~~~~~~~~

# Version ----
pkg_version <- "0.0.0.9007"

# Packages ----
library(shiny)
library(shinyjs) # hide buttons
library(shinyBS) # nice buttons
library(shinydashboard)
library(knitr)
library(DT)      # nice tables

# library(shinydashboardPlus) # only using for footer
# library(ggplot2)
# library(shinycssloaders)
# library(bslib)
# library(shinythemes)
# library(shinyalert)  # alerts

# Source ----

## Helper Functions ----
source(file.path("scripts", "helper_functions.R"))

# Max File Upload Size ----
#Maximum individual file size that can be uploaded is 300 MB
options(shiny.maxRequestSize = 300 * 1024^2)

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
### root
dn_data      <- "data"
dn_results   <- "results"
#### data
dn_import    <- "input"
dn_checked   <- "checked"
dn_ws_stress <- "ws_stress"
dn_clusters  <- "clusters"
dn_temp      <- "temp"
#### results
dn_bmi       <- "BMI"
dn_woe       <- "_WoE"

## Colors ----

## GitHub, Wshed ----
url_ws_stress_base <- "https://github.com/leppott/CAST_SupportFiles/raw/main/data"
url_ws_stress_zip <- file.path(url_ws_stress_base, 
										 "StreamCat_data_WA_test.zip")
url_ws_stress_info <- file.path(url_ws_stress_base,
										  "StreamCat_stressor-info_WA.csv")

# InfoBox Text ----
ib_check_metadata   <- "InfoBox (Check Files) Metadata"
ib_check_sites      <- "InfoBox (Check Files) Site File"
ib_check_mstress_d  <- "InfoBox (Check Files) measured stressor data"
ib_check_mstress_md <- "InfoBox (Check Files) measured stressor metadata"
ib_check_bmi_met_d  <- "InfoBox (Check Files) BMI metrics data"
ib_check_bmi_met_md <- "InfoBox (Check Files) BMI metrics metadata"
ib_check_bmi_cnt    <- "InfoBox (Check Files) BMI Counts"
ib_check_bmi_tax    <- "InfoBox (Check Files) BMI Taxa List"

# Import Check FileName Defaults ----
# n = 12
fn_default_check_input_cast_metadata <- "_CASTool_Metadata.xlsx"

# Report Hide Tabs ----
int_report <- 0