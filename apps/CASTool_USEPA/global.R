# Global
#
# Erik.Leppo@tetratech.com
# 2025-06-03
#~~~~~~~~~~~~~~~~~~~~~~~~

# Version ----
pkg_version <- "1.0.0.9001"

# Packages ----
library(shiny)
library(shinyjs) # hide buttons
library(shinyBS) # nice buttons
library(shinydashboard)
library(knitr)
library(DT)      # nice tables
library(bsplus)  # tooltips
# CASTool pkgs
library(CASTfxn)
library(CASToolClusterPckg) 
library(CASToolWSStressorPckg)
library(CASToolBaseDataPckg)
library(sf)
library(kableExtra)

# library(zip)	  # use `utils` as `zip` pkg doesn't work on ShinyApps.io

# from renv::dependencies()
# library(httr)
# library(rmarkdown)
# library(rsconnect)

# library(shinydashboardPlus) # only using for footer
# library(ggplot2)
# library(shinycssloaders)
# library(bslib)
# library(shinythemes)
# library(shinyalert)  # alerts

# Source ----
## Skeleton Code
boo_Shiny <- TRUE
# boo.debug <- TRUE
# debug.person <- "Erik"
path_skelcode <- file.path(system.file(package = "CASTfxn"),
									"shiny-examples", 
									"CASTool",
									"CASTool.r")


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
not_all_na <- function(x) {!all(is.na(x))}

# Tabs ----
tab_code_about       <- source("external/tab_about.R",
										 local = TRUE)$value
tab_code_checkfiles  <- source("external/tab_checkfiles.R", 
										 local = TRUE)$value
tab_code_setup       <- source("external/tab_setup.R", 
										 local = TRUE)$value
tab_code_report      <- source("external/tab_report.R", 
										 local = TRUE)$value
# # not always visible
tab_code_wshedstress <- source("external/tab_wshedstress.R",
										 local = TRUE)$value
tab_code_candcause   <- source("external/tab_candcause.R", 
										 local = TRUE)$value
tab_code_woesumm     <- source("external/tab_woesumm.R",
										 local = TRUE)$value
tab_code_stresssumm  <- source("external/tab_stresssumm.R", 
										 local = TRUE)$value
tab_code_gaps        <- source("external/tab_gaps.R", 
										 local = TRUE)$value

# Global ----
## Directories ----
### root
dn_data      <- "Data"
dn_results   <- "Results"
#### data
dn_checked   <- "checked"
dn_clusters  <- "clusters"
dn_import    <- "input"
dn_temp      <- "temp"
dn_ws_stress <- "ws_stress"
#### results
dn_bmi       <- "BMI"
dn_woe       <- "_WoE"
#### skeleton
dn_checked_sk <- "_CheckedInputs"

## Clean Dirs ----
clean_dir(file.path(dn_data), boo_dir = TRUE)
clean_dir(file.path(dn_results), boo_dir = TRUE)

## Create Dirs ----
create_dir(file.path(dn_data))
create_dir(file.path(dn_data, dn_checked))
# create_dir(file.path(dn_data, dn_clusters))
create_dir(file.path(dn_data, dn_import))
# create_dir(file.path(dn_data, dn_temp))
# create_dir(file.path(dn_data, dn_ws_stress))
create_dir(file.path(dn_results))
# create_dir(file.path(dn_results, dn_bmi))
# create_dir(file.path(dn_results, dn_woe))

## Colors ----
color_good <- "lightblue"
color_bad  <- "orange"

## GitHub, Wshed ----
url_ws_stress_base <- "https://github.com/leppott/CAST_SupportFiles/raw/main/data"
url_ws_stress_zip <- file.path(url_ws_stress_base, 
										 "StreamCat_data_WA_test.zip")
url_ws_stress_info <- file.path(url_ws_stress_base,
										  "StreamCat_stressor-info_WA.csv")

# Choices, Check Boxes ----
choices_chk_check_comm <- c("Algae", "Macroinvertebrates", "Fish")
choices_chk_check_stress <- c("Measured", "Modeled")
choices_chk_check_tol <- c("Algae", "Macroinvertebrates", "Fish")

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

# GitHub Files ----
url_github_castfxn <- "https://raw.githubusercontent.com/leppott/CASTfxn/main/inst/extdata"
url_github_castshiny <- "https://raw.githubusercontent.com/leppott/CAST_Shiny/main/apps/CASTool_USEPA/www"

# WWW----
dn_fc <- file.path("www", "RMD_HTML")
fn_from <- file.path(dn_fc, "html_frag_blank.html")
fn_to <- file.path(dn_fc, "ShinyHTML_StressSumm.html")
file.copy(from = fn_from, to = fn_to, overwrite = TRUE)

# WSStressors ----
# fn_wsstressors <- file.path("www", "CASTool_WSStressors.csv")
# df_wsstressors <- read.csv(fn_wsstressors)
