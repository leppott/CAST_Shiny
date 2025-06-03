README-LakeMonitoR
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

    #> Last Update: 2025-06-03 11:53:35.305914

# CAST_Shiny

Shiny app for CASTool

# Badges

[![Maintenance](https://img.shields.io/badge/Maintained%3F-yes-green.svg)](https://GitHub.com/leppott/CAST_Shiny/graphs/commit-activity)

[![Lifecycle](https://img.shields.io/badge/Lifecycle-Proof%20of%20Concept-blueviolet)](https://github.com/Camunda-Community-Hub/community/blob/main/extension-lifecycle.md#proof-of-concept-)

[![License:
MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://cran.r-project.org/web/licenses/MIT)

[![GitHub
issues](https://img.shields.io/github/issues/leppott/LakeMonitoR.svg)](https://GitHub.com/leppott/CAST_Shiny/issues/)

[![GitHub
release](https://img.shields.io/github/release/leppott/LakeMonitoR.svg)](https://GitHub.com/leppott/CAST_Shiny/releases/)
[![Github all
releases](https://img.shields.io/github/downloads/leppott/LakeMonitoR/total.svg)](https://GitHub.com/leppott/CAST_Shiny/releases/)

# Installation

To install the current version of the code from GitHub use the example
below.

``` r
if(!require(remotes)){install.packages("remotes")}  #install if needed
remotes::install_github("leppott/CAST_Shiny")
```

# Purpose

Provide Shiny interface for CAST tools.

# Issues

<https://github.com/leppott/CAST_Shiny/issues>

# Documentation

None at this time. Shiny app only.

# Shiny App

A Shiny app is included in the package.

The online version is hosted at
<https://tetratech-wtr-wne.shinyapps.io/CASTool_USEPA>

The Shiny app can be run from R console using the shiny package without
installing the package.

``` r
if(!require(shiny)){install.packages("shiny")}
shiny::runGitHub(repo = "CAST_Shiny"
                , username = "leppott"
                , ref = "main"
                , subdir = "apps/CASTool_USEPA")
```
