NEWS
================
<Erik.Leppo@tetratech.com>

<!-- NEWS.md is generated from NEWS.Rmd. Please edit that file -->

    #> Last Update: 2025-12-22 10:57:44.613392

# CAST_Shiny 0.0.2.9020 (2025-12-22)

- refactor: Update links to download files

# CAST_Shiny 0.0.2.9019 (2025-12-22)

- fix: Restore some directories in global had marked an extra/not needed
- files: Add files for download links
  - CASTool_UserGuide.pdf
  - CASTool_Templates.zip
- refactor: Update links to download files

# CAST_Shiny 0.0.2.9018 (2025-12-19)

- refactor: Change utils::unzip to zip::zip for use on ShinyApps.io

# CAST_Shiny 0.0.2.9017 (2025-12-19)

- refactor: getReport report render
  - Add RMD files from package to shiny app directory at run time
- refactor: Prep code for test hosting on ShinyApps.io

# CAST_Shiny 0.0.2.9016 (2025-12-19)

- refactor: Create RMD fragment for Stressor Summaries
  - Add to WWW folder
  - Add render to end of get report button
  - Overwrite RMD output at start up in global
  - Add reactive to update Shiny when file changes
- refactor: Remove get clusters button from Setup tab
- refactor: Add selected site id to report tab
- refactor: Update watershed stressors
  - Change to match Powerpoint mock up
  - Remove existing content
  - Add text box for reaches to display
  - Add select box for variables
  - Update button to display figure
  - Update download button
  - Add zip code to end of Report
- fix: Add sf package to DESCRIPTION and global.R
- refactor: Update Watershed Stressors tab
  - Pull down for watershed variable label
  - Display of figure

# CAST_Shiny 0.0.2.9015 (2025-12-17)

- refactor: Add download button for report results
- refactor: Unhide Explore Watershed Stressors tab based on metadata
  file
- refactor: Set up tab ‘map’ to ‘abiotic clustering’
- refactor: Add data gaps file to data gaps tab
- refactor: Add items to candidate causes tab
- refactor: Add items to weight of evidence causes tab

# CAST_Shiny 0.0.2.9014 (2025-12-17)

- refactor: Update checkInputs zip file creation for new directory
  structure
- docs: Update DESCRIPTION package title
- refactor: Update checkinputs file contents to reactive text boxes
- refactor: Update setup explore watershed stressors to reactive text
  box
- refactor: Add placeholder link on About to manual
- refactor: Add placeholder link on checkfiles to templates

# CAST_Shiny 0.0.2.9013 (2025-12-16)

- refactor: Update for changes in checkInputs
  - Add package CASToolBaseDataPckg to DESCRIPTION
  - Drop df_targets as input for checkInputs
- refactor: Remove ‘results’ directory from table 1 and 2 file creation

# CAST_Shiny 0.0.2.9012 (2025-12-05)

- refactor: Add user defined map to Set Up Tool tab

# CAST_Shiny 0.0.2.9011 (2025-12-04)

- refactor: Remove comparator info from Set Up Tool tab
- refactor: Update explore watershed stressor radio button based on
  metadata

# CAST_Shiny 0.0.2.9010 (2025-12-04)

- refactor: Update set up select target site to not auto select the firs
  site
- refactor: Add shiny alert for no site selected for run report
- refactor: Include any extra user files in RDS check files download zip
  file

# CAST_Shiny 0.0.2.9009 (2025-12-04)

- refactor: Update check files uploaded files check boxes based on files
  present in user files

# CAST_Shiny 0.0.2.9008 (2025-12-04)

- refactor: Add variable for check files qc tables row colors to global
- refactor: Add bold font for false results to check files qc tables
- refactor: Trigger to stop process when check files if any missing
  - Add Shiny alert

# CAST_Shiny 0.0.2.9007 (2025-12-02)

- fix: Update check files qc table displays

# CAST_Shiny 0.0.2.9006 (2025-12-02)

- refactor: Updates for run report
- refactor: Change directory structure for check files and run report
- fix: Download buttons for check files
- refactor: Update check files zip to include RDS and check tables
  - Tables needed for run report

# CAST_Shiny 0.0.2.9005 (2025-11-13)

- refactor: Update clean_dir helper function
  - Option for include directories
- style: Add roxygen styling to clean_dir function

# CAST_Shiny 0.0.2.9004 (2025-10-10)

- refactor: Add check files to Shiny
- refactor: Update server code for check files to pull targets from
  metadata
- refactor: Add shinyalerts package for pop up notifications
- docs: Update DESCRIPTION Imports

# CAST_Shiny 0.0.2.9003 (2025-10-08)

- refactor: Add placeholder for running skeleton code
- refactor: Disable and enable buttons
  - Report

# CAST_Shiny 0.0.2.9002 (2025-09-24)

- refactor: Change names of folder in app to match skeleton code

# CAST_Shiny 0.0.2.9001 (2025-09-04)

- refactor: Move Shiny apps from `CASTfxn` repo to this repo

# CAST_Shiny 0.0.1.9017 (2025-08-25)

- refactor: Add CASToolClusterPckg

# CAST_Shiny 0.0.1.9016 (2025-08-22)

- fix: Correct typo in zip command for wshed stressors GitHub download

# CAST_Shiny 0.0.1.9015 (2025-08-22)

- refactor: Change from zip::zip to utils::zip
  - zip::zip not working on ShinyApps.io

# CAST_Shiny 0.0.1.9014 (2025-08-22)

- refactor: Ensure data and result folders exist on start up

# CAST_Shiny 0.0.1.9013 (2025-08-21)

- refactor: Update select input of target sites based on checked files

# CAST_Shiny 0.0.1.9012 (2025-08-21)

- refactor: Rework how uploaded files are checked for inclusion in
  metadata

# CAST_Shiny 0.0.1.9011 (2025-08-19)

- refactor: Address comments 2025-08-18 on 2025-08-07 version of app

# CAST_Shiny 0.0.1.9010 (2025-08-19)

- feature: Update Shiny Apps prep routine + Add blank file before upload
  to ShinyApps.io - Upload does not transfer empty folders

# CAST_Shiny 0.0.1.9009 (2025-08-19)

- refactor: Address comments 2025-08-18 on 2025-08-07 version of app

# CAST_Shiny 0.0.1.9008 (2025-08-07)

- refactor: Change utils::unzip to zip::unzip for use on ShinyApps.io

# CAST_Shiny 0.0.1.9007 (2025-08-07)

- feat: Build out most of initial wireframe design

# CAST_Shiny 0.0.1.9006 (2025-07-29)

- refactor: Change case of text to be consistent for buttons and titles

# CAST_Shiny 0.0.1.9005 (2025-07-29)

- fix: Add shinyBS library to global

# CAST_Shiny 0.0.1.9004 (2025-07-29)

- refactor: Create wireframe with placeholders from 2025-06-02
  Storyboard

# CAST_Shiny 0.0.1.9003 (2025-06-03)

- docs: Update README badges

# CAST_Shiny 0.0.1.9002 (2025-06-03)

- docs: Added and updated documentation to support GitHub repo
  - DESCRIPTION
  - NEWS
  - README

# CAST_Shiny 0.0.1.9001 (2025-06-03)

- Initialized repo on GitHub
