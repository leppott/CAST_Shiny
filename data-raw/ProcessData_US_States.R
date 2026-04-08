# Process Data - US State boundaries
# Erik.Leppo@tetratech.com
# 2025-08-13 (HAB-DW-Risk-Viewer)
# 2026-04-08, copy over
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Avoid having to download each time shiny app is run
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Packages ----
library(tigris)
library(sf)
library(dplyr)

# Global
options(tigris_use_cache = TRUE)

# Download state boundaries
sf_states <- tigris::states(cb = TRUE, class = "sf")

# Change projection
# leaflet wants WGS84
sf_states <- sf::st_transform(sf_states, 4326) # 0.6 seconds

# Save to an RDS file
saveRDS(sf_states, 
		  file = file.path("apps", 
			  					 "CASTool_USEPA", 
			  					 "external", 
		  					    "data",
			  					 "states_sf.rds"))

# # in GLOBAL to load it back
# sf_states <- readRDS("states_sf.rds")

# create CSV
states_drop <- c("DC", "AS", "GU", "MP", "PR", "VI", "AK", "HI")
sf_states |>
	# drop geometry
	sf::st_drop_geometry() |>
	# CONUS only (no DC)
	dplyr::filter(!STUSPS %in% states_drop) |>
	write.csv(file = file.path("apps",
										"CASTool_USEPA",
										"external",
										"data",
										"states.csv"),
				 row.names = FALSE)

	

