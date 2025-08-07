# Helper functions so can repeat code without repeating the code
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Erik.Leppo@tetratech.com
# 2023-11-06
# 2025-08-06, rewrite clean_dir() to be generic
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_dir <- function(dir_path, ...) {
	
	# Check if the directory exists
	if (!dir.exists(dir_path)) {
		stop("Directory does not exist.")
	}
	
	message(paste0("Directory = ",
						dir_path))
	
	# Files in dir
	fn_dir <- list.files(dir_path
								, full.names = TRUE
								, include.dirs = TRUE
								, recursive = TRUE)
	message(paste0("Files and folders (n) in directory; before removal = "
						, length(fn_dir)))
	
	# file.remove(fn_dir) # ok if no files and only files
	unlink(fn_dir, recursive = TRUE) # includes directories
	
	# QC, repeat
	fn_dir2 <- list.files(dir_path
								 , full.names = TRUE
								 , include.dirs = TRUE
								 , recursive = TRUE)
	message(paste0("Files and folders (n) in directory; after removal [should be 0] = "
						, length(fn_dir2)))
	
}## clean_dir

# clean_dir_results <- function() {
# 	# Remove results folder contents
# 	# Create subfolders
# 	# Copy input file
# 	
# 	# Remove all files in "Results" folder
# 	# Triggered here so can run different files
# 	fn_results <- list.files(dn_results
# 									 , full.names = TRUE
# 									 , include.dirs = TRUE
# 									 , recursive = TRUE)
# 	message(paste0("Files and folders in 'results' folder (before removal) = "
# 						, length(fn_results)))
# 	# file.remove(fn_results) # ok if no files and only files
# 	unlink(fn_results, recursive = TRUE) # includes directories
# 	# QC, repeat
# 	fn_results2 <- list.files(dn_results
# 									  , full.names = TRUE
# 									  , include.dirs = TRUE
# 									  , recursive = TRUE)
# 	message(paste0("Files in 'results' folder (after removal [should be 0]) = "
# 						, length(fn_results2)))
# 	
# }## clean_dir_results

# clean_dir_results <- function() {
# 	# Remove results folder contents
# 	# Create subfolders
# 	# Copy input file
# 	
# 	# Remove all files in "Results" folder
# 	# Triggered here so can run different files
# 	fn_results <- list.files(dn_results
# 									 , full.names = TRUE
# 									 , include.dirs = TRUE
# 									 , recursive = TRUE)
# 	message(paste0("Files and folders in 'results' folder (before removal) = "
# 						, length(fn_results)))
# 	# file.remove(fn_results) # ok if no files and only files
# 	unlink(fn_results, recursive = TRUE) # includes directories
# 	# QC, repeat
# 	fn_results2 <- list.files(dn_results
# 									  , full.names = TRUE
# 									  , include.dirs = TRUE
# 									  , recursive = TRUE)
# 	message(paste0("Files in 'results' folder (after removal [should be 0]) = "
# 						, length(fn_results2)))
# 	
# }## clean_dir_results
# 
# clean_dir_data_input <- function() {
# 	# Remove data folder contents
# 	# Create subfolders
# 	# Copy input file
# 	
# 	# Remove all files in "Data" folder
# 	# Triggered here so can run different files
# 	fn_results <- list.files(file.path(dn_data, dn_import)
# 									 , full.names = TRUE
# 									 , include.dirs = TRUE
# 									 , recursive = TRUE)
# 	message(paste0("Files and folders in 'data' folder (before removal) = "
# 						, length(fn_results)))
# 	# file.remove(fn_results) # ok if no files and only files
# 	unlink(fn_results, recursive = TRUE) # includes directories
# 	# QC, repeat
# 	fn_results2 <- list.files(file.path(dn_data, dn_import)
# 									  , full.names = TRUE
# 									  , include.dirs = TRUE
# 									  , recursive = TRUE)
# 	message(paste0("Files in 'data' folder (after removal [should be 0]) = "
# 						, length(fn_results2)))
# 	
# }## clean_dir_data_input

# Function to get file list from GitHub API
# shinyapps.io assisstant, 20250807
get_github_files <- function(owner, repo, path) {
	url <- paste0("https://api.github.com/repos/", 
					  owner, 
					  "/", 
					  repo, 
					  "/contents/", 
					  path)
	
	tryCatch({
		response <- GET(url)
		
		if (status_code(response) == 200) {
			content <- fromJSON(rawToChar(response$content))
			# Filter for files only (not directories)
			files <- content[content$type == "file", ]
			return(files)
		} else {
			return(NULL)
		}
	}, error = function(e) {
		return(NULL)
	})
}