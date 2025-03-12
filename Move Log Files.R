# Define root and target directories
root_dir <- "/scratch/gev25289/workdir/georgia/wastewater/analysis1/clarke_632/beast/skyride/ucldprior0.001/edit"
target_dir <- file.path(root_dir, "log files")

# Create the target folder if it doesn't exist
if (!dir.exists(target_dir)) {
  dir.create(target_dir, recursive = TRUE)
}

# List all .log.txt files, excluding "archive" subdirectories
log_files <- list.files(
  root_dir, 
  pattern = "\\.log\\.txt$", 
  recursive = TRUE, 
  full.names = TRUE
)

# Filter out paths that contain "/archive/"
log_files <- log_files[!grepl("/archive/", log_files)]

# Process each file
for (file_path in log_files) {
  # Extract run number (e.g., run1, run2) from the path
  run_folder <- basename(dirname(file_path))  # parent directory name
  
  # Extract original file name
  original_file_name <- basename(file_path)
  
  # Construct new file name with run number prefix
  new_file_name <- paste0(run_folder, ".", original_file_name)
  
  # Define full path to the new file location
  new_file_path <- file.path(target_dir, new_file_name)
  
  # Move and rename the file
  file.copy(file_path, new_file_path)
  
  # Print confirmation
  message("Moved: ", file_path, " --> ", new_file_path)
}


