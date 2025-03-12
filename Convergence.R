#install.packages("devtools")
library(devtools)
#install_github("lfabreti/convenience")
library(convenience)
library(tidyverse)

options(scipen=999)

#----------------------------- Process file ------------------------------------
# Before loading your Beast 1 trees, you must remove all lines except for 2 which precede your log file's header...
# GV: for example, you're removing these lines:
  # BEAST v1.10.4 Prerelease #bc6cbd9
  # Generated Fri Dec 13 12:18:31 EST 2024 [seed=1734110304711]

process_log_files <- function(file_paths, output_dir) {
  # Ensure the output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # List all files in the file_paths folder (no filtering by pattern)
  all_files <- list.files(file_paths, full.names = TRUE)
  
  new_paths <- c()  # Initialize an empty character vector
  
  # Process each file
  for (file_path in all_files) {
    # Read the file
    lines <- readLines(file_path)
    
    # Find the indices of lines that start with #
    comment_indices <- grep("^#", lines)
    num_comments <- length(comment_indices)
    
    # Keep only the last 2 comment lines if there are more than 2
    if (num_comments > 2) {
      lines <- c(lines[(comment_indices[num_comments - 1]):(comment_indices[num_comments])], 
                 lines[-comment_indices])
    }
    
    # Modify the output filename to include ".removed" before the last extension
    output_file <- file.path(
      output_dir, 
      sub("(\\.log\\.txt)$", ".removed.log.txt", basename(file_path))
    )
    
    # Write the modified file out
    writeLines(lines, output_file)
    
    # Append the new file path to new_paths
    new_paths <- c(new_paths, output_file)
  }
  
  return(new_paths)  # Return new_paths as a vector of output file paths
}

# Example usage
file_paths <- "/scratch/gev25289/workdir/georgia/wastewater/analysis1/clarke_632/beast/skyride/ucldprior0.001/log files"
output_dir <- "/scratch/gev25289/workdir/georgia/wastewater/analysis1/clarke_632/beast/skyride/ucldprior0.001/convenience log files"

# Process all files in the folder
new_paths <- process_log_files(file_paths, output_dir)

# Get the list of files in the output directory
files_in_output_dir <- list.files(output_dir, full.names = TRUE)

# CheckConvergence looks for .log files
# Loop through the files and rename them from .log.txt to .log
for (file in files_in_output_dir) {
  # Create the new file name by replacing ".log.txt" with ".log"
  new_file <- sub("\\.log\\.txt$", ".log", file)
  # Rename the file
  file.rename(file, new_file)
}

# Update
files_in_output_dir <- list.files(output_dir, full.names = TRUE)

# Download folder in mobaxterm for tracer examination: 
# scp -r "gev25289@xfer.gacrc.uga.edu:/scratch/gev25289/workdir/georgia/wastewater/analysis3/beast_subsample/subsample1/skyride/convenience log files" .
 
#------------------------------ Check convergence ------------------------------
setwd("/scratch/gev25289/workdir/georgia/wastewater/analysis3/beast_subsample/subsample1/skyride/")
output <- checkConvergence(format = "beast", 
                           #path = output_dir,
                           list_files = c("convenience log files/run10.subsample1_output_mismatch4_trim.removed.log", 
                                          "run10/subsample1_output_mismatch4_trim.trees.txt"),
                           control = makeControl(
                             tracer = "TRUE"
                             #burnin = NULL
                             ))
convergencetable <- convenience::printTableContinuous(output) #printConvergenceTable is not in package
output$burnin
output$converged
output$failed

#These ESS values are very different than in Tracer -- is it automatically removing burn-in without reporting it?

run10trace <- convenience::readTrace(files_in_output_dir[1],
                       delim = "\t",
                       skip = 0,
                       burnin = 0)

run10 <- read.delim(files_in_output_dir[1], sep = "\t", skip = 2)
run10_burnin.10 <- run10 %>% filter(state >= 20000000) 
  
ggplot(run10_burnin.10, aes(x = state, y = likelihood)) +
  geom_line() +  # Line plot for the trace
  labs(x = "State", y = "likelihood") +
  theme_minimal() +
  ggtitle("MCMC Trace Plot")

library(dplyr)
library(ggplot2)
library(patchwork)

# Function to process each file and create the plot
process_and_plot <- function(file_path, burnin) {
  # Read the file (skip first 2 lines)
  data <- read.delim(file_path, sep = "\t", skip = 2)  # remove commented-out rows
  
  # Filter the data (remove the burn-in)
  burnin_data <- data %>% filter(state >= burnin)
  
  # Extract run number from the file path (assuming "run" is followed by the number)
  run_number <- sub(".*run(\\d+).*", "Run\\1", file_path)
  
  # Create the plot
  p <- ggplot(burnin_data, aes(x = state, y = likelihood)) +
    geom_line() +  # Line plot for the trace
    labs(x = "State", y = "Likelihood") +
    theme_minimal() +
    ggtitle(paste(run_number, "MCMC Trace Plot\nBurn-in:", burnin))  # Set the plot title
  
  # Return the plot
  return(p)
}

# Loop through the files and generate the plots with burn-in set to 20 million
plots <- lapply(files_in_output_dir, process_and_plot, burnin = 20000000)

# Combine all the individual plots into one using patchwork
combined_plot <- wrap_plots(plots, ncol = 3)  # Adjust the number of columns as needed

# Display combined plot
print(combined_plot)

library(epinet)
ess(run10$likelihood, burninProportion = 0.1)
