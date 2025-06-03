# Oregon Offshore Wind Project - Create Sample Dataset for GitHub
# This script reduces the full NREL wind dataset to ~10% of sites and 4 months of data
# Final output should be ~12MB, well under GitHub's 50MB limit

# Load required libraries
library(dplyr)
library(readr)
library(stringr)

# =============================================================================
# CONFIGURATION - UPDATE THESE PATHS
# =============================================================================

# Path to your extracted NREL data folder (the one with the hex name)
nrel_data_path <- "Data/WindData"  # UPDATE THIS PATH

# Output directory for reduced dataset
output_dir <- "Data/WindDataSubset"

# =============================================================================
# STEP 1: FIND AND FILTER CSV FILES
# =============================================================================

cat("Step 1: Finding CSV files...\n")

# Get all CSV files
csv_files <- list.files(nrel_data_path, pattern = "\\.csv$", full.names = TRUE)
cat("Total CSV files found:", length(csv_files), "\n")

# Extract site information from filenames
site_info <- tibble(
  file_path = csv_files,
  filename = basename(csv_files),
  site_id = str_extract(filename, "^\\d+"),
  lat = as.numeric(str_extract(filename, "(?<=_)\\d+\\.\\d+")),
  lon = as.numeric(str_extract(filename, "(?<=_)-\\d+\\.\\d+"))
)

cat("Latitude range:", min(site_info$lat), "to", max(site_info$lat), "\n")
cat("Longitude range:", min(site_info$lon), "to", max(site_info$lon), "\n")

# =============================================================================
# STEP 2: FILTER TO LEASE AREAS
# =============================================================================

cat("\nStep 2: Filtering to lease areas...\n")

# Define lease area boundaries
coos_bay_sites <- site_info %>%
  filter(lat >= 43.8 & lat <= 44.3 & lon >= -125.5 & lon <= -124.8)

brookings_sites <- site_info %>%
  filter(lat >= 42.0 & lat <= 42.8 & lon >= -124.8 & lon <= -124.3)

lease_area_sites <- bind_rows(coos_bay_sites, brookings_sites)

cat("Lease area sites found:", nrow(lease_area_sites), "\n")
cat("  - Coos Bay sites:", nrow(coos_bay_sites), "\n")
cat("  - Brookings sites:", nrow(brookings_sites), "\n")

# =============================================================================
# STEP 3: SAMPLE SITES (10% with GEOGRAPHIC STRATIFICATION)
# =============================================================================

cat("\nStep 3: Sampling sites...\n")

# Sample ~10% of sites with good geographic distribution
set.seed(123)  # For reproducibility

sample_sites <- lease_area_sites %>%
  group_by(
    lat_bin = round(lat, 1),  # Group by 0.1 degree latitude bins
    lon_bin = round(lon, 1)   # Group by 0.1 degree longitude bins
  ) %>%
  slice_sample(prop = 0.1) %>%  # Sample 10% from each geographic bin
  ungroup()

cat("Sample sites selected:", nrow(sample_sites), "\n")

# Show distribution
cat("Latitude distribution:\n")
print(table(round(sample_sites$lat, 1)))

# =============================================================================
# STEP 4: PROCESS FILES (KEEP 4 MONTHS: JAN, APR, JUL, OCT)
# =============================================================================

cat("\nStep 4: Processing files...\n")

# Create output directory
dir.create(output_dir, showWarnings = FALSE)

# Function to process each CSV file
process_csv_file <- function(file_path, output_dir) {
  # Read all lines
  all_lines <- readLines(file_path)
  
  # First line is metadata (keep as-is)
  metadata_line <- all_lines[1]
  
  # Second line is headers
  header_line <- all_lines[2]
  
  # Data starts from line 3
  data_lines <- all_lines[3:length(all_lines)]
  
  # Parse the data part
  data_text <- c(header_line, data_lines)
  data <- read_csv(I(paste(data_text, collapse = "\n")), show_col_types = FALSE)
  
  # Filter to specific months (1=Jan, 4=Apr, 7=Jul, 10=Oct)
  filtered_data <- data %>%
    filter(Month %in% c(1, 4, 7, 10))
  
  # Create output file with exact NREL format
  output_file <- file.path(output_dir, basename(file_path))
  
  # Write the file in original NREL format:
  # Line 1: Metadata
  # Line 2: Headers  
  # Line 3+: Data
  writeLines(c(metadata_line, header_line), output_file)
  write_csv(filtered_data, output_file, append = TRUE, col_names = FALSE)
  
  return(nrow(filtered_data))
}

# Process all sample files
processed_counts <- vector("integer", nrow(sample_sites))
start_time <- Sys.time()

for(i in 1:nrow(sample_sites)) {
  if(i %% 10 == 0) cat("  Processing file", i, "of", nrow(sample_sites), "\n")
  
  processed_counts[i] <- process_csv_file(sample_sites$file_path[i], output_dir)
}

end_time <- Sys.time()

# =============================================================================
# STEP 5: CREATE FINAL OUTPUTS
# =============================================================================

cat("\nStep 5: Creating final outputs...\n")

# Processing summary
cat("Processing completed in", round(difftime(end_time, start_time, units = "mins"), 2), "minutes\n")
cat("Total data rows processed:", sum(processed_counts), "\n")
cat("Average rows per file:", round(mean(processed_counts)), "\n")

# Check output directory size
output_files <- list.files(output_dir, full.names = TRUE)
total_size_mb <- sum(file.size(output_files)) / (1024^2)
cat("Total output size:", round(total_size_mb, 1), "MB\n")

# Create a ZIP file for GitHub
zip_file <- "Data/WindDataSubset.zip"

# Ensure the directory exists
dir.create(dirname(zip_file), showWarnings = FALSE, recursive = TRUE)

# Try different ZIP approaches
if (file.exists(zip_file)) file.remove(zip_file)

# Method 1: Try utils::zip with explicit method
zip_result <- tryCatch({
  utils::zip(zip_file, files = basename(output_files), flags = "-r9Xj", 
             extras = paste("-C", output_dir))
  "success"
}, error = function(e) {
  # Method 2: Try basic zip
  tryCatch({
    utils::zip(zip_file, files = output_files)
    "success"
  }, error = function(e) {
    # Method 3: Manual system call (cross-platform)
    if (Sys.info()["sysname"] == "Windows") {
      system(paste0('powershell Compress-Archive -Path "', output_dir, '/*" -DestinationPath "', zip_file, '"'))
    } else {
      system(paste0('cd "', dirname(output_dir), '" && zip -r "', zip_file, '" "', basename(output_dir), '"'))
    }
    if (file.exists(zip_file)) "success" else "failed"
  })
})

# Check if ZIP was created and get size
if (file.exists(zip_file)) {
  zip_size_mb <- file.size(zip_file) / (1024^2)
  cat("Final ZIP file created successfully:", round(zip_size_mb, 1), "MB\n")
} else {
  cat("ZIP file creation failed. Manual creation needed.\n")
  cat("You can manually ZIP the '", output_dir, "' directory\n")
  zip_size_mb <- NA
}

# Create documentation of sample sites
sample_summary <- sample_sites %>%
  select(site_id, lat, lon) %>%
  mutate(
    lease_area = ifelse(lat >= 43.8, "Coos Bay", "Brookings"),
    distance_from_shore_approx = case_when(
      lon >= -124.5 ~ "Close",
      lon >= -125.0 ~ "Medium", 
      TRUE ~ "Far"
    )
  )

# Save the sample site documentation
write_csv(sample_summary, "Data/WindDataSubsetSiteList.csv")

# =============================================================================
# FINAL SUMMARY
# =============================================================================

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("SAMPLE DATASET CREATION COMPLETE\n")
cat(paste(rep("=", 60), collapse = ""), "\n")
cat("Original data: ~", length(csv_files), "files covering broader region\n")
cat("Lease area sites:", nrow(lease_area_sites), "\n")
cat("Sample sites:", nrow(sample_sites), "(", round(100*nrow(sample_sites)/nrow(lease_area_sites), 1), "%)\n")
cat("Temporal coverage: 4 months (Jan, Apr, Jul, Oct) of 2019\n")
cat("Final ZIP size:", ifelse(is.na(zip_size_mb), "Manual ZIP needed", paste(round(zip_size_mb, 1), "MB")), "(target: <50MB for GitHub)\n")

cat("\nSample distribution by lease area:\n")
print(table(sample_summary$lease_area))

cat("\nFiles created:\n")
cat("  1. NREL_Wind_Sample_Data.zip - Reduced dataset for GitHub\n")
cat("  2. sample_sites_list.csv - Documentation of selected sites\n")
cat("  3. WindDataSubset/ - Directory with individual CSV files\n")

cat("\n", paste(rep("=", 60), collapse = ""), "\n")