library(dplyr)

# The wind data was downloaded overnight as a single ZIP file. It's in the 
# data directory. All the files were extracted into a directory called
# WindData

# Function to read and process a single wind file
read_wind_file_final <- function(filepath) {
  # Extract coordinates from filename
  filename <- basename(filepath)
  filename_parts <- strsplit(filename, "_")[[1]]
  
  site_id <- filename_parts[1]
  lat <- as.numeric(filename_parts[2])
  lon <- as.numeric(filename_parts[3])
  
  # Read the file skipping the metadata row, using row 2 as headers
  df <- read.csv(filepath, stringsAsFactors = FALSE, skip = 1, header = TRUE)
  
  n_rows <- nrow(df)
  
  clean_df <- data.frame(
    site_id = rep(site_id, n_rows),
    latitude = rep(lat, n_rows),
    longitude = rep(lon, n_rows),
    year = as.numeric(df[,1]),
    month = as.numeric(df[,2]),
    day = as.numeric(df[,3]),
    hour = as.numeric(df[,4]),
    minute = as.numeric(df[,5]),
    windspeed_120m = as.numeric(df[,6]),
    temperature_2m = as.numeric(df[,7]),
    pressure_0m = as.numeric(df[,8])
  )
  
  return(clean_df)
}

# Get list of all wind files
wind_files <- list.files(path = "Data/WindData", 
                         pattern = "*.csv", 
                         full.names = TRUE)

cat("Found", length(wind_files), "wind data files\n")

# Process all files with progress tracking
cat("Processing all wind data files...\n")

# METHOD 1: Using lapply (more R-like)
all_wind_data <- bind_rows(lapply(wind_files, read_wind_file_final))

# Check the results
cat("\n=== PROCESSING COMPLETE ===\n")
cat("Total rows:", nrow(all_wind_data), "\n")
cat("Unique sites:", length(unique(all_wind_data$site_id)), "\n")
cat("Coordinate ranges:\n")
cat("  Latitude:", min(all_wind_data$latitude), "to", max(all_wind_data$latitude), "\n")
cat("  Longitude:", min(all_wind_data$longitude), "to", max(all_wind_data$longitude), "\n")

# Save the processed data
write.csv(all_wind_data, "processed_wind_data.csv", row.names = FALSE)
cat("Data saved to 'processed_wind_data.csv'\n")