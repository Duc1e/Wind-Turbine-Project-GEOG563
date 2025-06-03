# Simple function using filename coordinates
read_wind_file_final <- function(filepath) {
  # Extract coordinates from filename - most reliable method
  filename <- basename(filepath)
  filename_parts <- strsplit(filename, "_")[[1]]
  
  site_id <- filename_parts[1]
  lat <- as.numeric(filename_parts[2])
  lon <- as.numeric(filename_parts[3])
  
  # Read the file skipping the metadata row, using row 2 as headers
  df <- read.csv(filepath, stringsAsFactors = FALSE, skip = 1, header = TRUE)
  
  cat("Processing site", site_id, "- Lat:", lat, "Lon:", lon, "- Rows:", nrow(df), "\n")
  
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

# Test this final approach
test_final <- read_wind_file_final(wind_files[1])
head(test_final)

# Test on the previously problematic file too
test_final_2 <- read_wind_file_final(wind_files[100])
head(test_final_2)

cat("\nBoth files successful - ready to process all 5823 files!\n")