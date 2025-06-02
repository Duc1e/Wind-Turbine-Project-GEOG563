# Step 2C: Oregon Offshore Wind Analysis - Download Wind Data

# Download wind data for multiple grid points with rate limiting
fetch_oregon_wind_data <- function(grid, year = "2019", max_points = 5) {
  cat("Downloading NREL wind data for", min(nrow(grid), max_points), "points...\n")
  cat("This may take several minutes due to API rate limiting...\n\n")
  
  # Limit to manageable number of points
  sample_grid <- grid[1:min(nrow(grid), max_points), ]
  
  wind_results <- data.frame(
    site_id = sample_grid$site_id,
    lon = sample_grid$lon,
    lat = sample_grid$lat,
    mean_wind_speed_80m = NA,
    mean_wind_speed_100m = NA, 
    mean_wind_speed_120m = NA,
    wind_speed_hub_height = NA,
    capacity_factor = NA,
    stringsAsFactors = FALSE
  )
  
  successful_downloads <- 0
  
  for (i in 1:nrow(sample_grid)) {
    cat("Downloading point", i, "of", nrow(sample_grid), 
        "(lat:", round(sample_grid$lat[i], 3), 
        "lon:", round(sample_grid$lon[i], 3), ")...")
    
    point_data <- download_single_point_wind(
      lat = sample_grid$lat[i],
      lon = sample_grid$lon[i],
      year = year
    )
    
    if (!is.null(point_data) && nrow(point_data) > 0) {
      # Extract wind speeds
      ws_80m <- as.numeric(point_data$`windspeed_80m`)
      ws_100m <- as.numeric(point_data$`windspeed_100m`)
      ws_120m <- as.numeric(point_data$`windspeed_120m`)
      
      # Calculate statistics
      wind_results$mean_wind_speed_80m[i] <- round(mean(ws_80m, na.rm = TRUE), 2)
      wind_results$mean_wind_speed_100m[i] <- round(mean(ws_100m, na.rm = TRUE), 2)
      wind_results$mean_wind_speed_120m[i] <- round(mean(ws_120m, na.rm = TRUE), 2)
      wind_results$wind_speed_hub_height[i] <- wind_results$mean_wind_speed_120m[i]
      wind_results$capacity_factor[i] <- round(calculate_capacity_factor_from_hourly(ws_120m), 3)
      
      successful_downloads <- successful_downloads + 1
      cat(" SUCCESS! Wind:", wind_results$wind_speed_hub_height[i], "m/s, CF:", 
          wind_results$capacity_factor[i], "\n")
    } else {
      cat(" FAILED\n")
    }
    
    # Rate limiting - wait between requests
    if (i < nrow(sample_grid)) {
      cat("Waiting 3 seconds...\n")
      Sys.sleep(3)
    }
  }
  
  cat("\nDownload complete! Successful:", successful_downloads, "out of", nrow(sample_grid), "\n")
  return(wind_results)
}

# Actually download the data
wind_data <- fetch_oregon_wind_data(grid, max_points = 3)

cat("Step 2A Complete: Downloaded wind data for", sum(!is.na(wind_data$capacity_factor)), "points\n")