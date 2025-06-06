# Oregon Offshore Wind Site Analysis
# Analyzing tradeoffs between wind energy potential and fishery impacts

# Load required libraries
library(sf)           # Spatial data handling
library(dplyr)        # Data manipulation
library(ggplot2)      # Plotting
library(plotly)       # Interactive plots
library(leaflet)      # Interactive maps
library(raster)       # Raster data
library(geosphere)    # Geographic calculations
library(viridis)      # Color palettes
library(DT)           # Interactive tables
library(httr)         # API requests
library(jsonlite)     # JSON handling
library(lubridate)    # Date handling

# NREL API Configuration
NREL_API_KEY <- "W3rtr6GukFNkoygjMm6EMYaVK3TpPFqbdqaDIWF9"
NREL_BASE_URL <- "https://developer.nrel.gov/api/wind-toolkit/v2/wind/offshore-nw-pacific-download"

# =============================================================================
# 1. DEFINE LEASE AREA BOUNDARIES AND CREATE GRID
# =============================================================================

# Oregon lease area boundaries (approximate coordinates based on BOEM data)
create_lease_areas <- function() {
  # Coos Bay lease area (61,203 acres, ~32 miles offshore)
  coos_bay <- data.frame(
    lon = c(-125.5, -124.8, -124.8, -125.5, -125.5),
    lat = c(43.8, 43.8, 44.3, 44.3, 43.8)
  )
  
  # Brookings lease area (133,792 acres, ~18 miles offshore) 
  brookings <- data.frame(
    lon = c(-124.8, -124.3, -124.3, -124.8, -124.8),
    lat = c(42.0, 42.0, 42.8, 42.8, 42.0)
  )
  
  # Convert to sf polygons
  coos_sf <- st_polygon(list(as.matrix(coos_bay))) %>%
    st_sfc(crs = 4326) %>%
    st_sf(area_name = "Coos Bay")
  
  brookings_sf <- st_polygon(list(as.matrix(brookings))) %>%
    st_sfc(crs = 4326) %>%
    st_sf(area_name = "Brookings")
  
  # Combine lease areas
  lease_areas <- rbind(coos_sf, brookings_sf)
  return(lease_areas)
}

# Create 2x2km grid covering lease areas
create_analysis_grid <- function(lease_areas, cell_size_km = 2) {
  # Transform to projected coordinate system (Oregon State Plane South)
  lease_proj <- st_transform(lease_areas, crs = 3857)  # Web Mercator for simplicity
  
  # Get bounding box
  bbox <- st_bbox(lease_proj)
  
  # Create grid (cell_size_km * 1000 for meters)
  cell_size_m <- cell_size_km * 1000
  
  grid <- st_make_grid(
    lease_proj,
    cellsize = c(cell_size_m, cell_size_m),
    what = "polygons"
  ) %>%
    st_sf(site_id = 1:length(.))
  
  # Keep only grid cells that intersect with lease areas
  grid_intersect <- st_intersects(grid, lease_proj)
  grid_in_lease <- grid[lengths(grid_intersect) > 0, ]
  
  # Transform back to WGS84
  grid_wgs84 <- st_transform(grid_in_lease, crs = 4326)
  
  # Add centroids for analysis
  centroids <- st_centroid(grid_wgs84)
  coords <- st_coordinates(centroids)
  
  grid_wgs84$lon <- coords[, 1]
  grid_wgs84$lat <- coords[, 2]
  grid_wgs84$site_id <- 1:nrow(grid_wgs84)
  
  return(grid_wgs84)
}

# =============================================================================
# 2. NREL WIND DATA INTEGRATION
# =============================================================================

# Fetch wind data from NREL API for a single point
fetch_nrel_wind_data <- function(lat, lon, year = "2019", 
                                 attributes = "windspeed_80m,windspeed_100m,windspeed_120m",
                                 email = "stevesw@live.com") {
  
  # Add delay to respect rate limits
  Sys.sleep(5)  # Wait 1 second between requests
  
  # Build API URL
  url <- paste0(NREL_BASE_URL, ".json")
  
  # Round coordinates to reasonable precision
  lat <- round(lat, 6)
  lon <- round(lon, 6)
  
  # Create WKT POINT format (note: longitude comes first in WKT!)
  wkt_point <- paste0("POINT(", lon, " ", lat, ")")
  
  # API parameters - use WKT instead of separate lat/lon
  params <- list(
    api_key = NREL_API_KEY,
    wkt = wkt_point,
    names = year,
    attributes = attributes,
    utc = "true",
    leap_day = "true",
    interval = "60",
    email = email  # Required parameter!
  )
  
  # Add debugging
  cat("Requesting WKT:", wkt_point, "\n")
  
  # Make API request with error handling
  tryCatch({
    response <- GET(url, query = params, timeout(30))
    
    if (status_code(response) == 200) {
      data <- fromJSON(content(response, "text"))
      return(data)
    } else {
      # Get more detailed error info
      error_content <- content(response, "text")
      cat("API Error for", wkt_point, "Status:", status_code(response), "\n")
      cat("Error details:", error_content, "\n")
      return(NULL)
    }
  }, error = function(e) {
    cat("Network error for", wkt_point, ":", e$message, "\n")
    return(NULL)
  })
}

# Process NREL wind data to calculate annual statistics
process_wind_data <- function(wind_data) {
  if (is.null(wind_data) || is.null(wind_data$outputs)) {
    return(list(
      mean_wind_speed_80m = NA,
      mean_wind_speed_100m = NA,
      mean_wind_speed_120m = NA,
      wind_speed_hub_height = NA,
      capacity_factor = NA
    ))
  }
  
  # Extract wind speeds at different heights
  ws_80m <- as.numeric(wind_data$outputs$windspeed_80m)
  ws_100m <- as.numeric(wind_data$outputs$windspeed_100m) 
  ws_120m <- as.numeric(wind_data$outputs$windspeed_120m)
  
  # Calculate annual means
  mean_ws_80m <- mean(ws_80m, na.rm = TRUE)
  mean_ws_100m <- mean(ws_100m, na.rm = TRUE)
  mean_ws_120m <- mean(ws_120m, na.rm = TRUE)
  
  # Use 120m as hub height (typical for large offshore turbines)
  hub_height_wind_speed <- mean_ws_120m
  
  # Calculate capacity factor using realistic power curve
  capacity_factor <- calculate_capacity_factor_from_hourly(ws_120m)
  
  return(list(
    mean_wind_speed_80m = round(mean_ws_80m, 2),
    mean_wind_speed_100m = round(mean_ws_100m, 2),
    mean_wind_speed_120m = round(mean_ws_120m, 2),
    wind_speed_hub_height = round(hub_height_wind_speed, 2),
    capacity_factor = round(capacity_factor, 3)
  ))
}

# Calculate capacity factor from hourly wind speed data
calculate_capacity_factor_from_hourly <- function(hourly_wind_speeds) {
  # Remove NA values
  ws <- hourly_wind_speeds[!is.na(hourly_wind_speeds)]
  
  if (length(ws) == 0) return(NA)
  
  # Modern offshore wind turbine power curve (e.g., GE Haliade-X 12MW)
  # Power curve parameters
  cut_in <- 3.0      # m/s
  rated_speed <- 12.5 # m/s  
  cut_out <- 25.0    # m/s
  rated_power <- 1.0  # normalized to 1.0
  
  # Calculate power output for each hour
  power_output <- sapply(ws, function(v) {
    if (v < cut_in || v >= cut_out) {
      return(0)
    } else if (v <= rated_speed) {
      # Cubic relationship in Region II
      return(rated_power * ((v^3 - cut_in^3) / (rated_speed^3 - cut_in^3)))
    } else {
      # Constant rated power in Region III
      return(rated_power)
    }
  })
  
  # Capacity factor is average power output
  capacity_factor <- mean(power_output, na.rm = TRUE)
  return(capacity_factor)
}

# Fetch wind data for all grid cells (with progress tracking)
fetch_wind_data_for_grid <- function(grid, year = "2019") {
  
  cat("Fetching NREL wind data for", nrow(grid), "grid cells...\n")
  cat("This may take several minutes...\n")
  
  # Initialize results
  wind_results <- data.frame(
    site_id = grid$site_id,
    mean_wind_speed_80m = NA,
    mean_wind_speed_100m = NA,
    mean_wind_speed_120m = NA,
    wind_speed_hub_height = NA,
    capacity_factor = NA,
    stringsAsFactors = FALSE
  )
  
  # Process in batches to avoid overwhelming the API
  batch_size <- 10
  n_batches <- ceiling(nrow(grid) / batch_size)
  
  for (batch in 1:n_batches) {
    start_idx <- (batch - 1) * batch_size + 1
    end_idx <- min(batch * batch_size, nrow(grid))
    
    cat("Processing batch", batch, "of", n_batches, 
        "(sites", start_idx, "to", end_idx, ")\n")
    
    for (i in start_idx:end_idx) {
      # Fetch data for this grid cell
      wind_data <- fetch_nrel_wind_data(grid$lat[i], grid$lon[i], year)
      
      # Process the data
      processed <- process_wind_data(wind_data)
      
      # Store results
      wind_results[i, 2:6] <- processed
      
      # Small delay to be respectful to the API
      Sys.sleep(0.1)
    }
    
    # Longer pause between batches
    if (batch < n_batches) {
      cat("Pausing between batches...\n")
      Sys.sleep(1)
    }
  }
  
  cat("Wind data fetch complete!\n")
  return(wind_results)
}

# =============================================================================
# 3. ENHANCED WIND POWER CALCULATION WITH REAL DATA
# =============================================================================

# Calculate wind power potential using real NREL data
calculate_wind_power_with_nrel_data <- function(grid, wind_data, turbine_capacity_mw = 12, 
                                                turbines_per_site = 1) {
  
  # Merge wind data with grid
  grid <- merge(grid, wind_data, by = "site_id", all.x = TRUE)
  
  # Calculate distance to shore for context
  grid$dist_to_shore <- apply(st_coordinates(st_centroid(grid)), 1, function(coord) {
    min(geosphere::distGeo(coord, c(-124.0, coord[2]))) / 1000  # km
  })
  
  # Use NREL capacity factor (already calculated from hourly data)
  # If missing, use a conservative estimate based on mean wind speed
  grid$capacity_factor[is.na(grid$capacity_factor)] <- 
    pmax(0.2, pmin(0.6, (grid$wind_speed_hub_height[is.na(grid$capacity_factor)] - 4) / 15))
  
  # Total capacity and annual generation
  grid$total_capacity_mw <- turbines_per_site * turbine_capacity_mw
  grid$annual_generation_gwh <- grid$total_capacity_mw * grid$capacity_factor * 8760 / 1000
  
  # Revenue estimate with more realistic pricing
  # Oregon wholesale electricity prices vary seasonally
  wholesale_price <- 55  # $/MWh (updated Oregon average)
  grid$annual_revenue_millions <- grid$annual_generation_gwh * wholesale_price / 1000
  
  # Add wind quality metrics
  grid$wind_quality <- ifelse(grid$capacity_factor > 0.45, "Excellent",
                              ifelse(grid$capacity_factor > 0.35, "Good",
                                     ifelse(grid$capacity_factor > 0.25, "Fair", "Poor")))
  
  return(grid)
}

# =============================================================================
# 4. FISHERY IMPACT ASSESSMENT  
# =============================================================================

# Calculate fishery impact for each grid cell
calculate_fishery_impact <- function(grid, base_fishery_value_per_km2 = 50000,
                                     impact_radius_km = 5, shore_distance_weight = 0.7) {
  
  # Fishery value varies with distance to shore (higher closer to shore)
  # and by latitude (different fishing grounds)
  
  # Base fishery density (higher closer to shore)
  grid$fishery_density <- base_fishery_value_per_km2 * 
    exp(-grid$dist_to_shore * shore_distance_weight / 30) *  # Exponential decay from shore
    (1 + 0.2 * sin((grid$lat - 42) * pi))  # Latitudinal variation
  
  # Calculate impact area (circle around each turbine site)
  impact_area_km2 <- pi * impact_radius_km^2
  
  # Total fishery value at risk per site
  grid$fishery_value_at_risk <- grid$fishery_density * impact_area_km2
  
  # Impact factor (assume 30-70% reduction in fishing in impact area)
  impact_factor <- 0.5  # 50% average reduction
  grid$annual_fishery_loss_millions <- grid$fishery_value_at_risk * impact_factor / 1e6
  
  return(grid)
}

# =============================================================================
# 5. TRADEOFF ANALYSIS AND OPTIMIZATION
# =============================================================================

# Calculate tradeoff scores and identify optimal sites
perform_tradeoff_analysis <- function(grid, energy_weight = 0.6) {
  fishery_weight <- 1 - energy_weight
  
  # Normalize metrics to 0-1 scale
  grid$energy_score <- (grid$annual_revenue_millions - min(grid$annual_revenue_millions)) /
    (max(grid$annual_revenue_millions) - min(grid$annual_revenue_millions))
  
  grid$fishery_score <- 1 - ((grid$annual_fishery_loss_millions - min(grid$annual_fishery_loss_millions)) /
                               (max(grid$annual_fishery_loss_millions) - min(grid$annual_fishery_loss_millions)))
  
  # Combined score (higher is better)
  grid$combined_score <- energy_weight * grid$energy_score + 
    fishery_weight * grid$fishery_score
  
  # Rank sites
  grid$rank <- rank(-grid$combined_score)
  
  return(grid)
}

# =============================================================================
# 6. VISUALIZATION FUNCTIONS
# =============================================================================

# Create interactive map of analysis results
create_results_map <- function(grid, top_n = 50) {
  
  # Prepare data for mapping
  top_sites <- grid %>% 
    arrange(rank) %>% 
    slice_head(n = top_n)
  
  # Create color palette
  pal <- colorNumeric(
    palette = viridis::viridis(100),
    domain = grid$combined_score
  )
  
  # Create leaflet map
  map <- leaflet(grid) %>%
    addTiles() %>%
    addPolygons(
      fillColor = ~pal(combined_score),
      fillOpacity = 0.6,
      color = "white",
      weight = 1,
      popup = ~paste(
        "<b>Site ID:</b>", site_id, "<br>",
        "<b>Rank:</b>", rank, "<br>",
        "<b>Combined Score:</b>", round(combined_score, 3), "<br>",
        "<b>Annual Revenue:</b> $", round(annual_revenue_millions, 1), "M<br>",
        "<b>Fishery Loss:</b> $", round(annual_fishery_loss_millions, 1), "M<br>",
        "<b>Wind Speed:</b>", round(wind_speed, 1), " m/s<br>",
        "<b>Capacity Factor:</b>", round(capacity_factor * 100, 1), "%"
      )
    ) %>%
    addPolygons(
      data = top_sites,
      fillColor = "red",
      fillOpacity = 0.8,
      color = "darkred",
      weight = 2,
      group = "Top Sites"
    ) %>%
    addLegend(
      pal = pal,
      values = ~combined_score,
      title = "Combined Score",
      position = "bottomright"
    ) %>%
    addLayersControl(
      overlayGroups = "Top Sites",
      options = layersControlOptions(collapsed = FALSE)
    )
  
  return(map)
}

# Create tradeoff scatter plot
create_tradeoff_plot <- function(grid, top_n = 20) {
  
  top_sites <- grid %>% 
    arrange(rank) %>% 
    slice_head(n = top_n)
  
  p <- ggplot(grid, aes(x = annual_fishery_loss_millions, y = annual_revenue_millions)) +
    geom_point(aes(color = combined_score, size = capacity_factor), alpha = 0.6) +
    geom_point(data = top_sites, color = "red", size = 3, shape = 1, stroke = 2) +
    scale_color_viridis_c(name = "Combined\nScore") +
    scale_size_continuous(name = "Capacity\nFactor", range = c(1, 4)) +
    labs(
      x = "Annual Fishery Loss ($ Millions)",
      y = "Annual Energy Revenue ($ Millions)",
      title = "Wind Energy Revenue vs. Fishery Impact Tradeoff",
      subtitle = "Red circles indicate top-ranked sites"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      legend.position = "right"
    )
  
  return(p)
}

# =============================================================================
# 7. MAIN ANALYSIS FUNCTION WITH NREL DATA
# =============================================================================

# Run complete analysis with real NREL wind data
run_offshore_wind_analysis_with_nrel <- function(
    cell_size_km = 2,
    turbine_capacity_mw = 12,
    turbines_per_site = 1,  # Updated for single large turbine per 2x2km
    base_fishery_value = 50000,
    impact_radius_km = 5,
    shore_distance_weight = 0.7,
    energy_weight = 0.6,
    wind_year = "2019",  # Year of wind data to fetch
    fetch_new_wind_data = TRUE  # Set to FALSE to use cached data
) {
  
  cat("Creating lease areas and analysis grid...\n")
  lease_areas <- create_lease_areas()
  grid <- create_analysis_grid(lease_areas, cell_size_km)
  
  if (fetch_new_wind_data) {
    cat("Fetching real wind data from NREL (this will take time)...\n")
    wind_data <- fetch_wind_data_for_grid(grid, wind_year)
    
    # Save wind data for future use
    saveRDS(wind_data, paste0("oregon_wind_data_", wind_year, ".rds"))
    cat("Wind data saved to oregon_wind_data_", wind_year, ".rds\n")
  } else {
    cat("Loading cached wind data...\n")
    wind_data <- readRDS(paste0("oregon_wind_data_", wind_year, ".rds"))
  }
  
  cat("Calculating wind power potential with real NREL data...\n")  
  grid <- calculate_wind_power_with_nrel_data(grid, wind_data, turbine_capacity_mw, turbines_per_site)
  
  cat("Assessing fishery impacts...\n")
  grid <- calculate_fishery_impact(grid, base_fishery_value, impact_radius_km, shore_distance_weight)
  
  cat("Performing tradeoff analysis...\n")
  grid <- perform_tradeoff_analysis(grid, energy_weight)
  
  # Enhanced summary statistics with real wind data
  summary_stats <- list(
    total_sites = nrow(grid),
    sites_with_wind_data = sum(!is.na(grid$wind_speed_hub_height)),
    total_capacity_gw = sum(grid$total_capacity_mw, na.rm = TRUE) / 1000,
    total_generation_twh = sum(grid$annual_generation_gwh, na.rm = TRUE) / 1000,
    avg_capacity_factor = mean(grid$capacity_factor, na.rm = TRUE),
    avg_wind_speed = mean(grid$wind_speed_hub_height, na.rm = TRUE),
    avg_fishery_impact = mean(grid$annual_fishery_loss_millions, na.rm = TRUE),
    wind_quality_distribution = table(grid$wind_quality),
    top_10_sites = grid %>% 
      filter(!is.na(capacity_factor)) %>%
      arrange(rank) %>% 
      slice_head(n = 10)
  )
  
  cat("Analysis complete!\n")
  cat("Total sites analyzed:", summary_stats$total_sites, "\n")
  cat("Sites with wind data:", summary_stats$sites_with_wind_data, "\n")
  cat("Total potential capacity:", round(summary_stats$total_capacity_gw, 1), "GW\n")
  cat("Average capacity factor:", round(summary_stats$avg_capacity_factor * 100, 1), "%\n")
  cat("Average wind speed (120m):", round(summary_stats$avg_wind_speed, 1), "m/s\n")
  cat("Average fishery impact:", round(summary_stats$avg_fishery_impact, 2), "$M/year\n")
  
  return(list(
    grid = grid,
    wind_data = wind_data,
    lease_areas = lease_areas,
    summary = summary_stats
  ))
}

# =============================================================================
# 8. EXAMPLE USAGE WITH REAL NREL DATA
# =============================================================================

# Run the analysis with real NREL wind data
cat("Starting Oregon Offshore Wind Analysis with NREL Data...\n")
cat("WARNING: This will fetch data for 100+ sites and may take 10-20 minutes!\n")
cat("Set fetch_new_wind_data = FALSE to use cached data after first run.\n")

# For initial run with new data:
results <- run_offshore_wind_analysis_with_nrel(
  fetch_new_wind_data = TRUE,
  wind_year = "2019"
)

# For subsequent runs using cached data:
# results <- run_offshore_wind_analysis_with_nrel(
#   fetch_new_wind_data = FALSE,
#   wind_year = "2019"
# )

# Create visualizations
cat("Creating visualizations...\n")
map <- create_results_map(results$grid)
tradeoff_plot <- create_tradeoff_plot(results$grid)

# Display enhanced results with real wind data
cat("\nTop 10 Optimal Sites (with real NREL wind data):\n")
top_sites_table <- results$summary$top_10_sites %>%
  select(site_id, rank, combined_score, annual_revenue_millions, 
         annual_fishery_loss_millions, wind_speed_hub_height, 
         capacity_factor, wind_quality) %>%
  mutate(
    combined_score = round(combined_score, 3),
    annual_revenue_millions = round(annual_revenue_millions, 1),
    annual_fishery_loss_millions = round(annual_fishery_loss_millions, 1),
    wind_speed_hub_height = round(wind_speed_hub_height, 1),
    capacity_factor = round(capacity_factor * 100, 1)  # Convert to percentage
  )

print(top_sites_table)

# Wind quality summary
cat("\nWind Quality Distribution:\n")
print(results$summary$wind_quality_distribution)

# Display interactive map and plot
cat("\nDisplaying interactive map and tradeoff plot...\n")
map
tradeoff_plot

# Save results for further analysis
# write_sf(results$grid, "oregon_wind_analysis_nrel_results.shp")
# saveRDS(results, "oregon_wind_analysis_nrel_results.rds")

cat("\nAnalysis complete with real NREL wind data!\n")
cat("Key Findings:\n")
cat("- Average wind speed at 120m:", round(results$summary$avg_wind_speed, 1), "m/s\n")
cat("- Average capacity factor:", round(results$summary$avg_capacity_factor * 100, 1), "%\n")
cat("- Total potential capacity:", round(results$summary$total_capacity_gw, 1), "GW\n")
cat("\nUse the 'results' object to explore the data further.\n")

# =============================================================================
# 9. EXTENSIBILITY FUNCTIONS
# =============================================================================

# Function to add new factors to the analysis
add_analysis_factor <- function(grid, factor_name, factor_values, weight = 0.1) {
  # Normalize the new factor
  normalized_values <- (factor_values - min(factor_values)) / 
    (max(factor_values) - min(factor_values))
  
  # Add to grid
  grid[[paste0(factor_name, "_score")]] <- normalized_values
  
  # Recalculate combined score (this is a simplified approach)
  # In practice, you'd want more sophisticated weight management
  grid$combined_score <- grid$combined_score * (1 - weight) + 
    normalized_values * weight
  
  # Re-rank
  grid$rank <- rank(-grid$combined_score)
  
  return(grid)
}

# Example: Add shipping lane conflict factor
# shipping_conflict <- runif(nrow(results$grid), 0, 1)  # Random example data
# results$grid <- add_analysis_factor(results$grid, "shipping_conflict", 
#                                   1 - shipping_conflict, weight = 0.15)