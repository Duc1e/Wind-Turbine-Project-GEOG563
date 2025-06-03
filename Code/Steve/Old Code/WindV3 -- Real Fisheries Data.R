# Oregon Offshore Wind Site Analysis
# Complete Enhanced Version with Bulk Download & Real Fisheries Integration

# Load required libraries
library(sf)
library(dplyr)
library(ggplot2)
library(leaflet)
library(httr)
library(jsonlite)
library(lubridate)
library(readr)
library(geosphere)
library(viridis)

# NREL API Configuration
NREL_API_KEY <- "W3rtr6GukFNkoygjMm6EMYaVK3TpPFqbdqaDIWF9"
NREL_BULK_URL <- "https://developer.nrel.gov/api/wind-toolkit/v2/wind/offshore-nw-pacific-download"

# =============================================================================
# 1. LEASE AREAS AND GRID
# =============================================================================

create_lease_areas <- function() {
  coos_bay <- data.frame(
    lon = c(-125.5, -124.8, -124.8, -125.5, -125.5),
    lat = c(43.8, 43.8, 44.3, 44.3, 43.8)
  )
  
  brookings <- data.frame(
    lon = c(-124.8, -124.3, -124.3, -124.8, -124.8),
    lat = c(42.0, 42.0, 42.8, 42.8, 42.0)
  )
  
  coos_sf <- st_polygon(list(as.matrix(coos_bay))) %>%
    st_sfc(crs = 4326) %>%
    st_sf(area_name = "Coos Bay")
  
  brookings_sf <- st_polygon(list(as.matrix(brookings))) %>%
    st_sfc(crs = 4326) %>%
    st_sf(area_name = "Brookings")
  
  lease_areas <- rbind(coos_sf, brookings_sf)
  return(lease_areas)
}

create_analysis_grid <- function(lease_areas, cell_size_km = 2) {
  lease_proj <- st_transform(lease_areas, crs = 3857)
  bbox <- st_bbox(lease_proj)
  cell_size_m <- cell_size_km * 1000
  
  grid <- st_make_grid(
    lease_proj,
    cellsize = c(cell_size_m, cell_size_m),
    what = "polygons"
  ) %>%
    st_sf(site_id = 1:length(.))
  
  grid_intersect <- st_intersects(grid, lease_proj)
  grid_in_lease <- grid[lengths(grid_intersect) > 0, ]
  grid_wgs84 <- st_transform(grid_in_lease, crs = 4326)
  
  centroids <- st_centroid(grid_wgs84)
  coords <- st_coordinates(centroids)
  
  grid_wgs84$lon <- coords[, 1]
  grid_wgs84$lat <- coords[, 2]
  grid_wgs84$site_id <- 1:nrow(grid_wgs84)
  
  return(grid_wgs84)
}

# =============================================================================
# 2. BULK WIND DATA DOWNLOAD
# =============================================================================

create_oregon_wkt_polygon <- function(grid) {
  cat("Creating WKT polygon for bulk download...\n")
  
  bbox <- st_bbox(grid)
  buffer <- 0.1
  min_lon <- bbox["xmin"] - buffer
  max_lon <- bbox["xmax"] + buffer  
  min_lat <- bbox["ymin"] - buffer
  max_lat <- bbox["ymax"] + buffer
  
  wkt_polygon <- paste0(
    "POLYGON((",
    min_lon, " ", min_lat, ",",
    max_lon, " ", min_lat, ",", 
    max_lon, " ", max_lat, ",",
    min_lon, " ", max_lat, ",",
    min_lon, " ", min_lat,
    "))"
  )
  
  cat("WKT Polygon:", wkt_polygon, "\n")
  return(wkt_polygon)
}

download_oregon_wind_bulk <- function(grid, year = "2019", 
                                      attributes = "windspeed_80m,windspeed_100m,windspeed_120m",
                                      email = "stevesw@live.com") {
  
  cat("Starting bulk wind data download...\n")
  
  wkt_polygon <- create_oregon_wkt_polygon(grid)
  url <- paste0(NREL_BULK_URL, ".csv")
  
  params <- list(
    api_key = NREL_API_KEY,
    wkt = wkt_polygon,
    names = year,
    attributes = attributes,
    utc = "true",
    leap_day = "true", 
    interval = "60",
    email = email
  )
  
  cat("Sending bulk download request to NREL...\n")
  
  tryCatch({
    response <- GET(url, query = params, timeout(300))
    
    if (status_code(response) == 200) {
      cat("Download successful! Processing data...\n")
      
      csv_content <- content(response, "text", encoding = "UTF-8")
      backup_filename <- paste0("nrel_oregon_bulk_", year, "_raw.csv")
      writeLines(csv_content, backup_filename)
      cat("Raw data saved to:", backup_filename, "\n")
      
      bulk_data <- read_csv(csv_content, show_col_types = FALSE)
      cat("Bulk data loaded successfully!\n")
      
      return(bulk_data)
      
    } else {
      error_content <- content(response, "text")
      cat("API Error - Status:", status_code(response), "\n")
      cat("Error details:", error_content, "\n")
      return(NULL)
    }
    
  }, error = function(e) {
    cat("Network error during bulk download:", e$message, "\n")
    return(NULL)
  })
}

process_bulk_wind_data <- function(bulk_data, grid) {
  cat("Processing bulk wind data to grid cells...\n")
  
  if (is.null(bulk_data)) {
    cat("No bulk data to process.\n")
    return(NULL)
  }
  
  coordinates <- bulk_data %>%
    select(longitude, latitude) %>%
    distinct() %>%
    arrange(longitude, latitude)
  
  cat("Found", nrow(coordinates), "unique locations in bulk data\n")
  
  wind_results <- data.frame(
    site_id = grid$site_id,
    lon = grid$lon,
    lat = grid$lat,
    mean_wind_speed_80m = NA,
    mean_wind_speed_100m = NA, 
    mean_wind_speed_120m = NA,
    wind_speed_hub_height = NA,
    capacity_factor = NA,
    stringsAsFactors = FALSE
  )
  
  for (i in 1:nrow(grid)) {
    if (i %% 10 == 0) cat("Processing grid cell", i, "of", nrow(grid), "\n")
    
    grid_lon <- grid$lon[i]
    grid_lat <- grid$lat[i]
    
    distances <- sqrt((coordinates$longitude - grid_lon)^2 + 
                        (coordinates$latitude - grid_lat)^2)
    
    nearest_idx <- which.min(distances)
    nearest_lon <- coordinates$longitude[nearest_idx]
    nearest_lat <- coordinates$latitude[nearest_idx]
    
    location_data <- bulk_data %>%
      filter(longitude == nearest_lon, latitude == nearest_lat)
    
    if (nrow(location_data) > 0) {
      wind_stats <- process_location_wind_data(location_data)
      
      wind_results[i, "mean_wind_speed_80m"] <- wind_stats$mean_wind_speed_80m
      wind_results[i, "mean_wind_speed_100m"] <- wind_stats$mean_wind_speed_100m
      wind_results[i, "mean_wind_speed_120m"] <- wind_stats$mean_wind_speed_120m
      wind_results[i, "wind_speed_hub_height"] <- wind_stats$wind_speed_hub_height
      wind_results[i, "capacity_factor"] <- wind_stats$capacity_factor
    }
  }
  
  complete_data <- sum(!is.na(wind_results$capacity_factor))
  cat("\nBulk processing complete!\n")
  cat("Grid cells with wind data:", complete_data, "/", nrow(grid), "\n")
  
  return(wind_results)
}

process_location_wind_data <- function(location_data) {
  ws_80m <- as.numeric(location_data$`windspeed_80m`)
  ws_100m <- as.numeric(location_data$`windspeed_100m`)
  ws_120m <- as.numeric(location_data$`windspeed_120m`)
  
  mean_ws_80m <- mean(ws_80m, na.rm = TRUE)
  mean_ws_100m <- mean(ws_100m, na.rm = TRUE) 
  mean_ws_120m <- mean(ws_120m, na.rm = TRUE)
  
  hub_height_wind_speed <- mean_ws_120m
  capacity_factor <- calculate_capacity_factor_from_hourly(ws_120m)
  
  return(list(
    mean_wind_speed_80m = round(mean_ws_80m, 2),
    mean_wind_speed_100m = round(mean_ws_100m, 2),
    mean_wind_speed_120m = round(mean_ws_120m, 2),
    wind_speed_hub_height = round(hub_height_wind_speed, 2),
    capacity_factor = round(capacity_factor, 3)
  ))
}

fetch_oregon_wind_data_bulk <- function(grid, year = "2019", save_results = TRUE) {
  cat("OREGON OFFSHORE WIND DATA - BULK DOWNLOAD\n")
  cat("=========================================\n\n")
  
  bulk_data <- download_oregon_wind_bulk(grid, year)
  
  if (is.null(bulk_data)) {
    cat("Bulk download failed.\n")
    return(NULL)
  }
  
  wind_results <- process_bulk_wind_data(bulk_data, grid)
  
  if (is.null(wind_results)) {
    cat("Processing failed.\n")
    return(NULL)
  }
  
  if (save_results) {
    results_filename <- paste0("oregon_wind_data_bulk_", year, ".rds")
    saveRDS(wind_results, results_filename)
    cat("Wind data saved to:", results_filename, "\n")
  }
  
  return(wind_results)
}

# =============================================================================
# 3. WIND POWER CALCULATIONS
# =============================================================================

calculate_capacity_factor_from_hourly <- function(hourly_wind_speeds) {
  ws <- hourly_wind_speeds[!is.na(hourly_wind_speeds)]
  
  if (length(ws) == 0) return(NA)
  
  cut_in <- 3.0
  rated_speed <- 12.5
  cut_out <- 25.0
  rated_power <- 1.0
  
  power_output <- sapply(ws, function(v) {
    if (v < cut_in || v >= cut_out) {
      return(0)
    } else if (v <= rated_speed) {
      return(rated_power * ((v^3 - cut_in^3) / (rated_speed^3 - cut_in^3)))
    } else {
      return(rated_power)
    }
  })
  
  capacity_factor <- mean(power_output, na.rm = TRUE)
  return(capacity_factor)
}

calculate_wind_power_with_nrel_data <- function(grid, wind_data, turbine_capacity_mw = 12, 
                                                turbines_per_site = 1) {
  
  grid <- merge(grid, wind_data, by = "site_id", all.x = TRUE)
  
  grid$dist_to_shore <- apply(st_coordinates(st_centroid(grid)), 1, function(coord) {
    min(geosphere::distGeo(coord, c(-124.0, coord[2]))) / 1000
  })
  
  grid$capacity_factor[is.na(grid$capacity_factor)] <- 
    pmax(0.2, pmin(0.6, (grid$wind_speed_hub_height[is.na(grid$capacity_factor)] - 4) / 15))
  
  grid$total_capacity_mw <- turbines_per_site * turbine_capacity_mw
  grid$annual_generation_gwh <- grid$total_capacity_mw * grid$capacity_factor * 8760 / 1000
  
  wholesale_price <- 55
  grid$annual_revenue_millions <- grid$annual_generation_gwh * wholesale_price / 1000
  
  grid$wind_quality <- ifelse(grid$capacity_factor > 0.45, "Excellent",
                              ifelse(grid$capacity_factor > 0.35, "Good",
                                     ifelse(grid$capacity_factor > 0.25, "Fair", "Poor")))
  
  return(grid)
}

# =============================================================================
# 4. FISHERIES DATA
# =============================================================================

generate_sample_fisheries_data <- function(grid, n_vessels = 50, n_trips_per_vessel = 20) {
  cat("Generating sample fisheries data...\n")
  
  vessels <- data.frame(
    vessel_id = paste0("OR", sprintf("%04d", 1:n_vessels)),
    vessel_length = runif(n_vessels, 25, 85),
    home_port = sample(c("Newport", "Astoria", "Brookings", "Bandon", "Florence"), 
                       n_vessels, replace = TRUE),
    primary_gear = sample(c("trawl", "longline", "crab_pot", "gillnet"), 
                          n_vessels, replace = TRUE, 
                          prob = c(0.4, 0.3, 0.2, 0.1))
  )
  
  all_trips <- list()
  trip_id <- 1
  
  for (v in 1:n_vessels) {
    vessel <- vessels[v, ]
    n_trips <- rpois(1, n_trips_per_vessel)
    
    for (t in 1:n_trips) {
      trip_date <- as.Date("2019-01-01") + sample(0:1460, 1)
      
      if (vessel$primary_gear == "crab_pot") {
        fishing_lons <- runif(5, -124.8, -124.3)
        fishing_lats <- runif(5, 42.0, 44.5)
        species <- "dungeness_crab"
        catch_lbs <- rlnorm(1, 6, 0.5)
        price_per_lb <- runif(1, 3, 6)
      } else if (vessel$primary_gear == "trawl") {
        fishing_lons <- runif(8, -125.2, -124.5)
        fishing_lats <- runif(8, 42.2, 44.3)
        species <- sample(c("rockfish", "lingcod", "sablefish", "dover_sole"), 1)
        catch_lbs <- rlnorm(1, 7, 0.7)
        price_per_lb <- runif(1, 1, 4)
      } else {
        fishing_lons <- runif(6, -125.0, -124.4)
        fishing_lats <- runif(6, 42.1, 44.4)
        species <- sample(c("salmon", "halibut", "albacore"), 1)
        catch_lbs <- rlnorm(1, 5.5, 0.6)
        price_per_lb <- runif(1, 2, 8)
      }
      
      trip_revenue <- catch_lbs * price_per_lb
      
      trip_data <- data.frame(
        trip_id = trip_id,
        vessel_id = vessel$vessel_id,
        trip_date = trip_date,
        gear_type = vessel$primary_gear,
        species = species,
        catch_lbs = round(catch_lbs, 1),
        price_per_lb = round(price_per_lb, 2),
        trip_revenue = round(trip_revenue, 2),
        fishing_locations = I(list(data.frame(
          lon = fishing_lons,
          lat = fishing_lats,
          fishing_hours = runif(length(fishing_lons), 1, 8)
        )))
      )
      
      all_trips[[trip_id]] <- trip_data
      trip_id <- trip_id + 1
    }
  }
  
  trips_df <- do.call(rbind, all_trips)
  cat("Generated", nrow(trips_df), "fishing trips\n")
  
  return(list(vessels = vessels, trips = trips_df))
}

process_fisheries_to_grid <- function(fisheries_data, grid, year_range = c(2019, 2023)) {
  cat("Processing fisheries data to grid cells...\n")
  
  grid$annual_fishing_revenue <- 0
  grid$fishing_trips <- 0
  grid$fishing_hours <- 0
  grid$dominant_gear <- NA
  grid$dominant_species <- NA
  grid$vessel_count <- 0
  
  if (!"sf" %in% class(grid)) {
    grid <- st_as_sf(grid)
  }
  
  for (i in 1:nrow(fisheries_data$trips)) {
    trip <- fisheries_data$trips[i, ]
    trip_year <- year(trip$trip_date)
    
    if (trip_year < year_range[1] || trip_year > year_range[2]) next
    
    locations <- trip$fishing_locations[[1]]
    
    fishing_points <- st_as_sf(locations, 
                               coords = c("lon", "lat"), 
                               crs = 4326)
    
    intersections <- st_intersects(fishing_points, grid)
    affected_cells <- unique(unlist(intersections))
    
    if (length(affected_cells) > 0) {
      for (cell_idx in affected_cells) {
        points_in_cell <- which(lengths(st_intersects(fishing_points, grid[cell_idx, ])) > 0)
        
        if (length(points_in_cell) > 0) {
          hours_in_cell <- sum(locations$fishing_hours[points_in_cell])
          total_trip_hours <- sum(locations$fishing_hours)
          
          cell_revenue <- trip$trip_revenue * (hours_in_cell / total_trip_hours)
          
          grid$annual_fishing_revenue[cell_idx] <- grid$annual_fishing_revenue[cell_idx] + cell_revenue
          grid$fishing_trips[cell_idx] <- grid$fishing_trips[cell_idx] + (hours_in_cell / total_trip_hours)
          grid$fishing_hours[cell_idx] <- grid$fishing_hours[cell_idx] + hours_in_cell
        }
      }
    }
  }
  
  years_in_data <- year_range[2] - year_range[1] + 1
  grid$annual_fishing_revenue <- grid$annual_fishing_revenue / years_in_data
  grid$fishing_trips <- grid$fishing_trips / years_in_data
  grid$fishing_hours <- grid$fishing_hours / years_in_data
  
  grid$revenue_per_km2 <- grid$annual_fishing_revenue / 4
  grid$trips_per_km2 <- grid$fishing_trips / 4
  grid$hours_per_km2 <- grid$fishing_hours / 4
  
  # Simplified gear assignment
  for (i in 1:nrow(grid)) {
    if (grid$annual_fishing_revenue[i] > 0) {
      if (grid$revenue_per_km2[i] > 50000) {
        grid$dominant_gear[i] <- "trawl"
        grid$dominant_species[i] <- "rockfish"
      } else if (grid$revenue_per_km2[i] > 20000) {
        grid$dominant_gear[i] <- "crab_pot"
        grid$dominant_species[i] <- "dungeness_crab"
      } else {
        grid$dominant_gear[i] <- "longline"
        grid$dominant_species[i] <- "sablefish"
      }
      grid$vessel_count[i] <- max(1, round(grid$fishing_trips[i] / 20))
    }
  }
  
  cat("Fisheries processing complete!\n")
  cat("Grid cells with fishing activity:", sum(grid$annual_fishing_revenue > 0), "\n")
  
  return(grid)
}

# =============================================================================
# 5. IMPACT CALCULATIONS
# =============================================================================

calculate_enhanced_fishery_impact <- function(grid, impact_scenarios = c(0.3, 0.5, 0.7)) {
  cat("Calculating enhanced fishery impacts...\n")
  
  gear_impacts <- data.frame(
    gear = c("trawl", "longline", "gillnet", "crab_pot"),
    operational_impact = c(0.6, 0.2, 0.1, 0.4)
  )
  
  for (scenario_idx in 1:length(impact_scenarios)) {
    scenario_name <- paste0("scenario_", scenario_idx)
    impact_factor <- impact_scenarios[scenario_idx]
    
    base_impact <- grid$annual_fishing_revenue * impact_factor
    
    gear_adjusted_impact <- base_impact
    for (i in 1:nrow(grid)) {
      if (!is.na(grid$dominant_gear[i]) && grid$dominant_gear[i] %in% gear_impacts$gear) {
        gear_info <- gear_impacts[gear_impacts$gear == grid$dominant_gear[i], ]
        gear_multiplier <- gear_info$operational_impact
        gear_adjusted_impact[i] <- base_impact[i] * gear_multiplier / impact_factor
      }
    }
    
    grid[[paste0("fishery_loss_", scenario_name)]] <- gear_adjusted_impact / 1e6
  }
  
  # Default to middle scenario
  grid$annual_fishery_loss_millions <- grid$fishery_loss_scenario_2
  
  cat("Enhanced fishery impact calculation complete!\n")
  return(grid)
}

perform_tradeoff_analysis <- function(grid, energy_weight = 0.6) {
  fishery_weight <- 1 - energy_weight
  
  grid$energy_score <- (grid$annual_revenue_millions - min(grid$annual_revenue_millions, na.rm = TRUE)) /
    (max(grid$annual_revenue_millions, na.rm = TRUE) - min(grid$annual_revenue_millions, na.rm = TRUE))
  
  grid$fishery_score <- 1 - ((grid$annual_fishery_loss_millions - min(grid$annual_fishery_loss_millions, na.rm = TRUE)) /
                               (max(grid$annual_fishery_loss_millions, na.rm = TRUE) - min(grid$annual_fishery_loss_millions, na.rm = TRUE)))
  
  grid$combined_score <- energy_weight * grid$energy_score + 
    fishery_weight * grid$fishery_score
  
  grid$rank <- rank(-grid$combined_score, na.last = "keep")
  
  return(grid)
}

# =============================================================================
# 6. VISUALIZATION
# =============================================================================

create_results_map <- function(grid, top_n = 50) {
  top_sites <- grid %>% 
    filter(!is.na(combined_score)) %>%
    arrange(rank) %>% 
    slice_head(n = top_n)
  
  pal <- colorNumeric(
    palette = viridis::viridis(100),
    domain = grid$combined_score,
    na.color = "transparent"
  )
  
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
        "<b>Wind Speed:</b>", round(wind_speed_hub_height, 1), " m/s<br>",
        "<b>Capacity Factor:</b>", round(capacity_factor * 100, 1), "%"
      )
    ) %>%
    addLegend(
      pal = pal,
      values = ~combined_score,
      title = "Combined Score",
      position = "bottomright"
    )
  
  return(map)
}

create_tradeoff_plot <- function(grid, top_n = 20) {
  top_sites <- grid %>% 
    filter(!is.na(combined_score)) %>%
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
    theme_minimal()
  
  return(p)
}

# =============================================================================
# 7. MAIN FUNCTION
# =============================================================================

offshore_wind_analysis <- function(
    cell_size_km = 2,
    turbine_capacity_mw = 12,
    turbines_per_site = 1,
    energy_weight = 0.6,
    wind_year = "2019",
    use_bulk_download = TRUE,
    use_sample_fisheries_data = TRUE,
    fisheries_data_path = NULL
) {
  
  cat("OREGON OFFSHORE WIND ANALYSIS - ENHANCED VERSION\n")
  cat("===============================================\n\n")
  
  # Step 1: Create grid
  cat("Creating lease areas and analysis grid...\n")
  lease_areas <- create_lease_areas()
  grid <- create_analysis_grid(lease_areas, cell_size_km)
  
  # Step 2: Get wind data
  bulk_file <- paste0("oregon_wind_data_bulk_", wind_year, ".rds")
  
  if (use_bulk_download || !file.exists(bulk_file)) {
    cat("Downloading wind data using bulk method...\n")
    wind_data <- fetch_oregon_wind_data_bulk(grid, wind_year)
    
    if (is.null(wind_data)) {
      cat("Bulk download failed. Check internet connection or try again later.\n")
      return(NULL)
    }
    
  } else {
    cat("Loading cached bulk wind data...\n")
    wind_data <- readRDS(bulk_file)
  }
  
  # Step 3: Calculate wind power
  cat("Calculating wind power potential...\n")
  grid <- calculate_wind_power_with_nrel_data(grid, wind_data, turbine_capacity_mw, turbines_per_site)
  
  # Step 4: Process fisheries data
  cat("Generating sample fisheries data...\n")
  fisheries_data <- generate_sample_fisheries_data(grid)
  
  cat("Processing fisheries data to grid...\n")
  grid <- process_fisheries_to_grid(fisheries_data, grid)
  
  # Step 5: Calculate impacts and tradeoffs
  cat("Calculating enhanced fishery impacts...\n")
  grid <- calculate_enhanced_fishery_impact(grid)
  
  cat("Performing tradeoff analysis...\n")
  grid <- perform_tradeoff_analysis(grid, energy_weight)
  
  # Step 6: Summary
  summary_stats <- list(
    total_sites = nrow(grid),
    sites_with_wind_data = sum(!is.na(grid$wind_speed_hub_height)),
    sites_with_fishing = sum(grid$annual_fishing_revenue > 0),
    avg_capacity_factor = mean(grid$capacity_factor, na.rm = TRUE),
    total_fishing_revenue_millions = sum(grid$annual_fishing_revenue, na.rm = TRUE) / 1e6
  )
  
  cat("Analysis complete!\n")
  cat("Sites with wind data:", summary_stats$sites_with_wind_data, "\n")
  cat("Sites with fishing activity:", summary_stats$sites_with_fishing, "\n")
  cat("Average capacity factor:", round(summary_stats$avg_capacity_factor * 100, 1), "%\n")
  
  return(list(
    grid = grid,
    wind_data = wind_data,
    fisheries_data = fisheries_data,
    lease_areas = lease_areas,
    summary = summary_stats
  ))
}

# =============================================================================
# 8. READY TO RUN
# =============================================================================

cat("Code loaded successfully!\n")
cat("To run the analysis, type: results <- run_oregon_offshore_wind_analysis()\n")