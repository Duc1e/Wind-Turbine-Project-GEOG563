# Step 1: Oregon Offshore Wind Analysis - Grid Creation and NREL Data Integration
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

# Create Oregon lease areas
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

# Create 2x2km analysis grid (reference grid for planning)
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

# Function to filter NREL wind data to lease areas
filter_nrel_to_lease_areas <- function(all_wind_data, lease_areas) {
  # Create unique NREL locations
  nrel_unique_locations <- all_wind_data %>%
    select(site_id, latitude, longitude) %>%
    distinct()
  
  # Convert to spatial data
  nrel_sf <- st_as_sf(nrel_unique_locations, 
                      coords = c("longitude", "latitude"), 
                      crs = 4326)
  
  # Filter NREL points to lease areas only
  nrel_in_lease <- st_filter(nrel_sf, lease_areas)
  
  # Get the site_ids for filtering
  lease_site_ids <- nrel_in_lease$site_id
  
  # Filter wind data to lease areas only
  wind_data_lease <- all_wind_data %>%
    filter(site_id %in% lease_site_ids)
  
  # Print summary
  cat("=== NREL DATA FILTERED TO LEASE AREAS ===\n")
  cat("NREL points in lease areas:", nrow(nrel_in_lease), "out of", nrow(nrel_sf), "total\n")
  cat("Wind data rows in lease areas:", nrow(wind_data_lease), "out of", nrow(all_wind_data), "total\n")
  cat("Unique sites in lease areas:", length(unique(wind_data_lease$site_id)), "\n")
  cat("Lat range:", round(min(wind_data_lease$latitude), 3), "to", round(max(wind_data_lease$latitude), 3), "\n")
  cat("Lon range:", round(min(wind_data_lease$longitude), 3), "to", round(max(wind_data_lease$longitude), 3), "\n")
  
  return(list(
    wind_data_lease = wind_data_lease,
    nrel_points_lease = nrel_in_lease,
    lease_site_ids = lease_site_ids
  ))
}

# Create the lease areas and reference grid
lease_areas <- create_lease_areas()
reference_grid <- create_analysis_grid(lease_areas)

cat("Step 1 Complete: Created lease areas and reference grid\n")
cat("Reference grid:", nrow(reference_grid), "cells (2x2km each)\n")
cat("Grid covers Oregon lease areas from", round(min(reference_grid$lat), 2), "to", round(max(reference_grid$lat), 2), "degrees latitude\n")

# Visualization function
plot_lease_areas_and_grid <- function(lease_areas, nrel_points = NULL, reference_grid = NULL) {
  p <- ggplot() +
    geom_sf(data = lease_areas, fill = "lightblue", alpha = 0.3, color = "blue", size = 1) +
    coord_sf(xlim = c(-125.5, -124.3), ylim = c(42, 44.3)) +
    labs(title = "Oregon Offshore Wind Lease Areas") +
    theme_minimal()
  
  if (!is.null(nrel_points)) {
    p <- p + geom_sf(data = nrel_points, color = "red", size = 0.8) +
      labs(subtitle = paste("With", nrow(nrel_points), "NREL wind data points"))
  }
  
  if (!is.null(reference_grid)) {
    p <- p + geom_sf(data = reference_grid, fill = NA, color = "gray", alpha = 0.5, size = 0.3)
  }
  
  return(p)
}

# Plot lease areas
plot_lease_areas_and_grid(lease_areas)

cat("\n=== READY FOR STEP 2: NREL DATA PROCESSING ===\n")
cat("Once NREL wind data is loaded as 'all_wind_data', run:\n")
cat("lease_data <- filter_nrel_to_lease_areas(all_wind_data, lease_areas)\n")