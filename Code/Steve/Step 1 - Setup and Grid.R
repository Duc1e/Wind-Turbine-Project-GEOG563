# Step 1: Oregon Offshore Wind Analysis - Setup and Grid Creation
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

# Create 2x2km analysis grid
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

# Create the actual grid
lease_areas <- create_lease_areas()
grid <- create_analysis_grid(lease_areas)

cat("Step 1 Complete: Created grid with", nrow(grid), "cells\n")
cat("Grid covers Oregon lease areas from", round(min(grid$lat), 2), "to", round(max(grid$lat), 2), "degrees latitude\n")
