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
library(shiny)        # Web app framework (optional)

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
# 2. WIND RESOURCE ASSESSMENT
# =============================================================================

# Calculate wind power potential for each grid cell
calculate_wind_power <- function(grid, base_wind_speed = 9.5, turbine_capacity_mw = 12, 
                                 turbines_per_site = 4) {
  
  # Simplified wind speed model based on distance from shore and latitude
  grid$dist_to_shore <- apply(st_coordinates(st_centroid(grid)), 1, function(coord) {
    # Approximate distance to Oregon coast
    min(geosphere::distGeo(coord, c(-124.0, coord[2]))) / 1000  # km
  })
  
  # Wind speed increases with distance from shore and varies by latitude
  grid$wind_speed <- base_wind_speed + 
    (grid$dist_to_shore - 20) * 0.05 +  # Increase with distance offshore
    (grid$lat - 43) * 0.3 +              # Latitudinal variation
    rnorm(nrow(grid), 0, 0.3)            # Add some realistic noise
  
  # Ensure reasonable wind speeds
  grid$wind_speed <- pmax(7, pmin(12, grid$wind_speed))
  
  # Calculate power using simplified power curve
  # Typical offshore wind turbine power curve approximation
  calculate_power_output <- function(wind_speed, rated_power_mw = turbine_capacity_mw) {
    cut_in <- 3      # m/s
    rated_speed <- 12 # m/s  
    cut_out <- 25    # m/s
    
    ifelse(wind_speed < cut_in, 0,
           ifelse(wind_speed < rated_speed, 
                  rated_power_mw * (wind_speed^3 - cut_in^3) / (rated_speed^3 - cut_in^3),
                  ifelse(wind_speed < cut_out, rated_power_mw, 0)))
  }
  
  # Calculate capacity factor (percentage of rated power achieved)
  grid$capacity_factor <- calculate_power_output(grid$wind_speed) / turbine_capacity_mw
  
  # Total capacity and annual generation
  grid$total_capacity_mw <- turbines_per_site * turbine_capacity_mw
  grid$annual_generation_gwh <- grid$total_capacity_mw * grid$capacity_factor * 8760 / 1000
  
  # Revenue estimate ($/MWh wholesale price)
  wholesale_price <- 45  # $/MWh (approximate)
  grid$annual_revenue_millions <- grid$annual_generation_gwh * wholesale_price / 1000
  
  return(grid)
}

# =============================================================================
# 3. FISHERY IMPACT ASSESSMENT  
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
# 4. TRADEOFF ANALYSIS AND OPTIMIZATION
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
# 5. VISUALIZATION FUNCTIONS
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
# 6. MAIN ANALYSIS FUNCTION
# =============================================================================

# Run complete analysis
run_offshore_wind_analysis <- function(
    cell_size_km = 2,
    base_wind_speed = 9.5,
    turbine_capacity_mw = 12,
    turbines_per_site = 4,
    base_fishery_value = 50000,
    impact_radius_km = 5,
    shore_distance_weight = 0.7,
    energy_weight = 0.6
) {
  
  cat("Creating lease areas and analysis grid...\n")
  lease_areas <- create_lease_areas()
  grid <- create_analysis_grid(lease_areas, cell_size_km)
  
  cat("Calculating wind power potential...\n")
  grid <- calculate_wind_power(grid, base_wind_speed, turbine_capacity_mw, turbines_per_site)
  
  cat("Assessing fishery impacts...\n")
  grid <- calculate_fishery_impact(grid, base_fishery_value, impact_radius_km, shore_distance_weight)
  
  cat("Performing tradeoff analysis...\n")
  grid <- perform_tradeoff_analysis(grid, energy_weight)
  
  # Summary statistics
  summary_stats <- list(
    total_sites = nrow(grid),
    total_capacity_gw = sum(grid$total_capacity_mw) / 1000,
    total_generation_twh = sum(grid$annual_generation_gwh) / 1000,
    avg_fishery_impact = mean(grid$annual_fishery_loss_millions),
    top_10_sites = grid %>% arrange(rank) %>% slice_head(n = 10)
  )
  
  cat("Analysis complete!\n")
  cat("Total sites analyzed:", summary_stats$total_sites, "\n")
  cat("Total potential capacity:", round(summary_stats$total_capacity_gw, 1), "GW\n")
  cat("Average fishery impact:", round(summary_stats$avg_fishery_impact, 2), "$M/year\n")
  
  return(list(
    grid = grid,
    lease_areas = lease_areas,
    summary = summary_stats
  ))
}

# =============================================================================
# 7. EXAMPLE USAGE
# =============================================================================

# Run the analysis with default parameters
cat("Starting Oregon Offshore Wind Analysis...\n")
results <- run_offshore_wind_analysis()

# Create visualizations
cat("Creating visualizations...\n")
map <- create_results_map(results$grid)
tradeoff_plot <- create_tradeoff_plot(results$grid)

# Display top 10 sites
cat("\nTop 10 Optimal Sites:\n")
top_sites_table <- results$summary$top_10_sites %>%
  select(site_id, rank, combined_score, annual_revenue_millions, 
         annual_fishery_loss_millions, wind_speed, capacity_factor) %>%
  mutate(
    combined_score = round(combined_score, 3),
    annual_revenue_millions = round(annual_revenue_millions, 1),
    annual_fishery_loss_millions = round(annual_fishery_loss_millions, 1),
    wind_speed = round(wind_speed, 1),
    capacity_factor = round(capacity_factor, 2)
  )

print(top_sites_table)

# Display interactive map and plot
cat("\nDisplaying interactive map and tradeoff plot...\n")
map
tradeoff_plot

# Save results for further analysis
# write_sf(results$grid, "oregon_wind_analysis_results.shp")
# saveRDS(results, "oregon_wind_analysis_results.rds")

cat("\nAnalysis complete! Use the 'results' object to explore the data further.\n")
cat("Access the grid data with: results$grid\n")
cat("View the interactive map with: map\n")
cat("View the tradeoff plot with: tradeoff_plot\n")

# =============================================================================
# 8. EXTENSIBILITY FUNCTIONS
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