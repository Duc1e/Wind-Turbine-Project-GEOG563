# Step 1 Map - Simple and Robust Version
library(sf)
library(ggplot2)
library(dplyr)
library(maps)

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

# Create data first
cat("Creating lease areas and grid...\n")
lease_areas <- create_lease_areas()
reference_grid <- create_analysis_grid(lease_areas)

# Get Oregon state data
cat("Loading Oregon map data...\n")
oregon_state <- map_data("state", region = "oregon")

# Create lease area data frames
coos_coords <- data.frame(
  long = c(-125.5, -124.8, -124.8, -125.5, -125.5),
  lat = c(43.8, 43.8, 44.3, 44.3, 43.8),
  group = 1,
  area = "Coos Bay"
)

brookings_coords <- data.frame(
  long = c(-124.8, -124.3, -124.3, -124.8, -124.8),
  lat = c(42.0, 42.0, 42.8, 42.8, 42.0),
  group = 2,
  area = "Brookings"
)

lease_polygons <- rbind(coos_coords, brookings_coords)

# Get turbine locations
grid_centroids <- st_centroid(st_geometry(reference_grid))
coords <- st_coordinates(grid_centroids)
turbine_sample <- data.frame(
  lon = coords[seq(1, nrow(coords), by = 3), 1],
  lat = coords[seq(1, nrow(coords), by = 3), 2]
)

# Cities
cities <- data.frame(
  name = c("Coos Bay", "Brookings"),
  lon = c(-124.22, -124.28),
  lat = c(43.37, 42.05)
)

# Create the plot step by step
cat("Creating map...\n")

base_plot <- ggplot()

# Add ocean background
ocean_plot <- base_plot + 
  geom_rect(aes(xmin = -126, xmax = -123.5, ymin = 41.5, ymax = 45), 
            fill = "lightblue", alpha = 0.3)

# Add Oregon state
oregon_plot <- ocean_plot +
  geom_polygon(data = oregon_state, 
               aes(x = long, y = lat, group = group), 
               fill = "wheat", color = "brown", size = 0.5)

# Add lease areas
lease_plot <- oregon_plot +
  geom_polygon(data = lease_polygons, 
               aes(x = long, y = lat, group = group, fill = area), 
               alpha = 0.7, color = "navy", size = 1)

# Add turbine points
turbine_plot <- lease_plot +
  geom_point(data = turbine_sample, 
             aes(x = lon, y = lat), 
             color = "red", size = 1, alpha = 0.8)

# Add cities
city_plot <- turbine_plot +
  geom_point(data = cities, 
             aes(x = lon, y = lat), 
             color = "darkblue", size = 3, shape = 17) +
  geom_text(data = cities, 
            aes(x = lon, y = lat, label = name), 
            nudge_x = 0.1, nudge_y = 0.08, 
            fontface = "bold", size = 3, color = "darkblue")

# Final styling
final_plot <- city_plot +
  scale_fill_manual(values = c("Coos Bay" = "blue", "Brookings" = "green"),
                    name = "Lease Areas") +
  coord_map("mercator", xlim = c(-125.6, -123.5), ylim = c(41.5, 44.5)) +
  labs(
    title = "Oregon Offshore Wind Development Areas",
    subtitle = paste("Federal lease areas with", nrow(reference_grid), "potential turbine sites"),
    x = "Longitude",
    y = "Latitude",
    caption = "Red dots: sample turbine locations"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom"
  )

# Display the map
print(final_plot)

# Save the map
ggsave("oregon_wind_map_final.png", final_plot, 
       width = 12, height = 9, dpi = 300, bg = "white")

cat("Map created and saved successfully!\n")
cat("Total turbine sites:", nrow(reference_grid), "\n")
cat("Sample shown on map:", nrow(turbine_sample), "\n")