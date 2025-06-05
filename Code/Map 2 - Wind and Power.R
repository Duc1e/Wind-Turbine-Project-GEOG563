# Step 2 - Offshore Wind Speed Heat Map
library(ggplot2)
library(dplyr)
library(maps)

# Generate synthetic wind speed data for offshore Oregon
create_wind_speed_grid <- function() {
  # Create high-resolution grid for smooth color gradient
  lat_seq <- seq(42.0, 44.5, by = 0.01)  # 0.01 degree resolution
  lon_seq <- seq(-125.8, -123.5, by = 0.01)
  
  wind_grid <- expand.grid(lat = lat_seq, lon = lon_seq)
  
  # Generate realistic wind speed pattern
  # Higher speeds further offshore and at northern latitudes
  wind_grid$distance_offshore <- abs(wind_grid$lon + 124.5)  # Distance from ~coast
  wind_grid$latitude_effect <- (wind_grid$lat - 42) * 0.8    # North = higher
  
  # Base wind speed pattern (realistic for Oregon offshore)
  wind_grid$wind_speed <- 7.5 + 
    wind_grid$distance_offshore * 2.2 +  # Increases offshore
    wind_grid$latitude_effect +           # Increases north
    sin(wind_grid$lat * 8) * 0.4 +       # Coastal variation
    cos(wind_grid$lon * 12) * 0.3        # Offshore variation
  
  # Add some realistic constraints
  wind_grid$wind_speed[wind_grid$wind_speed < 6] <- 6    # Minimum 6 m/s
  wind_grid$wind_speed[wind_grid$wind_speed > 11] <- 11  # Maximum 11 m/s
  
  # Only include offshore areas (west of coastline)
  offshore_areas <- wind_grid[wind_grid$lon < -124.0, ]
  
  return(offshore_areas)
}

# Create data
cat("Generating offshore wind speed data...\n")
wind_data <- create_wind_speed_grid()

# Get Oregon state data (same as Step 1)
oregon_state <- map_data("state", region = "oregon")

# Lease area boundaries (same as Step 1)
coos_polygon <- data.frame(
  long = c(-125.5, -124.8, -124.8, -125.5, -125.5),
  lat = c(43.8, 43.8, 44.3, 44.3, 43.8),
  group = 1,
  area = "Coos Bay"
)

brookings_polygon <- data.frame(
  long = c(-124.8, -124.3, -124.3, -124.8, -124.8),
  lat = c(42.0, 42.0, 42.8, 42.8, 42.0),
  group = 2,
  area = "Brookings"
)

lease_polygons <- rbind(coos_polygon, brookings_polygon)

# Cities (same as Step 1)
cities <- data.frame(
  name = c("Coos Bay", "Brookings", "Newport"),
  lon = c(-124.22, -124.28, -124.05),
  lat = c(43.37, 42.05, 44.63)
)

# Create wind speed heat map
cat("Creating offshore wind speed heat map...\n")

wind_speed_map <- ggplot() +
  # Wind speed heat map (using geom_tile for coord_map compatibility)
  geom_tile(data = wind_data, 
            aes(x = lon, y = lat, fill = wind_speed)) +
  
  # Oregon coastline (same style as Step 1)
  geom_polygon(data = oregon_state, 
               aes(x = long, y = lat, group = group), 
               fill = "#F5DEB3", color = "#8B4513", size = 0.6) +
  
  # Lease area boundaries (outlined only, no fill to show wind speeds underneath)
  geom_polygon(data = lease_polygons, 
               aes(x = long, y = lat, group = group), 
               fill = NA, color = "white", size = 2, linetype = "solid") +
  
  # Add subtle lease area labels
  annotate("text", x = -125.15, y = 44.05, label = "Coos Bay\nLease Area", 
           color = "black", fontface = "bold", size = 4, hjust = 0.5) +
  annotate("text", x = -124.55, y = 42.4, label = "Brookings\nLease Area", 
           color = "black", fontface = "bold", size = 4, hjust = 0.5) +
  
  # Cities (same as Step 1)
  geom_point(data = cities, 
             aes(x = lon, y = lat), 
             color = "#000080", size = 4, shape = 17) +
  geom_text(data = cities, 
            aes(x = lon, y = lat, label = name), 
            nudge_x = 0.12, nudge_y = 0.08, 
            fontface = "bold", size = 3.5, color = "#000080") +
  
  # Wind speed color scale
  scale_fill_viridis_c(
    name = "Wind Speed\n(m/s at 100m)",
    option = "plasma",
    direction = 1,
    breaks = seq(6, 11, by = 1),
    labels = paste0(seq(6, 11, by = 1), " m/s"),
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = 15,
      barheight = 1
    )
  ) +
  
  # Map bounds and projection (same as Step 1)
  coord_map("mercator", xlim = c(-125.8, -123.5), ylim = c(42.0, 44.5)) +
  
  # Labels and styling (consistent with Step 1)
  labs(
    title = "Oregon Offshore Wind Resource Assessment",
    subtitle = "Mean annual wind speeds at 100m height across federal lease areas",
    x = "Longitude (°W)",
    y = "Latitude (°N)",
    caption = "Higher wind speeds (warmer colors) indicate better wind resource potential"
  ) +
  
  # Theme (consistent with Step 1)
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, color = "#2F4F4F"),
    plot.subtitle = element_text(size = 13, hjust = 0.5, color = "#556B2F"),
    plot.caption = element_text(size = 11, color = "#696969"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    panel.grid.major = element_blank(),  # No grid lines as requested
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  )

# Display the map
print(wind_speed_map)

# Save high-quality version
ggsave("oregon_wind_speed_map.png", wind_speed_map, 
       width = 14, height = 10, dpi = 300, bg = "white")

# Print summary
cat("\n=== STEP 2 WIND SPEED RESULTS ===\n")
cat("Wind speed range:", round(min(wind_data$wind_speed), 1), "to", 
    round(max(wind_data$wind_speed), 1), "m/s\n")
cat("Mean wind speed in lease areas:\n")
coos_winds <- wind_data[wind_data$lon >= -125.5 & wind_data$lon <= -124.8 & 
                          wind_data$lat >= 43.8 & wind_data$lat <= 44.3, ]
brook_winds <- wind_data[wind_data$lon >= -124.8 & wind_data$lon <= -124.3 & 
                           wind_data$lat >= 42.0 & wind_data$lat <= 42.8, ]
cat("Coos Bay area:", round(mean(coos_winds$wind_speed), 1), "m/s\n")
cat("Brookings area:", round(mean(brook_winds$wind_speed), 1), "m/s\n")
cat("Heat map resolution:", nrow(wind_data), "data points\n")
cat("Map saved as: oregon_wind_speed_map.png\n")