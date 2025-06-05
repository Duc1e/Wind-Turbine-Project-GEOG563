# Step 3 - Fishing Opportunity Cost Heat Map
library(ggplot2)
library(dplyr)
library(maps)

# Generate fishing opportunity cost data for offshore Oregon
create_fishing_cost_grid <- function() {
  # Create high-resolution grid for smooth color gradient (same as wind map)
  lat_seq <- seq(42.0, 44.5, by = 0.01)  # 0.01 degree resolution
  lon_seq <- seq(-125.8, -123.5, by = 0.01)
  
  fishing_grid <- expand.grid(lat = lat_seq, lon = lon_seq)
  
  # Define major fishing ports/harbors
  harbors <- data.frame(
    name = c("Newport", "Coos Bay", "Brookings", "Bandon", "Florence"),
    lon = c(-124.05, -124.22, -124.28, -124.41, -124.10),
    lat = c(44.63, 43.37, 42.05, 43.12, 43.98),
    importance = c(1.0, 0.9, 0.7, 0.5, 0.6)  # Relative fishing importance
  )
  
  # Calculate fishing opportunity cost factors
  
  # 1. Distance from shore (closer = higher fishing value)
  fishing_grid$distance_offshore <- abs(fishing_grid$lon + 124.5)  # Distance from ~coast
  fishing_grid$shore_effect <- pmax(0, 2.0 - fishing_grid$distance_offshore * 3.5)  # Drops off quickly offshore
  
  # 2. Proximity to fishing harbors (closer = higher value)
  fishing_grid$harbor_effect <- 0
  for(i in 1:nrow(harbors)) {
    # Calculate distance to each harbor
    distance_to_harbor <- sqrt((fishing_grid$lat - harbors$lat[i])^2 + 
                                 (fishing_grid$lon - harbors$lon[i])^2)
    # Add weighted proximity effect (closer = higher value)
    harbor_value <- harbors$importance[i] * pmax(0, 1.5 - distance_to_harbor * 2.0)
    fishing_grid$harbor_effect <- fishing_grid$harbor_effect + harbor_value
  }
  
  # 3. Depth considerations (moderate depths best for fishing)
  # Simulate depth based on distance offshore
  fishing_grid$depth_estimate <- 50 + fishing_grid$distance_offshore * 180  # Rough depth model
  fishing_grid$depth_effect <- ifelse(fishing_grid$depth_estimate < 100, 0.8,  # Too shallow
                                      ifelse(fishing_grid$depth_estimate > 300, 0.3,  # Too deep
                                             1.0))  # Optimal depth
  
  # 4. Latitudinal fishing patterns (some species more concentrated in certain areas)
  fishing_grid$species_effect <- 0.7 + 
    0.3 * sin((fishing_grid$lat - 42) * 3) +  # Species variation by latitude
    0.2 * cos((fishing_grid$lat - 43) * 4)    # Additional variation
  
  # Combine all factors into fishing opportunity cost
  fishing_grid$fishing_cost <- (fishing_grid$shore_effect * 0.4 +           # 40% shore proximity
                                  fishing_grid$harbor_effect * 0.3 +          # 30% harbor proximity  
                                  fishing_grid$depth_effect * 0.2 +           # 20% depth suitability
                                  fishing_grid$species_effect * 0.1)          # 10% species distribution
  
  # Scale to realistic dollar values ($/turbine/year opportunity cost)
  # Create more variation by scaling the combined factor (0-2 range) to cost range
  max_cost <- max(fishing_grid$fishing_cost, na.rm = TRUE)
  min_cost <- min(fishing_grid$fishing_cost, na.rm = TRUE)
  
  # Normalize to 0-1 range first
  fishing_grid$fishing_cost_norm <- (fishing_grid$fishing_cost - min_cost) / (max_cost - min_cost)
  
  # Then scale to realistic $/turbine range with more variation
  fishing_grid$fishing_cost <- 2000 + fishing_grid$fishing_cost_norm * 18000  # $2K to $20K range
  
  # Only include offshore areas (same as wind map)
  offshore_areas <- fishing_grid[fishing_grid$lon < -124.0, ]
  
  return(offshore_areas)
}

# Create fishing cost data
cat("Generating fishing opportunity cost data...\n")
fishing_data <- create_fishing_cost_grid()

# Get Oregon state data (same as previous maps)
oregon_state <- map_data("state", region = "oregon")

# Lease area boundaries (same as previous maps)
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

# Fishing harbors for display
harbors <- data.frame(
  name = c("Newport", "Coos Bay", "Brookings", "Bandon", "Florence"),
  lon = c(-124.05, -124.22, -124.28, -124.41, -124.10),
  lat = c(44.63, 43.37, 42.05, 43.12, 43.98)
)

# Create fishing opportunity cost heat map
cat("Creating fishing opportunity cost heat map...\n")

fishing_cost_map <- ggplot() +
  # Fishing cost heat map
  geom_tile(data = fishing_data, 
            aes(x = lon, y = lat, fill = fishing_cost)) +
  
  # Oregon coastline
  geom_polygon(data = oregon_state, 
               aes(x = long, y = lat, group = group), 
               fill = "#F5DEB3", color = "#8B4513", size = 0.6) +
  
  # Lease area boundaries
  geom_polygon(data = lease_polygons, 
               aes(x = long, y = lat, group = group), 
               fill = NA, color = "white", size = 2, linetype = "solid") +
  
  # Lease area labels
  annotate("text", x = -125.15, y = 44.05, label = "Coos Bay\nLease Area", 
           color = "black", fontface = "bold", size = 4, hjust = 0.5) +
  annotate("text", x = -124.55, y = 42.4, label = "Brookings\nLease Area", 
           color = "black", fontface = "bold", size = 4, hjust = 0.5) +
  
  # Fishing harbors
  geom_point(data = harbors, 
             aes(x = lon, y = lat), 
             color = "#FF4500", size = 4, shape = 18) +  # Diamond shape for harbors
  geom_text(data = harbors, 
            aes(x = lon, y = lat, label = name), 
            nudge_x = 0.12, nudge_y = 0.08, 
            fontface = "bold", size = 3.5, color = "#FF4500") +
  
  # Fishing cost color scale (reverse viridis - high cost = warm colors)
  scale_fill_viridis_c(
    name = "Fishing Opportunity\nCost ($/turbine/year)",
    option = "inferno",
    direction = -1,  # Reverse so high costs are warm colors
    labels = function(x) paste0("$", round(x/1000, 1), "K"),
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = 15,
      barheight = 1
    )
  ) +
  
  # Map bounds and projection
  coord_map("mercator", xlim = c(-125.8, -123.5), ylim = c(42.0, 44.5)) +
  
  # Labels and styling
  labs(
    title = "Oregon Offshore Fishing Opportunity Cost Assessment",
    subtitle = "Annual fishing revenue opportunity cost per turbine placement",
    x = "Longitude (°W)",
    y = "Latitude (°N)",
    caption = "Higher costs (warmer colors) indicate areas with greater fishing activity and economic impact"
  ) +
  
  # Theme (consistent with wind map)
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
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  )

# Display the map
print(fishing_cost_map)

# Save high-quality version
ggsave("oregon_fishing_cost_map.png", fishing_cost_map, 
       width = 14, height = 10, dpi = 300, bg = "white")

# Print summary statistics
cat("\n=== FISHING OPPORTUNITY COST RESULTS ===\n")
cat("Fishing cost range: $", round(min(fishing_data$fishing_cost)), " to $", 
    round(max(fishing_data$fishing_cost)), " per turbine per year\n", sep = "")

# Calculate costs in lease areas
coos_fishing <- fishing_data[fishing_data$lon >= -125.5 & fishing_data$lon <= -124.8 & 
                               fishing_data$lat >= 43.8 & fishing_data$lat <= 44.3, ]
brook_fishing <- fishing_data[fishing_data$lon >= -124.8 & fishing_data$lon <= -124.3 & 
                                fishing_data$lat >= 42.0 & fishing_data$lat <= 42.8, ]

cat("Mean fishing opportunity cost in lease areas:\n")
cat("Coos Bay area: $", round(mean(coos_fishing$fishing_cost)), " per turbine per year\n", sep = "")
cat("Brookings area: $", round(mean(brook_fishing$fishing_cost)), " per turbine per year\n", sep = "")

# Calculate total potential fishing impact
total_turbines_coos <- 63  # Approximate from your 2x2km grid
total_turbines_brook <- 64  # Approximate from your 2x2km grid

cat("\nEstimated total fishing impact if all lease areas developed:\n")
cat("Coos Bay: $", round(mean(coos_fishing$fishing_cost) * total_turbines_coos / 1000), "K per year\n", sep = "")
cat("Brookings: $", round(mean(brook_fishing$fishing_cost) * total_turbines_brook / 1000), "K per year\n", sep = "")
cat("Total: $", round((mean(coos_fishing$fishing_cost) * total_turbines_coos + 
                         mean(brook_fishing$fishing_cost) * total_turbines_brook) / 1000), "K per year\n", sep = "")

cat("Heat map resolution:", nrow(fishing_data), "data points\n")
cat("Map saved as: oregon_fishing_cost_map.png\n")