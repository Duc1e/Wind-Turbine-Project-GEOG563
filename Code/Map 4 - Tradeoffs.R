# Wind-Fishing Trade-off Analysis with Normalized Values
library(ggplot2)
library(dplyr)
library(maps)

# Generate normalized trade-off data
create_normalized_tradeoff_grid <- function() {
  # Create high-resolution grid (same as previous maps)
  lat_seq <- seq(42.0, 44.5, by = 0.01)
  lon_seq <- seq(-125.8, -123.5, by = 0.01)
  
  tradeoff_grid <- expand.grid(lat = lat_seq, lon = lon_seq)
  
  # === WIND REVENUE CALCULATION ===
  # (Based on your wind speed heat map algorithm)
  
  # Calculate wind speed (same as wind map)
  tradeoff_grid$distance_offshore <- abs(tradeoff_grid$lon + 124.5)
  tradeoff_grid$latitude_effect <- (tradeoff_grid$lat - 42) * 0.8
  
  tradeoff_grid$wind_speed <- 7.5 + 
    tradeoff_grid$distance_offshore * 2.2 +  # Increases offshore
    tradeoff_grid$latitude_effect +           # Increases north
    sin(tradeoff_grid$lat * 8) * 0.4 +       # Coastal variation
    cos(tradeoff_grid$lon * 12) * 0.3        # Offshore variation
  
  # Constrain wind speeds
  tradeoff_grid$wind_speed[tradeoff_grid$wind_speed < 6] <- 6
  tradeoff_grid$wind_speed[tradeoff_grid$wind_speed > 11] <- 11
  
  # Convert to capacity factor (simplified power curve)
  tradeoff_grid$capacity_factor <- pmax(0, pmin(1, (tradeoff_grid$wind_speed - 3) / 8))
  
  # Calculate annual wind revenue (10MW turbine, $50/MWh)
  tradeoff_grid$annual_wind_revenue <- tradeoff_grid$capacity_factor * 10 * 8760 * 50  # MW * hours * $/MWh
  
  # === FISHING COST CALCULATION ===
  # (Using enhanced spatial algorithm)
  
  # Define major fishing harbors
  harbors <- data.frame(
    name = c("Newport", "Coos Bay", "Brookings", "Bandon", "Florence"),
    lon = c(-124.05, -124.22, -124.28, -124.41, -124.10),
    lat = c(44.63, 43.37, 42.05, 43.12, 43.98),
    importance = c(1.0, 0.9, 0.7, 0.5, 0.6)
  )
  
  # 1. Shore proximity effect
  tradeoff_grid$shore_effect <- pmax(0, 2.0 - tradeoff_grid$distance_offshore * 3.5)
  
  # 2. Harbor proximity effect
  tradeoff_grid$harbor_effect <- 0
  for(i in 1:nrow(harbors)) {
    distance_to_harbor <- sqrt((tradeoff_grid$lat - harbors$lat[i])^2 + 
                                 (tradeoff_grid$lon - harbors$lon[i])^2)
    harbor_value <- harbors$importance[i] * pmax(0, 1.5 - distance_to_harbor * 2.0)
    tradeoff_grid$harbor_effect <- tradeoff_grid$harbor_effect + harbor_value
  }
  
  # 3. Depth effect
  tradeoff_grid$estimated_depth <- 50 + tradeoff_grid$distance_offshore * 180
  tradeoff_grid$depth_effect <- ifelse(tradeoff_grid$estimated_depth < 100, 0.8,
                                       ifelse(tradeoff_grid$estimated_depth > 300, 0.3, 1.0))
  
  # 4. Species distribution effect
  tradeoff_grid$species_effect <- 0.7 + 
    0.3 * sin((tradeoff_grid$lat - 42) * 3) +
    0.2 * cos((tradeoff_grid$lat - 43) * 4)
  
  # Combine fishing factors
  tradeoff_grid$fishing_intensity <- (tradeoff_grid$shore_effect * 0.4 +
                                        tradeoff_grid$harbor_effect * 0.3 +
                                        tradeoff_grid$depth_effect * 0.2 +
                                        tradeoff_grid$species_effect * 0.1)
  
  # Scale to fishing opportunity cost (50% of fishing revenue lost)
  lease_area_fishing_revenue <- 189e6 * 0.05  # $9.45M total
  total_intensity <- sum(tradeoff_grid$fishing_intensity)
  tradeoff_grid$fishing_revenue <- (tradeoff_grid$fishing_intensity / total_intensity) * lease_area_fishing_revenue
  tradeoff_grid$fishing_opportunity_cost <- tradeoff_grid$fishing_revenue * 0.5
  
  # === NORMALIZATION FOR COMPARISON ===
  
  # Normalize both values to 0-1 scale for fair comparison
  tradeoff_grid$wind_revenue_norm <- (tradeoff_grid$annual_wind_revenue - min(tradeoff_grid$annual_wind_revenue)) /
    (max(tradeoff_grid$annual_wind_revenue) - min(tradeoff_grid$annual_wind_revenue))
  
  tradeoff_grid$fishing_cost_norm <- (tradeoff_grid$fishing_opportunity_cost - min(tradeoff_grid$fishing_opportunity_cost)) /
    (max(tradeoff_grid$fishing_opportunity_cost) - min(tradeoff_grid$fishing_opportunity_cost))
  
  # Calculate normalized net benefit (wind benefit minus fishing cost)
  tradeoff_grid$normalized_net_benefit <- tradeoff_grid$wind_revenue_norm - tradeoff_grid$fishing_cost_norm
  
  # Calculate wind-to-fishing ratio (for alternative visualization)
  tradeoff_grid$wind_fishing_ratio <- tradeoff_grid$annual_wind_revenue / (tradeoff_grid$fishing_opportunity_cost + 1000)  # Add small constant to avoid division by zero
  
  # Only include offshore areas
  offshore_areas <- tradeoff_grid[tradeoff_grid$lon < -124.0, ]
  
  return(offshore_areas)
}

# Create trade-off data
cat("Generating normalized wind-fishing trade-off data...\n")
tradeoff_data <- create_normalized_tradeoff_grid()

# Get map components (same as previous maps)
oregon_state <- map_data("state", region = "oregon")

# Lease area boundaries
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

# Cities and harbors for reference
cities <- data.frame(
  name = c("Coos Bay", "Brookings", "Newport"),
  lon = c(-124.22, -124.28, -124.05),
  lat = c(43.37, 42.05, 44.63)
)

# Create normalized trade-off heat map
cat("Creating normalized wind-fishing trade-off map...\n")

tradeoff_map <- ggplot() +
  # Normalized net benefit heat map
  geom_tile(data = tradeoff_data, 
            aes(x = lon, y = lat, fill = normalized_net_benefit)) +
  
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
  
  # Cities for reference
  geom_point(data = cities, 
             aes(x = lon, y = lat), 
             color = "#000080", size = 3, shape = 17) +
  geom_text(data = cities, 
            aes(x = lon, y = lat, label = name), 
            nudge_x = 0.12, nudge_y = 0.08, 
            fontface = "bold", size = 3, color = "#000080") +
  
  # Diverging color scale: red = fishing dominates, blue = wind dominates
  scale_fill_gradient2(
    name = "Normalized\nNet Benefit",
    low = "#D73027",      # Red: High fishing cost relative to wind benefit
    mid = "#FFFFBF",      # Yellow: Balanced trade-off
    high = "#1A9850",     # Green: High wind benefit relative to fishing cost
    midpoint = 0,
    breaks = seq(-1, 1, by = 0.5),
    labels = c("Fishing\nDominates", "Balanced\nTrade-off", "", "", "Wind\nDominates"),
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
    title = "Oregon Offshore Wind Development Optimization",
    subtitle = "Normalized trade-off analysis: Wind revenue potential vs. fishing opportunity cost",
    x = "Longitude (°W)",
    y = "Latitude (°N)",
    caption = "Green areas = optimal sites (high wind, low fishing impact) | Red areas = avoid (low wind, high fishing impact)"
  ) +
  
  # Theme
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, color = "#2F4F4F"),
    plot.subtitle = element_text(size = 13, hjust = 0.5, color = "#556B2F"),
    plot.caption = element_text(size = 11, color = "#696969"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  )

# Display the map
print(tradeoff_map)

# Save high-quality version
ggsave("oregon_wind_fishing_tradeoff_map.png", tradeoff_map, 
       width = 14, height = 10, dpi = 300, bg = "white")

# Print summary statistics
cat("\n=== NORMALIZED TRADE-OFF ANALYSIS RESULTS ===\n")
cat("WIND REVENUE (Original Scale):\n")
cat("Range: $", format(round(min(tradeoff_data$annual_wind_revenue)), big.mark=","), 
    " to $", format(round(max(tradeoff_data$annual_wind_revenue)), big.mark=","), " per turbine per year\n", sep="")
cat("Mean: $", format(round(mean(tradeoff_data$annual_wind_revenue)), big.mark=","), " per turbine per year\n", sep="")

cat("\nFISHING OPPORTUNITY COST (Original Scale):\n")
cat("Range: $", format(round(min(tradeoff_data$fishing_opportunity_cost)), big.mark=","), 
    " to $", format(round(max(tradeoff_data$fishing_opportunity_cost)), big.mark=","), " per turbine per year\n", sep="")
cat("Mean: $", format(round(mean(tradeoff_data$fishing_opportunity_cost)), big.mark=","), " per turbine per year\n", sep="")

cat("\nNORMALIZED NET BENEFIT:\n")
cat("Range:", round(min(tradeoff_data$normalized_net_benefit), 3), "to", round(max(tradeoff_data$normalized_net_benefit), 3), "\n")
cat("Mean:", round(mean(tradeoff_data$normalized_net_benefit), 3), "\n")

# Calculate lease area summaries
coos_tradeoff <- tradeoff_data[tradeoff_data$lon >= -125.5 & tradeoff_data$lon <= -124.8 & 
                                 tradeoff_data$lat >= 43.8 & tradeoff_data$lat <= 44.3, ]
brook_tradeoff <- tradeoff_data[tradeoff_data$lon >= -124.8 & tradeoff_data$lon <= -124.3 & 
                                  tradeoff_data$lat >= 42.0 & tradeoff_data$lat <= 42.8, ]

cat("\nLEASE AREA COMPARISON:\n")
cat("Coos Bay - Mean normalized net benefit:", round(mean(coos_tradeoff$normalized_net_benefit), 3), "\n")
cat("Brookings - Mean normalized net benefit:", round(mean(brook_tradeoff$normalized_net_benefit), 3), "\n")

# Identify optimal zones
optimal_sites <- tradeoff_data[tradeoff_data$normalized_net_benefit > 0.5, ]
cat("\nOPTIMAL DEVELOPMENT ZONES (normalized net benefit > 0.5):\n")
cat("Number of optimal grid cells:", nrow(optimal_sites), "out of", nrow(tradeoff_data), 
    "(", round(100 * nrow(optimal_sites) / nrow(tradeoff_data), 1), "%)\n")

cat("Heat map resolution:", nrow(tradeoff_data), "data points\n")
cat("Map saved as: oregon_wind_fishing_tradeoff_map.png\n")