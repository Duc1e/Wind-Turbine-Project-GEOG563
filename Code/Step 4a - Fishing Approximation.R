# Step 4: Oregon Offshore Wind Analysis - Fisheries Economic Data Creation
# Prerequisites: Run Steps 1-3 (lease areas, wind data, power calculations)

library(dplyr)
library(sf)
library(ggplot2)

# Function to create synthetic fisheries data based on Oregon research
create_fisheries_data <- function(annual_power, method = "distance_based") {
  
  cat("=== CREATING FISHERIES ECONOMIC DATA ===\n")
  cat("Based on Oregon commercial fishing research and spatial patterns\n\n")
  
  # Oregon commercial fishing economics (2018-2022 average: $189M statewide)
  # Key fisheries: Dungeness crab, groundfish, whiting, salmon, shrimp
  
  # Convert power data to spatial object
  power_sf <- st_as_sf(annual_power, coords = c("longitude", "latitude"), crs = 4326)
  
  if (method == "distance_based") {
    # Method 1: Distance-based fishing intensity
    # Assumption: Fishing activity decreases with distance from shore
    
    # Calculate distance to nearest shore (simplified)
    # Approximate Oregon coast as line from 42°N to 44.3°N at -125.5°W (approximate shore)
    coast_line <- data.frame(
      lat = seq(42, 44.3, by = 0.1),
      lon = rep(-125.5, length(seq(42, 44.3, by = 0.1)))
    )
    
    power_coords <- st_coordinates(power_sf)
    
    # Calculate approximate distance to shore (degrees, then convert to km)
    annual_power$dist_to_shore_km <- apply(power_coords, 1, function(point) {
      distances <- sqrt((point[1] - coast_line$lon)^2 + (point[2] - coast_line$lat)^2)
      min(distances) * 111  # Convert degrees to km (approximate)
    })
    
    # Fishing intensity decreases exponentially with distance from shore
    # Based on research: most Oregon commercial fishing is within 30 nautical miles (55 km)
    annual_power$fishing_intensity <- exp(-annual_power$dist_to_shore_km / 20)
    
    # Normalize fishing intensity (0 to 1 scale)
    annual_power$fishing_intensity <- annual_power$fishing_intensity / max(annual_power$fishing_intensity)
    
  } else if (method == "depth_based") {
    # Method 2: Depth-based fishing patterns
    # Different fisheries operate at different depths
    
    # Estimate depth based on distance from shore (rough approximation)
    # Oregon continental shelf: 0-200m depth within ~50km of shore
    annual_power$estimated_depth_m <- pmax(10, annual_power$dist_to_shore_km * 4)
    
    # Different fishing intensity by depth zone
    annual_power$fishing_intensity <- ifelse(
      annual_power$estimated_depth_m < 50, 0.9,      # Nearshore high intensity
      ifelse(annual_power$estimated_depth_m < 100, 0.7,  # Mid-shelf moderate
             ifelse(annual_power$estimated_depth_m < 200, 0.5,  # Shelf edge
                    0.2))  # Deep water low intensity
    )
  }
  
  # Calculate annual fishing revenue per 2x2km grid cell
  # Based on Oregon statewide revenue distributed by fishing intensity
  
  # Oregon lease area represents approximately 5% of Oregon's fishing grounds
  lease_area_total_revenue <- 189e6 * 0.05  # $9.45M for lease areas (2018-2022 avg)
  
  # Distribute revenue based on fishing intensity
  total_intensity <- sum(annual_power$fishing_intensity)
  annual_power$annual_fishing_revenue_usd <- (annual_power$fishing_intensity / total_intensity) * lease_area_total_revenue
  
  # Add fishery composition (based on Oregon research)
  annual_power$crab_revenue <- annual_power$annual_fishing_revenue_usd * 0.35      # Dungeness crab - 35%
  annual_power$groundfish_revenue <- annual_power$annual_fishing_revenue_usd * 0.30  # Groundfish - 30%
  annual_power$whiting_revenue <- annual_power$annual_fishing_revenue_usd * 0.15     # Pacific whiting - 15%
  annual_power$salmon_revenue <- annual_power$annual_fishing_revenue_usd * 0.10      # Salmon - 10%
  annual_power$other_revenue <- annual_power$annual_fishing_revenue_usd * 0.10       # Other species - 10%
  
  # Calculate revenue per km² (each grid cell is 4 km²)
  annual_power$fishing_revenue_per_km2 <- annual_power$annual_fishing_revenue_usd / 4
  
  cat("Fisheries data created using", method, "approach\n")
  cat("Total fishing revenue in lease areas: $", format(round(sum(annual_power$annual_fishing_revenue_usd)), big.mark=","), "\n", sep="")
  cat("Fishing revenue range: $", format(round(min(annual_power$annual_fishing_revenue_usd)), big.mark=","), 
      " to $", format(round(max(annual_power$annual_fishing_revenue_usd)), big.mark=","), " per grid cell\n", sep="")
  cat("Average revenue per km²: $", format(round(mean(annual_power$fishing_revenue_per_km2)), big.mark=","), "\n", sep="")
  
  return(annual_power)
}

# Function to calculate economic trade-offs
calculate_economic_tradeoffs <- function(wind_fish_data, fishing_loss_percent = 50) {
  
  cat("\n=== ECONOMIC TRADE-OFF ANALYSIS ===\n")
  cat("Assuming", fishing_loss_percent, "% of fishing revenue is lost due to turbine installation\n")
  
  # Calculate fishing opportunity cost (percentage of total fishing revenue lost)
  wind_fish_data$fishing_opportunity_cost <- wind_fish_data$annual_fishing_revenue_usd * (fishing_loss_percent / 100)
  
  # Calculate net economic value per grid cell
  # Wind revenue - fishing opportunity cost
  wind_fish_data$net_economic_value <- wind_fish_data$annual_revenue_usd - wind_fish_data$fishing_opportunity_cost
  
  # Calculate benefit-cost ratio
  wind_fish_data$benefit_cost_ratio <- wind_fish_data$annual_revenue_usd / wind_fish_data$fishing_opportunity_cost
  
  # Classify grid cells by economic attractiveness
  wind_fish_data$economic_category <- cut(
    wind_fish_data$net_economic_value,
    breaks = quantile(wind_fish_data$net_economic_value, c(0, 0.25, 0.75, 1)),
    labels = c("Low Value", "Moderate Value", "High Value"),
    include.lowest = TRUE
  )
  
  # Summary statistics
  cat("NET ECONOMIC VALUE SUMMARY:\n")
  cat("Range: $", format(round(min(wind_fish_data$net_economic_value)), big.mark=","), 
      " to $", format(round(max(wind_fish_data$net_economic_value)), big.mark=","), " per turbine\n", sep="")
  cat("Mean: $", format(round(mean(wind_fish_data$net_economic_value)), big.mark=","), " per turbine\n", sep="")
  
  cat("\nFISHING IMPACT SUMMARY:\n")
  cat("Total fishing revenue at risk: $", format(round(sum(wind_fish_data$annual_fishing_revenue_usd)), big.mark=","), "\n", sep="")
  cat("Total fishing opportunity cost (", fishing_loss_percent, "%): $", format(round(sum(wind_fish_data$fishing_opportunity_cost)), big.mark=","), "\n", sep="")
  
  cat("\nTURBINE SITES BY ECONOMIC CATEGORY:\n")
  print(table(wind_fish_data$economic_category))
  
  cat("\nBEST SITES (Top 10 by net economic value):\n")
  best_sites <- wind_fish_data %>%
    arrange(desc(net_economic_value)) %>%
    slice_head(n = 10) %>%
    select(site_id, latitude, longitude, annual_revenue_usd, fishing_opportunity_cost, 
           net_economic_value, capacity_factor)
  print(best_sites)
  
  cat("\nWORST SITES (Bottom 10 by net economic value):\n")
  worst_sites <- wind_fish_data %>%
    arrange(net_economic_value) %>%
    slice_head(n = 10) %>%
    select(site_id, latitude, longitude, annual_revenue_usd, fishing_opportunity_cost, 
           net_economic_value, capacity_factor)
  print(worst_sites)
  
  return(wind_fish_data)
}

# Function to create optimization visualizations
plot_economic_optimization <- function(wind_fish_data, lease_areas) {
  
  # Convert to spatial data
  optimization_sf <- st_as_sf(wind_fish_data, coords = c("longitude", "latitude"), crs = 4326)
  
  # 1. Net Economic Value Map
  net_value_map <- ggplot() +
    geom_sf(data = lease_areas, fill = "lightblue", alpha = 0.3, color = "blue") +
    geom_sf(data = optimization_sf, aes(color = net_economic_value), size = 1.2) +
    scale_color_gradient2(
      low = "red", mid = "yellow", high = "darkgreen", 
      midpoint = 0, name = "Net Economic\nValue (USD)"
    ) +
    labs(title = "Offshore Wind Economic Optimization",
         subtitle = "Net Economic Value (Wind Revenue - Fishing Opportunity Cost)") +
    theme_minimal()
  
  # 2. Fishing Intensity Map
  fishing_map <- ggplot() +
    geom_sf(data = lease_areas, fill = "lightblue", alpha = 0.3, color = "blue") +
    geom_sf(data = optimization_sf, aes(color = fishing_intensity), size = 1.2) +
    scale_color_viridis_c(name = "Fishing\nIntensity") +
    labs(title = "Commercial Fishing Intensity",
         subtitle = "Higher values indicate more fishing activity") +
    theme_minimal()
  
  # 3. Economic Category Map
  category_map <- ggplot() +
    geom_sf(data = lease_areas, fill = "lightblue", alpha = 0.3, color = "blue") +
    geom_sf(data = optimization_sf, aes(color = economic_category), size = 1.2) +
    scale_color_manual(
      values = c("Low Value" = "red", "Moderate Value" = "orange", "High Value" = "darkgreen"),
      name = "Economic\nCategory"
    ) +
    labs(title = "Turbine Site Economic Categories",
         subtitle = "Based on net economic value (wind revenue - fishing cost)") +
    theme_minimal()
  
  # 4. Benefit-Cost Ratio Map
  ratio_map <- ggplot() +
    geom_sf(data = lease_areas, fill = "lightblue", alpha = 0.3, color = "blue") +
    geom_sf(data = optimization_sf, aes(color = pmin(benefit_cost_ratio, 50)), size = 1.2) +
    scale_color_viridis_c(name = "Benefit-Cost\nRatio") +
    labs(title = "Wind Revenue to Fishing Cost Ratio",
         subtitle = "Higher ratios indicate better economic trade-offs") +
    theme_minimal()
  
  return(list(
    net_value = net_value_map,
    fishing_intensity = fishing_map, 
    economic_category = category_map,
    benefit_cost = ratio_map
  ))
}

cat("=== STEP 4 FUNCTIONS LOADED ===\n")
cat("Ready to create fisheries data and run optimization!\n\n")
cat("To run Step 4, execute:\n")
wind_fish_data <- create_fisheries_data(annual_power)
optimization_results <- calculate_economic_tradeoffs(wind_fish_data, fishing_loss_percent = 50)
maps <- plot_economic_optimization(optimization_results, lease_areas)
maps$net_value  # View net economic value map
cat("To test different fishing loss scenarios:\n")
cat("# 25% loss: optimization_25 <- calculate_economic_tradeoffs(wind_fish_data, 25)\n")
cat("# 75% loss: optimization_75 <- calculate_economic_tradeoffs(wind_fish_data, 75)\n")