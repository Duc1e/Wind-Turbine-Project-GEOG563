# Step 3: Oregon Offshore Wind Analysis - Power Generation Calculations
# Prerequisites: Run Step 1 (lease areas) and Step 2 (NREL data processing)

library(dplyr)
library(ggplot2)
library(sf)

# Generic offshore wind turbine power curve function
generic_power_curve <- function(windspeed, rated_power_mw = 10) {
  
  # Generic offshore turbine parameters
  cut_in_speed <- 3.0    # m/s - wind speed where turbine starts
  rated_speed <- 13.0    # m/s - wind speed where max power reached  
  cut_out_speed <- 25.0  # m/s - wind speed where turbine shuts down
  
  # Initialize power output
  power_mw <- rep(0, length(windspeed))
  
  # Power curve segments
  # 1. Below cut-in: no power
  power_mw[windspeed < cut_in_speed] <- 0
  
  # 2. Cut-in to rated: cubic relationship (typical for wind)
  active_range <- windspeed >= cut_in_speed & windspeed < rated_speed
  power_mw[active_range] <- rated_power_mw * 
    ((windspeed[active_range] - cut_in_speed) / (rated_speed - cut_in_speed))^3
  
  # 3. Rated to cut-out: constant max power
  power_mw[windspeed >= rated_speed & windspeed < cut_out_speed] <- rated_power_mw
  
  # 4. Above cut-out: no power (safety shutdown)
  power_mw[windspeed >= cut_out_speed] <- 0
  
  return(power_mw)
}

# Function to calculate annual power generation
calculate_annual_power <- function(wind_data_lease, rated_power_mw = 10) {
  
  cat("=== CALCULATING POWER GENERATION ===\n")
  cat("Applying generic", rated_power_mw, "MW turbine power curve...\n")
  
  # Apply power curve to wind data
  wind_data_lease$power_mw <- generic_power_curve(wind_data_lease$windspeed_120m, 
                                                  rated_power_mw = rated_power_mw)
  
  # Calculate annual metrics per turbine location
  annual_power <- wind_data_lease %>%
    group_by(site_id, latitude, longitude) %>%
    summarise(
      # Wind resource metrics
      mean_windspeed = mean(windspeed_120m, na.rm = TRUE),
      max_windspeed = max(windspeed_120m, na.rm = TRUE),
      min_windspeed = min(windspeed_120m, na.rm = TRUE),
      
      # Power generation metrics
      annual_generation_mwh = sum(power_mw, na.rm = TRUE),
      capacity_factor = annual_generation_mwh / (rated_power_mw * 8760), # Actual/theoretical
      
      # Operational metrics
      hours_operating = sum(power_mw > 0, na.rm = TRUE),
      hours_rated_power = sum(power_mw >= rated_power_mw * 0.95, na.rm = TRUE), # Near full power
      hours_above_cutout = sum(windspeed_120m >= 25, na.rm = TRUE),
      
      # Additional wind statistics
      hours_above_15ms = sum(windspeed_120m >= 15, na.rm = TRUE),
      hours_below_3ms = sum(windspeed_120m < 3, na.rm = TRUE),
      
      .groups = 'drop'
    )
  
  # Add economic metrics (assuming $50/MWh electricity price)
  annual_power$annual_revenue_usd <- annual_power$annual_generation_mwh * 50
  
  cat("Annual power calculations complete!\n")
  cat("Processed", nrow(annual_power), "potential turbine locations\n")
  
  return(annual_power)
}

# Function to summarize wind resource results
summarize_wind_resource <- function(annual_power) {
  
  cat("=== WIND RESOURCE SUMMARY ===\n")
  cat("Total potential turbine locations:", nrow(annual_power), "\n\n")
  
  cat("WIND SPEED STATISTICS:\n")
  cat("Mean wind speed range:", round(min(annual_power$mean_windspeed), 1), "to", 
      round(max(annual_power$mean_windspeed), 1), "m/s\n")
  cat("Overall mean wind speed:", round(mean(annual_power$mean_windspeed), 1), "m/s\n\n")
  
  cat("POWER GENERATION STATISTICS:\n")
  cat("Annual generation range:", round(min(annual_power$annual_generation_mwh)), "to", 
      round(max(annual_power$annual_generation_mwh)), "MWh per turbine\n")
  cat("Mean annual generation:", round(mean(annual_power$annual_generation_mwh)), "MWh per turbine\n")
  cat("Total potential generation:", round(sum(annual_power$annual_generation_mwh)/1000), "GWh from all sites\n\n")
  
  cat("CAPACITY FACTOR STATISTICS:\n")
  cat("Capacity factor range:", round(min(annual_power$capacity_factor), 3), "to", 
      round(max(annual_power$capacity_factor), 3), "\n")
  cat("Mean capacity factor:", round(mean(annual_power$capacity_factor), 3), "\n\n")
  
  cat("ECONOMIC POTENTIAL (at $50/MWh):\n")
  cat("Annual revenue range: $", format(round(min(annual_power$annual_revenue_usd)), big.mark=","), 
      " to $", format(round(max(annual_power$annual_revenue_usd)), big.mark=","), " per turbine\n", sep="")
  cat("Total annual revenue potential: $", format(round(sum(annual_power$annual_revenue_usd)/1e6), big.mark=","), 
      " million from all sites\n", sep="")
  
  # Quality assessment
  high_quality_sites <- sum(annual_power$capacity_factor >= 0.45)
  good_quality_sites <- sum(annual_power$capacity_factor >= 0.40 & annual_power$capacity_factor < 0.45)
  
  cat("\nWIND RESOURCE QUALITY:\n")
  cat("High quality sites (CF >= 45%):", high_quality_sites, "sites\n")
  cat("Good quality sites (CF 40-45%):", good_quality_sites, "sites\n")
  cat("Lower quality sites (CF < 40%):", nrow(annual_power) - high_quality_sites - good_quality_sites, "sites\n")
}

# Function to create power generation visualizations
plot_wind_resource <- function(annual_power, lease_areas) {
  
  # Convert to spatial data for mapping
  power_sf <- st_as_sf(annual_power, coords = c("longitude", "latitude"), crs = 4326)
  
  # 1. Capacity factor map
  cf_map <- ggplot() +
    geom_sf(data = lease_areas, fill = "lightblue", alpha = 0.3, color = "blue") +
    geom_sf(data = power_sf, aes(color = capacity_factor), size = 1) +
    scale_color_viridis_c(name = "Capacity\nFactor") +
    labs(title = "Offshore Wind Capacity Factor",
         subtitle = paste("Based on", nrow(annual_power), "potential turbine locations")) +
    theme_minimal()
  
  # 2. Annual generation map  
  gen_map <- ggplot() +
    geom_sf(data = lease_areas, fill = "lightblue", alpha = 0.3, color = "blue") +
    geom_sf(data = power_sf, aes(color = annual_generation_mwh), size = 1) +
    scale_color_viridis_c(name = "Annual\nGeneration\n(MWh)") +
    labs(title = "Annual Power Generation Potential",
         subtitle = "10 MW Generic Offshore Turbine") +
    theme_minimal()
  
  # 3. Wind speed map
  wind_map <- ggplot() +
    geom_sf(data = lease_areas, fill = "lightblue", alpha = 0.3, color = "blue") +
    geom_sf(data = power_sf, aes(color = mean_windspeed), size = 1) +
    scale_color_viridis_c(name = "Mean Wind\nSpeed (m/s)") +
    labs(title = "Mean Wind Speed at 120m Height",
         subtitle = "2019 Annual Average") +
    theme_minimal()
  
  return(list(capacity_factor = cf_map, generation = gen_map, wind_speed = wind_map))
}

cat("=== STEP 3 FUNCTIONS LOADED ===\n")
cat("Ready to calculate power generation!\n\n")
cat("To run Step 3, execute:\n")
cat("1. annual_power <- calculate_annual_power(wind_data_lease)\n")
cat("2. summarize_wind_resource(annual_power)\n") 
cat("3. maps <- plot_wind_resource(annual_power, lease_areas)\n")
cat("4. maps$capacity_factor  # View capacity factor map\n")