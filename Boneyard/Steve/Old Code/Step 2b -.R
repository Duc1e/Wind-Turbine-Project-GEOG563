# First, let's check the data loaded correctly and set up for power calculations
cat("Loading wind data for power calculations...\n")

# If you want to reload from CSV (optional, since you have it in memory):
# all_wind_data <- read.csv("processed_wind_data.csv")

# Apply generic power curve to calculate hourly power generation
cat("Calculating power generation using generic 10MW turbine...\n")

# Add power generation column
all_wind_data$power_mw <- generic_power_curve(all_wind_data$windspeed_120m, rated_power_mw = 10)

# Aggregate to annual values per grid cell (site_id)
annual_power <- all_wind_data %>%
  group_by(site_id, latitude, longitude) %>%
  summarise(
    mean_windspeed = mean(windspeed_120m, na.rm = TRUE),
    annual_generation_mwh = sum(power_mw, na.rm = TRUE),
    capacity_factor = annual_generation_mwh / (10 * 8760), # CF = actual/theoretical
    hours_above_cutout = sum(windspeed_120m >= 25, na.rm = TRUE),
    .groups = 'drop'
  )

cat("Annual power summary complete!\n")
head(annual_power)