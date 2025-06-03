# Step 2A: Oregon Offshore Wind Analysis - Wind Data Functions

# NREL API Configuration
NREL_API_KEY <- "W3rtr6GukFNkoygjMm6EMYaVK3TpPFqbdqaDIWF9"

# Download wind data for a single point from NREL
download_single_point_wind <- function(lat, lon, year = "2019", 
                                       attributes = "windspeed_80m,windspeed_100m,windspeed_120m") {
  
  url <- "https://developer.nrel.gov/api/wind-toolkit/v2/wind/offshore-nw-pacific-download.csv"
  
  params <- list(
    api_key = NREL_API_KEY,
    lat = lat,
    lon = lon,
    names = year,
    attributes = attributes,
    utc = "true",
    leap_day = "true",
    interval = "60"
  )
  
  tryCatch({
    response <- GET(url, query = params, timeout(120))
    
    if (status_code(response) == 200) {
      csv_content <- content(response, "text", encoding = "UTF-8")
      wind_data <- read_csv(csv_content, show_col_types = FALSE)
      return(wind_data)
    } else {
      cat("API Error - Status:", status_code(response), "\n")
      return(NULL)
    }
    
  }, error = function(e) {
    cat("Network error:", e$message, "\n")
    return(NULL)
  })
}

# Calculate capacity factor from hourly wind speeds
calculate_capacity_factor_from_hourly <- function(hourly_wind_speeds) {
  ws <- hourly_wind_speeds[!is.na(hourly_wind_speeds)]
  
  if (length(ws) == 0) return(NA)
  
  # Turbine power curve parameters
  cut_in <- 3.0      # Cut-in wind speed
  rated_speed <- 12.5 # Rated wind speed
  cut_out <- 25.0    # Cut-out wind speed
  rated_power <- 1.0 # Normalized rated power
  
  power_output <- sapply(ws, function(v) {
    if (v < cut_in || v >= cut_out) {
      return(0)
    } else if (v <= rated_speed) {
      return(rated_power * ((v^3 - cut_in^3) / (rated_speed^3 - cut_in^3)))
    } else {
      return(rated_power)
    }
  })
  
  capacity_factor <- mean(power_output, na.rm = TRUE)
  return(capacity_factor)
}

cat("Step 2 Complete: Capacity factor calculation function loaded\n")
