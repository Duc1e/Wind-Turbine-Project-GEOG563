# Step 2D: Debug NREL API Issues

# Let's test the API with a simple request first
test_nrel_api <- function() {
  cat("Testing NREL API with basic request...\n")
  
  # Try a simple test point in known coverage area
  test_lat <- 43.0
  test_lon <- -125.0
  
  # Try the basic wind toolkit URL first
  url <- "https://developer.nrel.gov/api/wind-toolkit/v2/wind/wtk-download.csv"
  
  params <- list(
    api_key = NREL_API_KEY,
    lat = test_lat,
    lon = test_lon,
    names = "2019",
    attributes = "windspeed_120m",
    utc = "true"
  )
  
  cat("Testing URL:", url, "\n")
  cat("Parameters:", paste(names(params), params, sep="=", collapse=", "), "\n\n")
  
  response <- GET(url, query = params, timeout(30))
  
  cat("Response status:", status_code(response), "\n")
  cat("Response headers:\n")
  print(headers(response))
  
  if (status_code(response) != 200) {
    cat("Error content:\n")
    cat(content(response, "text"), "\n")
  }
  
  return(response)
}

# Test the API
cat("Step 2D: Testing NREL API...\n")
test_response <- test_nrel_api()