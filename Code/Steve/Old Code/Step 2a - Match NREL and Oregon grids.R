# Hybrid approach: Filter NREL grid to lease areas only
cat("=== HYBRID APPROACH: NREL GRID FILTERED TO LEASE AREAS ===\n")

# Create NREL points as spatial data
nrel_sf <- st_as_sf(nrel_unique_locations, 
                    coords = c("longitude", "latitude"), 
                    crs = 4326)

# Filter NREL points to only those within lease areas
nrel_in_lease <- st_filter(nrel_sf, lease_areas)

cat("NREL points in lease areas:", nrow(nrel_in_lease), "out of", nrow(nrel_sf), "total\n")

# Get the site_ids for filtering the wind data
lease_site_ids <- nrel_in_lease$site_id

# Filter wind data to lease areas only
wind_data_lease <- all_wind_data %>%
  filter(site_id %in% lease_site_ids)

cat("Wind data rows in lease areas:", nrow(wind_data_lease), "out of", nrow(all_wind_data), "total\n")
cat("Unique sites in lease areas:", length(unique(wind_data_lease$site_id)), "\n")

# Check coverage
cat("\nLease area coverage:\n")
cat("Lat range:", round(min(wind_data_lease$latitude), 3), "to", round(max(wind_data_lease$latitude), 3), "\n")
cat("Lon range:", round(min(wind_data_lease$longitude), 3), "to", round(max(wind_data_lease$longitude), 3), "\n")

# Visual check
ggplot() +
  geom_sf(data = lease_areas, fill = "lightblue", alpha = 0.3, color = "blue") +
  geom_sf(data = nrel_in_lease, color = "red", size = 0.8) +
  coord_sf(xlim = c(-125.5, -124.3), ylim = c(42, 44.3)) +
  labs(title = "NREL Grid Points Within Oregon Lease Areas",
       subtitle = paste(nrow(nrel_in_lease), "potential turbine locations"))