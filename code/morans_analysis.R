# Script to run Moran's I test for spatial autocorrelation on grazing and browsing rates at the 20 sites. 

# Packages -----
#install.packages("spdep")
#install.packages("sf")

library(spdep)
library(sf)
library(tidyverse)
# library(ggplot2)
library(readr)

# Data -----
herb_data <- read_csv("data/siteDat_2017_25.10.2021.csv")

graz_data <- herb_data %>% 
  select(Grazing, Lat, Lon)

brow_data <- herb_data %>% 
  select(PercConsumed, Lat, Lon)

# Moran's test for browsing rate -----
# Convert to spatial object with sf
herb_data_sf <- st_as_sf(herb_data, coords = c("Lat", "Lon"), crs = 4326)

# Extract geographic coordinates
coords <- st_coordinates(herb_data_sf)

# Set a reasonable distance threshold (in meters) for neighbors
dist_threshold <- 1  # 1 km

# Define neighbors based on this distance threshold
neighbors <- dnearneigh(coords, 0, dist_threshold)

# Create spatial weights list with row-standardized weights
weights <- nb2listw(neighbors, style = "W")

# Perform Moran's I test
brow_moran_test <- moran.test(brow_data$PercConsumed, weights)
print(brow_moran_test)


# Moran's test for grazing rate -----
# Perform Moran's I test
graz_moran_test <- moran.test(graz_data$Grazing, weights)
print(graz_moran_test)

