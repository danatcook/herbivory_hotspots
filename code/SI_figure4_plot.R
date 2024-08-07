
# Packages -----
library(tidyverse)
library(readr)
library(ggplot2)
library(ggrepel)
library(ggpubr)

# Data -----
# Herbivore removal of adult Turbinaria density in 2021 experiment
browsing <- read_csv("data/HR_algae_density_summary.csv")
# Clean up site names
browsing$site <- sub("_", " ", browsing$site) # removes "_" in site names
browsing$site <- sub("_", " ", browsing$site) # have to run this twice to fix Ava site names
browsing <- browsing %>%
  rename_with(str_to_title) %>%  # capitalize first letters of column names
  filter(Treatment == "Unpalatable")

# Geographic locations for sites in 2021 experiment
distances <- read_csv("data/HR_2021_site_locations.csv")

# Data wrangling -----
# Merge dataframes
browsing_distance <- merge(browsing, distances, by = "Site")

# Log transform distance to deep water
browsing_distance <- browsing_distance %>% 
  # mutate(logDensity_turb_adult_percconsumed = log(Density_turb_adult_percconsumed)) %>% 
  mutate(logDistance_from_deep_water_m = log(Distance_from_deep_water_m))

# Plot -----
# Adult Turbinaria removal as a function of distance from deep water (log-transformed) with Pearson's R
ggscatter(browsing_distance, 
          x = "logDistance_from_deep_water_m",
          y = "Density_turb_adult_percchange", 
          size = 5
          #add = "reg.line" 
          #add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          #conf.int = TRUE
) +
  #stat_cor(method = "pearson", label.x = 2, label.y = -5, size = 5) +
  ylab("Percent change in adult Turbinaria density") +
  xlab("Distance from deep water (m) (log-transformed)") +
  theme_classic() +
  theme(text = element_text(size = 15)) +
  theme(axis.text = element_text(size = 15))
