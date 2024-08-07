# Script to produce supplementary figure 1

# SI Figure 1 is made up of boxplots showing rates of grazing and b) browsing at the twenty sites grouped by habitat (N = 10 sites per habitat).

# Packages -----
library(tidyverse)
library(readr)
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(rlang)
library(ggbeeswarm)

# Colors for habitats -----
MiCol <- 'cadetblue4'
FrCol <- 'cadetblue3'

# Data -----
allData <- read_csv("data/siteDat_2017_25.10.2021.csv")

# Data wrangling -----
# Subset herbivory rates and biotic/abiotic explanatory variables at 20 sites
data <- allData %>% 
  dplyr::select(Site, Lat, Lon, Habitat, Lagoon, Grazing, PercConsumed, Percent_N_June2017, Turf_normal, Turf_sediment, Stegastes_turf, Fleshy_macroalgae, AFDW_g_3wk, MacAccumulation_g_8wk, Detritivore, Browser, Excavator, Scraper, Grazer, Min_distance_from_crest_m, Min_distance_from_deep_m, Min_distance_from_pass_m) %>% 
  mutate(TurfEaters = Excavator + Scraper + Grazer) # Create composite TurfEaters variable (total biomass of Excavators, Scrapers, and Grazers)

## Boxplot function ----- 
make_boxplot <- function(x, y, Fill, title, xlabel, ylabel) {
  data %>% 
    ggplot(aes({{x}}, {{y}}, fill={{Fill}})) +
    scale_fill_manual(values = c(FrCol, MiCol)) +
    geom_boxplot() +
    geom_jitter(color="black", size=2, alpha=0.5) +
    theme_classic() +
    theme(text = element_text(size = 18)) +
    theme(axis.text.x = element_text(color='black', size=18),
          axis.text.y = element_text(color='black', size=18)) +
    #theme_ipsum() +
    ggtitle({{title}}) +
    xlab({{xlabel}})  +
    ylab({{ylabel}})
}

# Make function to plot boxplots with limits 0 to 100 (use for variables measured in percent, such as percent cover and herbivory rates)
make_boxplot_percent <- function(x, y, Fill, title, xlabel, ylabel) {
  data %>% 
    ggplot(aes({{x}}, {{y}}, fill={{Fill}})) +
    scale_fill_manual(values = c(FrCol, MiCol)) +
    geom_boxplot() +
    geom_jitter(color="black", size=2, alpha=0.5) +
    theme_classic() +
    theme(text = element_text(size = 18)) +
    theme(axis.text.x = element_text(color='black', size=18),
          axis.text.y = element_text(color='black', size=18)) +
    ylim(c(0,100)) +
    #theme_ipsum() +
    ggtitle({{title}}) +
    xlab({{xlabel}})  +
    ylab({{ylabel}})
}

## Make boxplots
# Browsing rate
make_boxplot_percent(Habitat, PercConsumed, Habitat, "Browsing Rate", "Habitat", "Browsing rate")

ggsave("browsing_boxplot.png", # name of the plot to save, must end with '.png' or other file type
       width = 8,
       height = 6,
       units = "in",
       dpi = 500, # high resolution
       path = "output")


# Grazing rate
make_boxplot_percent(Habitat, Grazing, Habitat, "Grazing Rate", "Habitat", "Grazing Rate")

ggsave("grazing_boxplot.png", # name of the plot to save, must end with '.png' or other file type
       width = 8,
       height = 6,
       units = "in",
       dpi = 500, # high resolution
       path = "output")

# Normal turf cover
make_boxplot_percent(Habitat, Turf_normal, Habitat, "Turf Cover", "Habitat", "Percent cover")

ggsave("turf-cover_boxplot.png", # name of the plot to save, must end with '.png' or other file type
       width = 8,
       height = 6,
       units = "in",
       dpi = 500, # high resolution
       path = "output")

# Sedimented turf cover
make_boxplot_percent(Habitat, Turf_sediment, Habitat, "Sedimented Turf Cover", "Habitat", "Percent cover")

ggsave("sedimented-turf-cover_boxplot.png", # name of the plot to save, must end with '.png' or other file type
       width = 8,
       height = 6,
       units = "in",
       dpi = 500, # high resolution
       path = "output")

# Stegastes turf cover
make_boxplot_percent(Habitat, Stegastes_turf, Habitat, "Stegastes Turf Cover", "Habitat", "Percent cover")

ggsave("steg-turf-cover_boxplot.png", # name of the plot to save, must end with '.png' or other file type
       width = 8,
       height = 6,
       units = "in",
       dpi = 500, # high resolution
       path = "output")

