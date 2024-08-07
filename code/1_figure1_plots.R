# Script to produce plots for Figure 1

# Part 1 of script produces maps showing percent consumption (mean ± SE) of turf assays after 3 hr (N = 10 replicates per site) (i.e., grazing rate) and macroalgae assays after 24 hr (N = 15 replicates per site) (i.e., browsing rate) across the twenty sites in the Moorea north shore lagoons.

# Part 2 of script produces barplots that show the same rates of grazing and browsing at the twenty sites sorted from west to east.


# PART 1: MAPS -----
## Packages -----
library(lubridate)
library(ggplot2)
library(viridis)
library(dplyr)
library(data.table)
library(ggrepel)
library(tidyverse)

## Data -----
# site-averaged abiotic and biotic data for 20 sites
# fish and benthic data are non-transformed and log-transformed [ln(x+1)]
herbData <- read_csv("data/siteDat_2017_25.10.2021.csv") %>% 
  dplyr::rename(Browsing = PercConsumed)

## Map code ----
# Install latest update of ggmap
# if(!requireNamespace("devtools")) install.packages("devtools")
# devtools::install_github("dkahle/ggmap", ref = "tidyup", force=TRUE)

library(ggmap)

#Set your API Key
ggmap::register_google(key = "AIzaSyCABhyIj1NScNa7YDwLnwjaI5PsxHBlGRU")

# Define Moorea
moorea <- c(lon = -149.837385, lat = -17.509545) # centered to show entire state

# Define map source, type, and color
# Map = moorea map
moorea_map <- get_map(
  location = c(lon = -149.839185, lat = -17.491710),
  source = "google", 
  maptype = "satellite", 
  crop=FALSE, 
  zoom = 13)
ggmap(moorea_map)

# Make grazing map
grazing_map <-  ggmap(moorea_map) +
  # geom_text_repel(
  #   aes(Lon, Lat, label = Site),
  #   data=herbData,
  #   box.padding = 0.25,
  #   max.overlaps = Inf,
  #   min.segment.length = Inf,
  #   segment.color = 'black',
  #   colour = 'white',
  #   fontface = "bold") +
  geom_point(aes(x = Lon, y = Lat, fill = Grazing), data = herbData, shape = 21, colour = 'black', size = 6) +
  scale_fill_viridis(option = "plasma") +
  # scale_fill_gradient2(low = "yellow", mid = "orange", high = "red", midpoint = 50, limits = c(0,100)) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position="right",
        legend.text = element_text(face = "italic"),
        legend.title = element_blank())
grazing_map
#scale_colour_continuous(name="Grazing") # add legend title

# ggsave("output/herbivory_maps/grazing-map_plasma_noSiteLabels.png", width = 8, height = 8, dpi = 300)

# Make browsing map
browsing_map <-  ggmap(moorea_map) +
  # geom_text_repel(
  #   aes(Lon, Lat, label = Site),
  #   data=herbData,
  #   box.padding = 0.25,
  #   max.overlaps = Inf,
  #   min.segment.length = Inf,
  #   segment.color = 'black',
  #   colour = 'white',
  #   fontface = "bold") +
  geom_point(aes(x = Lon, y = Lat, fill = Browsing), data = herbData, shape = 21, colour = 'black', size = 6) +
  scale_fill_viridis(option = "plasma") +
  # scale_fill_gradient2(low = "yellow", mid = "orange", high = "red", midpoint = 50, limits = c(0,100)) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position="right",
        legend.text = element_text(face = "italic"),
        legend.title = element_blank())
browsing_map

# ggsave("output/herbivory_maps/browsing-map_plasma_noSiteLabels.png", width = 8, height = 8, dpi = 300)


# PART 2: BARPLOTS -----

## Packages -----
library(tidyverse)
library(ggplot2)
library(readr)
library(ggbiplot)

## Colors -----
MiCol <- 'cadetblue4'
FrCol <- 'cadetblue3'

## Data -----
herbivoryData <- read_csv("data/siteDat_2017_25.10.2021.csv") %>% 
  select(Site, Lon, Lat, Habitat, PercConsumed, Grazing) %>% 
  dplyr::rename(Browsing = PercConsumed)
# site-averaged abiotic and biotic data for 20 sites
# fish and benthic data are non-transformed and log-transformed [ln(x+1)]

# New site labels (renumbered to be 1-20 from west to east)
new_sites <- read_csv("data/new_site_numbers.csv")

# Add new site labels to herbivoryData
herbivoryData <- merge(herbivoryData, new_sites, by = "Site")

# Grazing site summary stats (average of 2 surveys + SD, SE, CI)
grazing_sum <- read_csv("data/grazing_site-summary-stats.csv") %>% 
  dplyr::rename(Grazing_SD = SD) %>% 
  dplyr::rename(Grazing_SE = SE) %>% 
  dplyr::rename(Grazing_CI = CI) %>% 
  select(!N)

# Browsing site summary stats (average of 3 surveys + SD, SE, CI)
browsing_sum <- read_csv("data/browsing_site-summary-stats.csv") %>% 
  dplyr::rename(Browsing_SD = SD) %>% 
  dplyr::rename(Browsing_SE = SE) %>% 
  dplyr::rename(Browsing_CI = CI) %>% 
  select(!N)

# Add new site labels to grazing and browsing summary data
grazing_sum <- merge(grazing_sum, new_sites, by = "Site")
browsing_sum <- merge(browsing_sum, new_sites, by = "Site")

## Data wrangling -----

herbivoryData$New_site <- as.factor(herbivoryData$New_site)
grazing_sum$New_site <- as.factor(grazing_sum$New_site)
browsing_sum$New_site <- as.factor(browsing_sum$New_site)

# Add summary stats for grazing and browsing to main dataframe
herbivoryData <- merge(herbivoryData, grazing_sum, by = c("New_site", "Site", "Habitat", "Grazing"))
herbivoryData <- merge(herbivoryData, browsing_sum, by = c("New_site", "Site", "Habitat"))

herbivoryData <- herbivoryData %>% 
  select(!Browsing.y) %>% 
  dplyr::rename(Browsing = Browsing.x)

herbivoryData$Habitat <- as.factor(herbivoryData$Habitat)


## Plots -----

## Grazing rate west to east
ggplot() +
  geom_bar(data = herbivoryData, aes(x = reorder(New_site, Lon), y = Grazing, fill = Habitat),
           stat = "identity") +
  scale_fill_manual(values = c(FrCol, MiCol)) +
  geom_errorbar(data = herbivoryData,
                aes(x = New_site,
                    ymin=Grazing-Grazing_SE, ymax=Grazing+Grazing_SE), width=.2) +
  theme_classic() +
  theme(text = element_text(size = 18)) +
  theme(axis.text.x = element_text(color='black', 
                                   size=15, angle=45, vjust = 0.6),
        axis.text.y = element_text(color='black', size=18)) +
  scale_y_continuous(limits = c(0,101), expand = c(0, 0)) +
  xlab("Site") +
  ylab("Percent consumed of turf")

# ggsave("grazing-east-west_new-site-labels.tiff", # name of the plot to save, must end with '.png' or other file type
#        width = 10,
#        height = 5,
#        units = "in",
#        dpi = 300, # high resolution
#        path = "output/herbivory")


## Browsing rate low to high
ggplot() +
  geom_bar(data = herbivoryData, aes(x = reorder(New_site, Lon), y = Browsing, fill = Habitat),
           stat = "identity") +
  scale_fill_manual(values = c(FrCol, MiCol)) +
  geom_errorbar(data = herbivoryData,
                aes(x = New_site,
                    ymin=Browsing-Browsing_SE, ymax=Browsing+Browsing_SE), width=.2) +
  theme_classic() +
  theme(text = element_text(size = 18)) +
  theme(axis.text.x = element_text(color='black', 
                                   size=15, angle=45, vjust = 0.6),
        axis.text.y = element_text(color='black', size=18)) +
  scale_y_continuous(limits = c(0,101), expand = c(0, 0)) +
  xlab("Site") +
  ylab("Percent consumed of macroalgae")


# ggsave("browsing-east-west_new-site-labels.tiff", # name of the plot to save, must end with '.png' or other file type
#        width = 10,
#        height = 5,
#        units = "in",
#        dpi = 300, # high resolution
#        path = "output/herbivory")

