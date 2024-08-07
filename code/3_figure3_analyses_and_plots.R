# Script to run the principal component analyses (PCAs) and produce the plots for Figure 3. 

# The PCAs explore the relationships between spatial variation in grazing and browsing with environmental attributes at hot and cold spots (i.e., sites with high or low herbivory rates). 

## Packages -----
library(tidyverse)
library(ggplot2)
library(readr)
library(ggbiplot) # visualize principal components using ggplot
library(corrplot)

## Data -----
allData <- read_csv("data/siteDat_2017_25.10.2021.csv")
# site-averaged abiotic and biotic data for 20 sites
# fish and benthic data are non-transformed and log-transformed [ln(x+1)]

## Colors -----
hotCol <- 'darkgoldenrod1'
coldCol <- 'blue4'

## Data wrangling ----- 
# Subset and wrangle data into format for PCA analysis by filtering sites for hotspots and coldspots in grazing and browsing
# High G, high B sites:  145, 150, 186, 176
# Low G, low B sites: 163, 126, 175
# High G, low B sites: 134, 139, 166, 152, 136
hrData <- allData[c(8,11,20,18,15,1,17,4,6,16,12,5),]

# Create new column to label sites with herbivory levels
Regime.vec <- c("High Grazing, High Browsing", "High Grazing, High Browsing", "High Grazing, High Browsing", "High Grazing, High Browsing", "Low Grazing, Low Browsing", "Low Grazing, Low Browsing", "Low Grazing, Low Browsing", "High Grazing, Low Browsing", "High Grazing, Low Browsing", "High Grazing, Low Browsing", "High Grazing, Low Browsing", "High Grazing, Low Browsing")
# Add herbivory regime column to df
hrData$Regime <- Regime.vec

## Prepare dataframe for PCA analyses by filtering for the following columns: grazing rates, browsing rates, and explanatory variables
hrData <- hrData %>% 
  dplyr::select(Site, Regime, Lat, Lon, Habitat, Lagoon, Grazing, Sargassum_consumption, Percent_N_June2017, Turf_normal, Turf_sediment, Stegastes_turf, Fleshy_macroalgae, AFDW_g_3wk, MacAccumulation_g_8wk, Coral, Detritivore, Browser, Excavator, Scraper, Grazer, Min_distance_from_crest_m, Min_distance_from_deep_m, Min_distance_from_pass_m) %>% 
  mutate(TurfEaters = Excavator + Scraper + Grazer) %>%  # Create composite TurfEaters variable (total biomass of Excavators, Scrapers, and Grazers)
  mutate(ScrExc = Scraper + Excavator)



## Principal components analyses -----
# Grazing PCA
# Filter for sites (hotspots and coldspots in grazing)
grData_exp <- hrData %>% 
  filter(Regime == "High Grazing, Low Browsing" | Regime == "Low Grazing, Low Browsing")

# Label sites
grData_labels <- grData_exp %>% 
  select(Site, Regime)

# Create dataframe of explanatory variables
grData_exp <- grData_exp %>% 
  dplyr::select(Turf_normal, Turf_sediment, Stegastes_turf, Fleshy_macroalgae, AFDW_g_3wk, MacAccumulation_g_8wk, Grazer, Detritivore, Browser, Scraper, Excavator, Min_distance_from_crest_m, Min_distance_from_deep_m, Min_distance_from_pass_m)

# Run grazing PCA
gr.pca <- prcomp(grData_exp %>% 
                   select(Turf_normal, Turf_sediment, Stegastes_turf, AFDW_g_3wk, Min_distance_from_crest_m, Min_distance_from_deep_m), center = TRUE, scale. = TRUE)

# Visualize grazing PCA
ggbiplot(gr.pca, choices = c(1,2), labels = grData_labels$Site, groups = grData_labels$Regime, ellipse = TRUE) +
  scale_color_manual(name="Site", values=c(hotCol, coldCol)) +
  theme_bw()
summary(gr.pca)
gr.pca

# Make grazing PCA plot pretty (remove labels)
q <- ggbiplot(gr.pca,  
              choices = c(1,2), # change which PCs are plotted
              groups = grData_labels$Regime,
              ellipse = TRUE,
              varname.abbrev = TRUE,
              varname.size = 0) +
  scale_color_manual(name="Site", values=c(hotCol, coldCol)) +
  geom_point(aes(color = grData_labels$Regime), size = 4) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  theme(axis.ticks = element_line(size = (0.75))) + # axis tick marks
  theme(axis.text = element_text(size = 12)) +
  expand_limits(y = c(-2.25, 2.25)) +
  expand_limits(x = c(-2.25, 2.25))

seg <- which(sapply(q$layers, function(x) class(x$geom)[1] == 'GeomSegment'))
txt <- which(sapply(q$layers, function(x) class(x$geom)[1] == 'GeomText'))

q$layers[[seg]]$aes_params$colour <- 'black'
q$layers[[seg]]$aes_params$size <- 0.5

q

# ggsave("output/ordination_herbivory-regime-subset/grazing-pca_blank.png", width = 6, height = 6, dpi = 300, type = "cairo")


# Browsing PCA
# Filter for sites (hotspots and coldspots in browsing)
brData_exp <- hrData %>% 
  filter(Regime == "High Grazing, High Browsing" | Regime == "High Grazing, Low Browsing")

# Label sites
brData_labels <- brData_exp %>% 
  select(Site, Regime)

# Create dataframe of explanatory variables
brData_exp <- brData_exp %>% 
  dplyr::select(Percent_N_June2017, Turf_normal, Turf_sediment, Stegastes_turf, Fleshy_macroalgae, AFDW_g_3wk, MacAccumulation_g_8wk, Excavator, Grazer, Scraper, Detritivore, Browser, TurfEaters, Min_distance_from_crest_m, Min_distance_from_deep_m, Min_distance_from_pass_m)

# Run browsing PCA
br.pca <- prcomp(brData_exp %>% 
                   select(MacAccumulation_g_8wk, Fleshy_macroalgae, Min_distance_from_crest_m, Min_distance_from_deep_m), center = TRUE, scale. = TRUE)

# Visualize browsing PCA
ggbiplot(br.pca, choices = c(1,2), labels = brData_labels$Site, groups = brData_labels$Regime, ellipse = TRUE) +
  scale_color_manual(name="Site", values=c(hotCol, coldCol)) +
  theme_bw()
summary(br.pca)
br.pca

# Make browsing PCA plot pretty (remove labels)
p <- ggbiplot(br.pca,  
              choices = c(1,2), # change which PCs are plotted
              groups = brData_labels$Regime,
              #varname.size = 4,
              ellipse = TRUE,
              varname.abbrev = TRUE,
              varname.size = 0.1) +
  scale_color_manual(name="Site", values=c(hotCol, coldCol)) +
  geom_point(aes(color = brData_labels$Regime), size = 4) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none") + 
  theme(axis.ticks = element_line(size = (0.75))) + # axis tick marks
  theme(axis.text = element_text(size = 12)) +
  expand_limits(y = c(-2.5, 2.5)) +
  expand_limits(x = c(-2.5, 2.5))

p

seg <- which(sapply(p$layers, function(x) class(x$geom)[1] == 'GeomSegment'))
txt <- which(sapply(p$layers, function(x) class(x$geom)[1] == 'GeomText'))

p$layers[[seg]]$aes_params$colour <- 'black'
p$layers[[seg]]$aes_params$size <- 0.5

# p$layers[[txt]] <- geom_label(aes(x = xvar, y = yvar, label = varname,
#                                   angle = angle, hjust = hjust),
#                               label.size = NA,
#                               data = p$layers[[txt]]$data,
#                               fill = 'white')
p

# ggsave("output/ordination_herbivory-regime-subset/browsing-pca_blank.png", width = 6, height = 6, dpi = 500)
