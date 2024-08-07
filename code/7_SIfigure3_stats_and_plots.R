# Script to produce Supplemental Figure 3

# Supplemental Figure 3 shows the relationships between a) grazing rate and grazer biomass and b) browsing rate and browser biomass at the twenty sites.

# Packages -----
library(tidyverse)
library(readr)
library(cowplot)
library(ggpubr) # ggscatter() to make correlation plots with correlation coefficient and significance


# Data -----
data.all <- read_csv("data/siteDat_2017_25.10.2021.csv")
data.herbivory <- data.all %>% 
  dplyr::select(Site, Grazing, Sargassum_consumption, PercConsumed, Turf_normal, Turf_sediment, Stegastes_turf, Fleshy_macroalgae, Mac_palatable, Mac_unpalatable, AFDW_g_3wk, MacAccumulation_g_8wk, Percent_N_June2017, Scraper, Grazer, Excavator, Detritivore, Browser, LogBrowser, Min_distance_from_crest_m, Min_distance_from_deep_m, Habitat) %>% 
  mutate(ExcScrGra = Scraper + Grazer + Excavator) 
  


## Function for correlation analysis and plot -----
# Note: x and y variables must be in quotations, e.g., function("Turf_sediment", "Grazing")
make_corr_plot <- function(x, y, xlabel, ylabel) {
  ggscatter(data.herbivory, 
            x = {{x}}, 
            y = {{y}}, 
            color = "black", shape = 21, size = 3, # Points color, shape and size
            add = "reg.line",  # Add regressin line
            add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
            conf.int = TRUE, # Add confidence interval
            cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
            cor.coeff.args = list(method = "pearson", label.x.npc = "left", label.y.npc = "bottom", label.sep = "\n"),
            cor.coef.size = 5) +
    xlab({{xlabel}}) +
    ylab({{ylabel}}) +
    theme(text = element_text(size = 18)) +
    theme(axis.text = element_text(size = 18)) +
    theme(axis.line = element_line(size = 0.75)) + # axis line
    theme(axis.ticks = element_line(size = (0.75))) + # axis tick marks
    theme(aspect.ratio = 1)
}

# Plots with statistics ----- 
# Pearson's correlation between browser biomass and browsing rate
make_corr_plot("LogBrowser", "PercConsumed", "Browser Biomass (Log-transformed)", "Browsing Rate")

# Pearson's correlation between grazer biomass and grazing rate
make_corr_plot("ExcScrGra", "Grazing", "Grazer Biomass", "Grazing Rate")

