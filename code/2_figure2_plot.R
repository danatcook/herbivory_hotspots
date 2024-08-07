# Script to produce Figure 2 

# Figure 2 shows spatial covariation between grazing and browsing rates across the twenty sites. Values are the same site averages shown in Fig. 1. 

# Packages -----
library(tidyverse)
library(readr)
library(ggplot2)
library(cowplot)
library(ggrepel)

# Data ------
siteDat <- read_csv("data/siteDat_2017_25.10.2021.csv")
siteDat$Grazing[16] <- 85

# For site 166, increase grazing rate from 83.3 to 85 so site 166 and site 152 aren't overlapping. Note: this data point is only adjusted for a "jittering" effect in this figure (analyses uses the actual value) so that the two sites don't overlap in the plot.



# Plot -----
ggplot(siteDat, 
       aes(x= Grazing,
           y = PercConsumed)) +
  geom_point(size = 9, alpha = 0.3, colour = "black") +
  #geom_jitter(size = 7, alpha = 0.3, width = 0.5, height = 0.5) +
  # geom_text_repel(label = siteDat$Site) + # Label sites
  xlab("Grazing Rate") +
  ylab("Browsing Rate") +
  scale_x_continuous(expand = c(0, 0), limits = c(0,105)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0,105)) +
  theme_classic() +
  theme(text = element_text(size = 23)) +
  theme(axis.text = element_text(size = 23)) +
  theme(axis.line = element_line(size = 1)) + # axis line
  theme(axis.ticks = element_line(size = (1))) # axis tick marks

