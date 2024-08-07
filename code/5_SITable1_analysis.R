# Script to run linear regressions that predict biomass of grazing and browsing herbivores

# The predictor variables used in the grazing and browsing regressions  are the environmental variables associated with hot and cold spots in grazing and browsing, respectively, and the same used in the principal component analyses. 

# Packages -----
library(tidyverse)
library(readr)
library(leaps) # model selection tool for regression analysis (performs exhaustive search to determine best predictors)
library(AICcmodavg) # run AIC test
library(stargazer) # to make regression summary statistics table

# Data -----
allData <- read_csv("data/siteDat_2017_25.10.2021.csv")
# site-averaged abiotic and biotic data for 20 sites
# fish and benthic data are non-transformed and log-transformed [ln(x+1)]

# Data wrangling -----
# Calculate total grazer biomass (sum 'Excavator', 'Scraper' and 'Grazer')
allData <- allData %>% 
  dplyr::mutate(ExcScrGra = Excavator + Scraper + Grazer)

# Create df of response and predictor variables for grazer model
grazers.df <- allData %>% 
  dplyr::select(Turf_normal, Turf_sediment, Stegastes_turf, AFDW_g_3wk, Min_distance_from_crest_m, Min_distance_from_deep_m, ExcScrGra)

# Create df of response and predictor variables for browser model
logbrowsers.df <- allData %>% 
  dplyr::select(Fleshy_macroalgae, MacAccumulation_g_8wk, Min_distance_from_crest_m, Min_distance_from_deep_m, LogBrowser)


# Run browser regression models -----
## Browser models -----
# n = 20 sites (small sample size), so I should use 2 or less independent variables for each regression.

# Determine best model that predicts browser biomass
# Run regressions using all combinations of predictors (with 2 predictors max)
LogBr.subset <- regsubsets(LogBrowser ~.,
                           data = logbrowsers.df,
                           nbest = 1,
                           nvmax = 2, # limit on number of predictors
                           method = "exhaustive")
summary.LogBr.subset <- summary(LogBr.subset) # save results
as.data.frame(summary.LogBr.subset$outmat) # convert results to df for viewing
which.max(summary.LogBr.subset$adjr2) # recommends 2 predictors to get the best model
summary.LogBr.subset$which[2,] # recommends these 2 variables as best predictors for model

# Mac productivity and distance from crest are two best predictor variables of browser biomass.

# Run browser models using all possible combinations of predictors
# Mac productivity and distance from crest
LogBr.model1 <- lm(LogBrowser ~ MacAccumulation_g_8wk + Min_distance_from_crest_m, data = logbrowsers.df)
summary(LogBr.model1) # r2 = 0.36, p = 0.009

# Mac productivity and distance from deep water 
LogBr.model2 <- lm(LogBrowser ~ MacAccumulation_g_8wk + Min_distance_from_deep_m, data = logbrowsers.df)
summary(LogBr.model2) # r2 = 0.31, p = 0.02

# Mac cover and distance from deep water
LogBr.model3 <- lm(LogBrowser ~ Fleshy_macroalgae + Min_distance_from_deep_m, data = logbrowsers.df)
summary(LogBr.model3) # r2 = 0.14, p = 0.1

# Distance from crest and distance from deep water
LogBr.model4 <- lm(LogBrowser ~ Min_distance_from_crest_m + Min_distance_from_deep_m, data = logbrowsers.df)
summary(LogBr.model4) # r2 = 0.13, p = 0.1


# Run AIC test on possible models
LogBr.models <- list(LogBr.model1, LogBr.model2, LogBr.model3, LogBr.model4) # combine models into a list
LogBr.mod.names <- c('sed.crest', 'prod.crest', 'mac.crest', 'prod.deep') # give models each a name
aictab(cand.set = LogBr.models, modnames = LogBr.mod.names) # spits out AIC stats, lowest AIC score is best model


## Grazer models -----
# Run regressions using all combinations of predictors (2 predictors max per regression)

# Determine best model that predicts grazer biomass
ExcScrGra.subset <- regsubsets(ExcScrGra ~.,
                               data = grazers.df, 
                               nbest = 1,
                               nvmax = 2, 
                               method = "exhaustive")
summary.ExcScrGra.subset <- summary(ExcScrGra.subset) 
as.data.frame(summary.ExcScrGra.subset$outmat) 
which.max(summary.ExcScrGra.subset$adjr2) 
summary.ExcScrGra.subset$which[2,] 

# Sedimented turf cover and turf productivity are two best predictor variables of grazer biomass.

# Run grazer model
# Sedimented turf cover and turf productivity
Gr.model1 <- lm(ExcScrGra ~ Turf_sediment + AFDW_g_3wk, data = grazers.df)
summary(Gr.model1) # r2 = 0.24, p = 0.04


## Regression summary statistics table -----
# Output statistics summary table for browser model
stargazer(LogBr.model1, no.space = TRUE, 
          title = "Supplemental Table 1", out = "output/browser-regression-table.html")

# Output statistics summary table for grazer model
stargazer(Gr.model1, no.space = TRUE, 
          title = "Supplemental Table 2", out = "output/grazer-regression-table.html")

