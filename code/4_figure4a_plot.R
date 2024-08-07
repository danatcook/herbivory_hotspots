# Script to produce figure 4a 

# Figure 4a shows percent change (mean ± SE) in adult Turbinaria density within macroalgal communities (N = 6 plots per site) along a gradient in ambient browsing level (based on short-term browsing assays). 

# Packages -----
library(tidyverse)
library(readr)

# Data ----
density_data_raw <- read_csv("data/HR_algae_density_raw.csv") %>% 
  filter(treatment == "Unpalatable")
# In this dataset, I added estimates of Adult Turbinaria Density before and after the experiment using before/after photos of the plots. I replaced the actual Initial Adult Turbinaria Density values (i.e., those that I actually counted during the experiment that were recorded on the not lost datasheet) with my estimates from the photos so that final and initial estimates were collected in the same way (not initial values from counting stipes in real life and final values from looking at photos).

# Functions -----
# summarySE function
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}




# Data wrangling -----
# Calculate raw and percent change in density of adult Turbinaria stipes
density_data <- density_data_raw %>% 
  pivot_wider(names_from = timepoint, values_from = density_turb_adult) %>% #make final density and initial density into separate columns (easier to calculate change in density)
  mutate(raw_change = 
           ifelse(Initial == 0 & Final == 0, NA, (Final - Initial))) %>% #calculate raw change in adult turb 
  mutate(perc_change = (raw_change/Initial * 100))  #calculate percent change in adult turb

# Calculate summary stats for percent change in density of Turbinaria
perc_change_data_summary <- summarySE(density_data, measurevar = "perc_change", groupvars = c("site", "treatment"), na.rm = TRUE)

# Plot -----
#  Percent change in adult Turbinaria density in Unpalatable treatment ----
ggplot() + 
  geom_point(data=perc_change_data_summary,
             aes(x=site, y=perc_change, na.rm=TRUE),
             shape=19,
             size=7) +
  geom_errorbar(data=perc_change_data_summary,
                aes(x=site, ymin=perc_change-se, ymax=perc_change+se), width=0, alpha=1, size=0.75) +
  ylab("Percent change in adult Turbinaria density") +
  xlab("Site") +
  scale_x_discrete(labels=c('', '', '', '', '')) +
  theme_classic() +
  geom_hline(yintercept=0) +
  theme(axis.text.y = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 22)) + 
  theme(axis.title.x = element_text(size = 20))

