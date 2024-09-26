# Script to generate supplemental table 2 output table of the biomass and abundance of herbivores from the 2017 fish count surveys of the 20 NS sites. Values are the total biomass (g/m2) and total abundance (# of individuals) of each species or functional group of herbivores across all 20 sites. 
###################################

# Packages -----
library(tidyverse)
library(janitor) # for clean_names()
library(readr) 
library(knitr) # to make HTML table
library(kableExtra) # to make HTML table
library(webshot) # to save HTML table as PNG or PDF
library(htmltools) # to save HTML table as PNG or PDF

# Data -----
# number and size of species observed every transect minute
fishDat <- read_csv("data/fish_2017_original.csv") %>% 
  select(!c(Date, Surveyor, Survey_code, GPS, GoPro, Notes)) %>% 
  filter(Taxonomy != "Carcharhinus melanopterus") %>% 
  filter(Taxonomy != "Acanthurus triostegus")

# weight to length conversions for each species
conversionDat <- read_csv("data/fish_weight_length.csv") %>% 
  select(!c(Totlen2forklen, Cite_length_conversion, Note_length_conversion, Length_code, Cite_bm_conversion, Taxon_resolution))

# area surveyed per transect
areaDat <- read_csv("data/area_sampled_all.csv") %>% 
  select(!c(Date, Survey))

# fish functional groups
fish_groups <- read_csv("data/fish_attributes_v2.csv") 


# Data wrangling: summarize fish count data -----
# Create dataframe that sums total biomass and total abundance of herbivorous fish (by species and functional group) for all 20 sites.

# Merge fish data with fish functional groups 
fishDat <- left_join(fishDat, fish_groups) %>% 
  select(!c(Corallivore, Skeleton_remover))

# Merge fish data with conversion info and calculate biomass of each species per observation (g per observation)
fishDat <- left_join(fishDat, conversionDat) %>% 
  mutate(Biomass = (a * Size^b)*Number)


# Calculate total biomass of each species observed at each site (total g per site summed across transects)
fishDat_total <- fishDat %>%  
  group_by(Taxonomy, Site, Herbivore_functional_group) %>% 
  summarise(
    Total_Biomass = sum(Biomass, na.rm = TRUE),
    Total_Abundance = sum(Number, na.rm = TRUE)
  ) 

# Calculate total area surveyed at each site (total m2)
areaTotal <- areaDat %>% 
  group_by(Site) %>% 
  summarise(
    Total_Area = sum(Area, na.rm = TRUE)
  )

# Merge fish and area data (now both at site level)
fishDat_total <- left_join(fishDat_total, areaTotal)

# Divide total biomass per site by total area surveyed per site to get total biomass per area surveyed at each site (total g/m2)
fishDat_total <- fishDat_total %>% 
  mutate(Total_Biomass_g_m2 = Total_Biomass/Total_Area)

# Summarize total biomass and abundance by species and herbivore functional group
herbivoreDat_summarized <- fishDat_total %>% 
  filter(Herbivore_functional_group == "Grazer" | Herbivore_functional_group == "Excavator" | Herbivore_functional_group == "Scraper" | Herbivore_functional_group == "Browser") %>% 
  group_by(Taxonomy, Herbivore_functional_group) %>% 
  summarise(
    Biomass_g_m2 = sum(Total_Biomass_g_m2, na.rm = TRUE),
    Abundance = sum(Total_Abundance, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(Total_Biomass_g_m2 = sum(Biomass_g_m2)) %>% 
  mutate(Total_Abundance = sum(Abundance)) %>%
  mutate(Prop_Biomass_g_m2 = Biomass_g_m2/Total_Biomass_g_m2) %>% 
  mutate(Prop_Abundance = Abundance/Total_Abundance) %>% 
  select(!c(Total_Biomass_g_m2, Total_Abundance))

# Round to 4 decimal places
herbivoreDat_summarized_rounded <- herbivoreDat_summarized %>%
  mutate(
    `Total Biomass` = round(`Total Biomass`, 3),
    `Proportional Biomass` = round(`Proportional Biomass`, 4),
    `Proportional Abundance` = round(`Proportional Abundance`, 4)
  )

# Output table -----
# Create nicely formatted PDF table of total biomass and total abundance of herbivores across 20 NS sites

# Change column names
colnames(herbivoreDat_summarized_rounded) <- c("Species", "Herbivore Group", "Total Biomass", "Total Abundance", "Proportional Biomass", "Proportional Abundance")

# Create a formatted table in HTML format
# Create a formatted table in HTML format
herbivore_table <- kable(herbivoreDat_summarized_rounded, format = "html", table.attr = 'class="table table-bordered"') %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    fixed_thead = TRUE
  ) %>%
  column_spec(1, width = "auto") %>%
  column_spec(2, width = "auto") %>%
  column_spec(3, width = "auto") %>% 
  column_spec(4, width = "auto") %>%
  column_spec(5, width = "auto") %>%
  column_spec(6, width = "auto")


# Display the table in the RStudio Viewer or HTML format
print(herbivore_table)

# Save the table as an HTML file
html_file <- "output/herbivore_table.html"
save_kable(herbivore_table, file = html_file, path = "output")

# Save the HTML table as a high-resolution PNG
webshot(html_file, file = "output/herbivore_table.png", vwidth = 1600, vheight = 1200, cliprect = "viewport")

# Save the HTML table as PDF using pagedown
pagedown::chrome_print(html_file, output = "output/herbivore_table.pdf")

# Create a formatted table
kable_output <- kable(herbivoreDat_summarized, format = "latex", booktabs = TRUE, col.names = colnames(herbivoreDat_summarized)) %>%
  kable_styling(latex_options = c("striped", "hold_position"))

# Save the table as a PDF
pdf("herbivore_table.pdf", width = 8.5, height = 11) # Specify PDF dimensions as needed
cat(kable_output)
dev.off()

# Inform the user
cat("PDF table saved as 'herbivore_table.pdf'")

