rm(list=ls())


library(ggplot2)
library(tidyverse)
seabird_100 <- read_csv(here::here("data/table_100.csv"))

##-----------------------------------------------------------------------------
# make a table of all unique species seen in the survey and their breeding season info
# are all the birds we see breeding during the LTER cruise
# are they raising chicks/incubating eggs etc. or not breeding at all
##-----------------------------------------------------------------------------

# manually specify bird species to include
# would it be better to exclude certain spp by identifying a pattern?
bird_species <- c("SPSK", "WISP", "ADPE", "ANTE","GIPE","FUSE","BBAL","LMSA",
                      "GHAL","CAPE","SOFU","GEPE","ANPR","BLPE","BBSP","CHPE","KEGU",
                      "LESE","BESH","UNSK","ROAL","WCPE","UNPE","DIPE","UNSE","SNPE",
                      "CRSE","ANPE","WESE","BRSK","ARTE","ROSE","ELSE","UNAL","EMPE",
                      "TBPR","FAPR","MAPE","WAAL","UNPR","BNSW","UNSP","SOAL","UNTE",
                      "TPPR","AMSH","SGPE","SPSK","GIPR")

# filter the dataset to include only the bird species
bird_data <- seabird_100 %>%
  filter(Species %in% bird_species)

# get unique bird species seen overall
unique_bird_species <- unique(bird_data$Species)

# creat table of unique bird species seen overall
unique_species_table <- data.frame(Species = unique_bird_species)
print(unique_species_table)

# write to csv
# write.csv(unique_species_table, "birds_breedinginfo.csv", row.names = FALSE)


##-----------------------------------------------------------------------------
# plot year (x axis) vs. total # seen that year. A separate plot for each species
# You can do this in ggplot or run a for loop. 
# Plots dont need to be pretty, I just want to look at basic trends in time. 
# This should also help with your research. 
##-----------------------------------------------------------------------------

# Aggregate data to get total sightings per cruise number (representing years) for each species
sightings_per_cruise <- bird_data %>%
  group_by(Species, Cruise) %>%
  summarise(total_sightings = n())

# Convert CruiseNumber to factor with specific order
cruise_order <- c("9301", "9401", "9501", "9601", "9701", "9801", "9901", "0001", 
                  "0101", "0201", "0301", "0401", "0501", "0601", "0701", "0801", 
                  "0901", "1001", "1101", "1201", "1301", "1401", "1501", "1601", 
                  "1701", "1801")

sightings_per_cruise$Cruise <- factor(sightings_per_cruise$Cruise, levels = cruise_order)

# Function to plot all species in facets
plot_all_species <- function() {
  p <- ggplot(sightings_per_cruise, aes(x = CruiseNumber, y = total_sightings)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    labs(title = "Total Sightings by Species",
         x = "Cruise Number (Year)",
         y = "Total Sightings") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    facet_wrap(~ Species, scales = "free_y", ncol = 5)
  
  print(p)
}

# Display all plots in one window
plot_all_species()





# # function to plot each species separately
# plot_species <- function(species) {
#   combined_data <- sightings_per_cruise %>%
#     filter(Species == species)
#   
#   p <- ggplot(combined_data, aes(x = CruiseNumber, y = total_sightings)) +
#     geom_bar(stat = "identity", fill = "skyblue") +
#     labs(title = paste("Sightings of", species),
#          x = "Cruise Number (Year)",
#          y = "Total Sightings") +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1))
#   
#   print(p)
# }
# 
# # create separate plots for each species
# for (species in bird_species) {
#   plot_species(species)
# }












