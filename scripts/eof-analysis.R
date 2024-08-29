rm(list=ls())


library(ggplot2)
library(tidyverse)
ßseabird_100 <- read_csv(here::here("data/Cruise_Transect_Observations_merged.csv"),show_col_types = FALSE)
# seabird_100 <- read.csv("data/Cruise_Transect_Observations_merged.csv", header = TRUE)


# Convert 'years' column to factor and rename levels
levels(seabird_100$Cruise) <- c("1993","1994", "1995", "1996", "1997", "1998","1999", 
                                "2000", "2001", "2002", "2003","2004","2005","2006",
                                "2007","2008","2009","2010","2011","2012","2013",
                                "2014","2015","2016","2017","2018","2019","2021",
                                "2022","2023","2024")

# Print the updated dataframe to verify changes
print(head(seabird_100))

##-----------------------------------------------------------------------------
# make a table of all unique species seen in the survey and their breeding season info
# are all the birds we see breeding during the LTER cruise
# are they raising chicks/incubating eggs etc. or not breeding at all
##-----------------------------------------------------------------------------

# manually specify bird species to include
# would it be better to exclude certain spp by identifying a pattern?
bird_species <- c("SPSK", "WISP", "ADPE", "ANTE","GIPE","BBAL","LMSA","GHAL",
                  "CAPE","SOFU","GEPE","ANPR","BLPE","BBSP","CHPE","KEGU","BESH",
                  "UNSK","ROAL","WCPE","UNPE","DIPE","UNSE","SNPE","ANPE",
                  "BRSK","ARTE","UNAL","EMPE","TBPR","FAPR","MAPE","WAAL",
                  "UNPR","BNSW","UNSP","SOAL","UNTE","TPPR","AMSH","SGPE",
                  "SPSK","GIPR")

# bird_species <- c("SPSK", "WISP", "ADPE", "ANTE","GIPE","BBAL","LMSA","GHAL",
#                   "CAPE","SOFU","GEPE","ANPR","BLPE","BBSP","CHPE","KEGU","BESH",
#                   "UNSK","ROAL","WCPE","UNPE","DIPE","UNSE","SNPE","ANPE",
#                   "BRSK","ARTE","UNAL","EMPE","TBPR","FAPR","MAPE","WAAL",
#                   "UNPR","BNSW","UNSP","SOAL","UNTE","TPPR","AMSH","SGPE",
#                   "SPSK","GIPR")

# filter the dataset to include only the bird species
bird_data <- seabird_100 %>%
  filter(Species %in% bird_species)

# get unique bird species seen overall
unique_bird_species <- unique(bird_data$Species)

# create table of unique bird species seen overall
unique_species_table <- data.frame(Species = unique_bird_species)
print(unique_species_table)

# write to csv
# write.csv(unique_species_table, "birds_breedinginfo.csv", row.names = FALSE)


years <- c(1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,
           2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2021,2022,2023,2024)
spp <- seabird_100$Species
number <- as.numeric(seabird_100$Number)

year <- filter(seabird_100, Cruise %in% years)
pengs <- c("GEPE", "CHPE", "ADPE")

test <- seabird_100 %>%
  filter(Species %in% pengs,
         Cruise %in% years)

test$Number <- as.numeric(test$Number)
test$Number
peng_df <- aggregate(test$Number, by=list(Category=test$Cruise, test$Species), FUN=sum)


## isolating one species time series plot
# species_of_interest <- "CHPE"  # Specify the species you want to isolate
# 
# # Subset data for the species of interest
# species_data <- peng_df[peng_df$Group.2 == species_of_interest, ]

# Combine all species data into one dataframe
species_data <- rbind(
  transform(peng_df, species = "CHPE"),
  transform(peng_df, species = "ADPE"),
  transform(peng_df, species = "GEPE")
)

##-----------------------------------------------------------------------------
# plot year (x axis) vs. total # seen that year. A separate plot for each species
# You can do this in ggplot or run a for loop. 
# Plots dont need to be pretty, I just want to look at basic trends in time. 
# This should also help with your research. 
##-----------------------------------------------------------------------------

# aggregate data to get total sightings per cruise number (representing years) for each species
sightings_per_cruise <- bird_data %>%
  group_by(Species, Cruise) %>%
  summarise(total_sightings = n())

# convert CruiseNumber to factor with specific order
cruise_order <- c("9301", "9401", "9501", "9601", "9701", "9801", "9901", "0001", 
                  "0101", "0201", "0301", "0401", "0501", "0601", "0701", "0801", 
                  "0901", "1001", "1101", "1201", "1301", "1401", "1501", "1601", 
                  "1701", "1801")

sightings_per_cruise$Cruise <- factor(sightings_per_cruise$Cruise, levels = cruise_order)

# function to plot all species in facets
plot_all_species <- function() {
  p <- ggplot(sightings_per_cruise, aes(x = Cruise, y = total_sightings)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    labs(title = "Total Sightings by Species",
         x = "Cruise Number (Year)",
         y = "Total Sightings") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    facet_wrap(~ Species, scales = "free_y", ncol = 5)
  
  print(p)
}

# display all plots in one window
plot_all_species()

# save file
# ggsave("total_sightings_plot.png", plot = p, width = 12, height = 8, units = "in", dpi = 300)

## subset data to only include the spp counts per cruise year
subset_seabird_data <- sightings_per_cruise[sightings_per_cruise$Cruise %in% cruise_order, ]


#-------------------------------------------------------------------------------
# start EOF analysis
#-------------------------------------------------------------------------------

# compute anomalies
bird_data_anomalies <- subset_seabird_data %>%
  group_by(Species) %>%
  mutate(sightings_anomaly = total_sightings - mean(total_sightings))

# Sample dataframe
bird_data <- data.frame(
  species = bird_data_anomalies$Species,
  year = bird_data_anomalies$Cruise,
  count = bird_data_anomalies$total_sightings,
  anomalies = bird_data_anomalies$sightings_anomaly
)

# Compute the covariance matrix (S matrix)
S <- cov(numeric_data)
# Compute the correlation matrix (R matrix)
R <- cor(numeric_data)
# Compute eigenvalues and eigenvectors of S matrix
eig_S <- eigen(S)

# Extract eigenvalues and eigenvectors
eigenvalues <- eig_S$values
eigenvectors <- eig_S$vectors












