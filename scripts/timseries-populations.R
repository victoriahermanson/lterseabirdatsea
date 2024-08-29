rm(list=ls())


library(ggplot2)
library(tidyverse)
seabird_100 <- read_csv(here::here("data/Cruise_Transect_Observations_merged.csv"),show_col_types = FALSE)
# combined 
seabird_95 <- read_csv(here::here("data/table_95.csv"),show_col_types = FALSE)


##-----------------------------------------------------------------------------
# convert 'years' column to factor and rename levels
seabird_100$Cruise <- factor(seabird_100$Cruise, levels = unique(seabird_100$Cruise))
levels(seabird_100$Cruise) <- c("1993","1994", "1995", "1996", "1997", "1998","1999", 
                                "2000", "2001", "2002", "2003","2004","2005","2006",
                                "2007","2008","2009","2010","2011","2012","2013",
                                "2014","2015","2016","2017","2018","2019","2021",
                                "2022","2023","2024")

print(head(seabird_100))


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

# define max value to filter out large values
# max_value <- 200  # Example threshold value

# filter the data to exclude values greater than max_value
filtered_data <- peng_df %>%
  filter(x <= max_value)

## isolating one species time series plot
# species_of_interest <- "CHPE"  # Specify the species you want to isolate
# 
# # Subset data for the species of interest
# species_data <- peng_df[peng_df$Group.2 == species_of_interest, ]

# combine all species data into one dataframe
species_data <- rbind(
  transform(peng_df, species = "CHPE"),
  transform(peng_df, species = "ADPE"),
  transform(peng_df, species = "GEPE")
)

# create time series plot with lines and points for multiple species
p <- ggplot(filtered_data, aes(x = Category, y = x, color = Group.2, group = Group.2)) +
  geom_line() +  # Add lines connecting the points for each species
  geom_point(size = 2) +  # Add points for each data point
  labs(title = "Species Sightings Over Time", x = "Year", y = "Count") +
  theme_minimal() +  # Customize plot theme
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),  # adjust title appearance
        axis.text = element_text(size = 8),  # Adjust axis text size
        axis.title = element_text(size = 12, face = "bold")) +  # adjust axis title appearance
  scale_color_manual(values = c("magenta", "cyan", "darkgreen"))  # custom colors for each species


# # Create time series for one species plot with line and points
## group = 1 created the line to connect points
# p <- ggplot(species_data, aes(x = Category, y = x, group = 1)) +
#   geom_line(color = "lightblue") +  # Add a line connecting the points
#   geom_point(color = "black", size = 1) +  # Add points for clarity
#   labs(title = paste("Population of", species_of_interest), x = "Year", y = "Count") +
#   theme_minimal() +  # Customize plot theme
#   theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),  # Adjust title appearance
#         axis.text = element_text(size = 12),  # Adjust axis text size
#         axis.title = element_text(size = 12, face = "bold"))  # Adjust axis title appearance

# Print the plot
print(p)

