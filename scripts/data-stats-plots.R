rm(list=ls())

library(readr)
library(lubridate)
library(knitr)
library(kableExtra)
library(ggplot2)
library(geosphere)
library(dplyr)
library(stringr)
library(DT)
library(tidyr)


## ---------------------------------------------------------------------------
# read in all files
## ---------------------------------------------------------------------------
# underway_data  <- read_csv(here::here("data/Cruise_Transect_Header_merged_withfixes.csv"),show_col_types = FALSE)
# read csv
underway_obvs <- read_csv(here::here("data/underway_obvs.csv"))

# station data
station_data <- read.csv("data/LTERStns.csv", header = TRUE)

# -----------------------------------------------------------------------------
# spp facet plots
# -----------------------------------------------------------------------------
library(RColorBrewer)

# Define species of interest
species_of_interest <- c("SPSK","WISP","ADPE","ANTE","GIPE","BBAL","LMSA","GHAL","CAPE","SOFU","GEPE","ANPR","BLPE","BBSP","CHPE","KEGU","BESH","UNSK","ROAL","WCPE","UNPE","DIPE","SNPE","ANPE","BRSK","ARTE","UNSP","UNAL","EMPE","TBPR","FAPR","MAPE","WAAL","WSIP","UNPR","BNSW","SOAL","UNTE","TPPR","AMSH","SGPE","SKSP","GIPR","UNPT","BBPE")

# Filter to include only species of interest
underway_obvs <- underway_obvs[underway_obvs$Species %in% species_of_interest, ]

# Remove rows with missing or invalid longitude, latitude, or cruise values
underway_obvs <- underway_obvs[!is.na(underway_obvs$`Longitude Start`) & 
                                 !is.na(underway_obvs$`Latitude Start`) & 
                                 !is.na(underway_obvs$`Longitude End`) & 
                                 !is.na(underway_obvs$`Latitude End`) &
                                 !is.na(underway_obvs$Cruise) &
                                 underway_obvs$`Longitude Start` >= -180 & 
                                 underway_obvs$`Longitude Start` <= 180 & 
                                 underway_obvs$`Latitude Start` >= -90 & 
                                 underway_obvs$`Latitude Start` <= 90 & 
                                 underway_obvs$`Longitude End` >= -180 & 
                                 underway_obvs$`Longitude End` <= 180 & 
                                 underway_obvs$`Latitude End` >= -90 & 
                                 underway_obvs$`Latitude End` <= 90, ]

# Calculate the min and max latitude and longitude across all data
min_longitude <- min(c(underway_obvs$`Longitude Start`, underway_obvs$`Longitude End`), na.rm = TRUE)
max_longitude <- max(c(underway_obvs$`Longitude Start`, underway_obvs$`Longitude End`), na.rm = TRUE)
min_latitude <- min(c(underway_obvs$`Latitude Start`, underway_obvs$`Latitude End`), na.rm = TRUE)
max_latitude <- max(c(underway_obvs$`Latitude Start`, underway_obvs$`Latitude End`), na.rm = TRUE)

# Determine the number of unique species
num_species <- length(unique(underway_obvs$Species))

# Generate a color palette based on the number of unique species
color_palette <- brewer.pal(n = min(num_species, 12), name = "Set3")

# If you need more colors than available in the palette, use `colorRampPalette` to create a custom palette
if (num_species > 12) {
  color_palette <- colorRampPalette(brewer.pal(12, "Set3"))(num_species)
}


base_layer <- data.frame(Latitude = station_data$Latitude, 
                         Longitude = station_data$Longitude)

# Loop through each species and create the facet grid by cruise with fixed lat/long scales and sequential track lines
for (species in species_of_interest) {
  
  # Filter data for the current species
  species_data <- underway_obvs %>%
    filter(Species == species) %>%
    arrange(Cruise, `Longitude Start`, `Latitude Start`)  # Ensure the data is sorted by cruise and lat/long
  
  # Check if the species_data is not empty and has valid cruises
  if (nrow(species_data) > 0 && !all(is.na(species_data$Cruise))) {
    
    # Create track lines between consecutive points
    species_data <- species_data %>%
      mutate(
        next_Latitude_Start = lead(`Latitude Start`),
        next_Longitude_Start = lead(`Longitude Start`)
      )  # Shift the coordinates to the next row for connecting tracks
    
    # Create the faceted plot by cruise for the current species with fixed latitude and longitude scales
    p <- ggplot(species_data) +
      
      # Plot the points based on start positions
      geom_point(aes(x = `Longitude Start`, y = `Latitude Start`, size = Number), alpha = 0.7, color = color_palette[which(species_of_interest == species)]) +
      
      # Plot sequential track lines connecting the start of one observation to the start of the next
      geom_segment(aes(x = `Longitude Start`, y = `Latitude Start`, 
                       xend = next_Longitude_Start, yend = next_Latitude_Start), 
                   color = "blue", alpha = 0.5, size = 1, na.rm = TRUE) +  # Adjust the line color and transparency
      
      labs(title = paste("Species Sightings with Sequential Track Lines -", species), 
           x = "Longitude", y = "Latitude", size = "Number of Individuals") +
      
      theme_minimal() +  # Use a minimal theme for the plot
      theme(
        plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),  # Adjust title appearance
        axis.text = element_text(size = 10),  # Adjust axis text size
        axis.title = element_text(size = 12, face = "bold"),  # Adjust axis title appearance
        strip.text = element_text(size = 12),  # Adjust facet label size
        strip.background = element_rect(fill = "lightgray")  # Facet label background
      ) +
      
      # Facet by cruise with fixed scales
      facet_wrap(~ Cruise, scales = "fixed") +
      
      # Apply fixed latitude and longitude limits
      xlim(min_longitude, max_longitude) +
      ylim(min_latitude, max_latitude)
    
    # Print the plot for the current species
    print(p)
  } else {
    message(paste("No valid data for species:", species))
  }
}











# Check if the species_data is not empty and has valid cruises
if (nrow(species_data) > 0 && !all(is.na(species_data$Cruise))) {
  
  # Create track lines between consecutive points
  species_data <- species_data %>%
    mutate(
      next_Latitude_Start = lead(`Latitude Start`),
      next_Longitude_Start = lead(`Longitude Start`)
    )  # Shift the coordinates to the next row for connecting tracks
  
  # Create the faceted plot by cruise for the current species with fixed latitude and longitude scales
  p <- ggplot() +
    # Plot the base layer of lat/long as background points
    geom_point(data = base_layer, aes(x = Longitude, y = Latitude), color = "gray", alpha = 0.5) +
    
    # Plot the points based on start positions for the species data
    geom_point(data = species_data, aes(x = `Longitude Start`, y = `Latitude Start`, size = Number), alpha = 0.7, color = color_palette[which(species_of_interest == species)]) +
    
    # Plot sequential track lines connecting the start of one observation to the start of the next
    geom_segment(data = species_data, aes(x = `Longitude Start`, y = `Latitude Start`, 
                                          xend = next_Longitude_Start, yend = next_Latitude_Start), 
                 color = "blue", alpha = 0.5, size = 1, na.rm = TRUE) +  # Adjust the line color and transparency
    
    labs(title = paste("Species Sightings with Sequential Track Lines -", species), 
         x = "Longitude", y = "Latitude", size = "Number of Individuals") +
    
    theme_minimal() +  # Use a minimal theme for the plot
    theme(
      plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),  # Adjust title appearance
      axis.text = element_text(size = 10),  # Adjust axis text size
      axis.title = element_text(size = 12, face = "bold"),  # Adjust axis title appearance
      strip.text = element_text(size = 12),  # Adjust facet label size
      strip.background = element_rect(fill = "lightgray")  # Facet label background
    ) +
    
    # Facet by cruise with fixed scales
    facet_wrap(~ Cruise, scales = "fixed") +
    
    # Apply fixed latitude and longitude limits
    xlim(min_longitude, max_longitude) +
    ylim(min_latitude, max_latitude)
  
  # Print the plot for the current species
  print(p)
} else {
  message(paste("No valid data for species:", species))
}




## ---------------------------------------------------------------------------
# histograms of Count Minute
## ---------------------------------------------------------------------------

# histogram with outliers
underway_obvs$`Count Minute` <- as.numeric(underway_obvs$`Count Minute`)

# check for non-numeric values or NA values
summary(underway_obvs$`Count Minute`)

# plot histogram treating `Count Minute` as a continuous variable
countmin_hist_woutliers <- ggplot(underway_obvs, aes(x = `Count Minute`)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Survey Minutes", x = "Hour of Day", y = "Frequency") +
  theme_minimal()

# ggsave(filename = "countmin_hist_woutliers.png", plot = countmin_hist_woutliers,units = "in",dpi = 300)


# calculate Q1, Q3, and IQR (difference btwn Q3 and Q1)
Q1 <- quantile(underway_obvs$`Count Minute`, 0.25, na.rm = TRUE)
Q3 <- quantile(underway_obvs$`Count Minute`, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

# define the lower and upper bounds for outliers
# lower_bound <- Q1 - 1.5 * IQR
# upper_bound <- Q3 + 1.5 * IQR
lower_bound <- Q1 - 100 
upper_bound <- Q3 + 100

# filter out the outliers
underway_obvs_filtered <- underway_obvs %>%
  filter(`Count Minute` >= lower_bound & `Count Minute` <= upper_bound)

## histogram of hours
underway_obvs$`Count Minute` <- (underway_obvs$`Count Minute`)
countmin_hist_nooutliers <- hist(underway_obvs$`Count Minute`, breaks = 24, col = "lightblue", 
                                 main = "Histogram of Survey Minutes", xlab = "Count Minute of Survey", ylab = "Frequency")
print(countmin_hist_nooutliers)

#ggsave(filename = "countmin_hist_nooutliers.png", plot = countmin_hist_nooutliers,units = "in",dpi = 300)
# mean((underway_obvs$`Count Minute`), na.rm = TRUE)

## ---------------------------------------------------------------------------
# histograms of Date/Time
## ---------------------------------------------------------------------------
## separating the Date/Time column contents into separate columns
# extract the Julian day, hour, and minute from the `YearDay.Hour.Minute column
underway_obvs <- underway_obvs %>%
  mutate(
    `YearDay/Hour/Minute` = str_pad(as.character(`YearDay/Hour/Minute`), width = 7, pad = "0"),  # Ensure 7 characters
    JulianDay = substr(`YearDay/Hour/Minute`, 1, 3),  # Extract Julian Day (first 3 characters)
    Hour = substr(`YearDay/Hour/Minute`, 4, 5),       # Extract Hour (next 2 characters)
    Minute = substr(`YearDay/Hour/Minute`, 6, 7)      # Extract Minute (last 2 characters)
  )

underway_obvs$JulianDay <- as.numeric(underway_obvs$JulianDay)

# plot histogram for JulianDay
ggplot(underway_obvs, aes(x = JulianDay)) + 
  geom_histogram(binwidth = 1, fill = "lightcoral", color = "black") +
  labs(title = "Histogram of Julian Days", x = "Julian Day", y = "Frequency") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, 365, by = 30),  # set breaks every 30 days
                     labels = function(x) paste0("Day ", x))  
## ---------------------------------------------------------------------------
# evaluating hours column
## ---------------------------------------------------------------------------
# histogram with outliers
underway_obvs$Hour <- as.numeric(underway_obvs$Hour)

# check for non-numeric values or NA values
summary(underway_obvs$Hour)

# plot histogram treating Hour as a continuous variable
ggplot(underway_obvs, aes(x = Hour)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Hours", x = "Hour of Day", y = "Frequency") +
  theme_minimal()


# calculate Q1, Q3, and IQR (difference btwn Q3 and Q1)
Q1 <- quantile(underway_obvs$Hour, 0.25, na.rm = TRUE)
Q3 <- quantile(underway_obvs$Hour, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

# define the lower and upper bounds for outliers
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# filter out the outliers
underway_obvs_filtered <- underway_obvs %>%
  filter(Minute >= lower_bound & Minute <= upper_bound)

## histogram of hours
underway_obvs$Hour <- (underway_obvs$Hour)
hist(underway_obvs$Hour, breaks = 24, col = "lightblue", 
     main = "Histogram of Hours", xlab = "Hour of Day", ylab = "Frequency")
mean((underway_obvs$Hour), na.rm = TRUE)

## ---------------------------------------------------------------------------
# evaluating minutes column
## ---------------------------------------------------------------------------
# check for non-numeric values or NA values
summary(underway_obvs$Minute)
underway_obvs$Minute <- as.numeric(underway_obvs$Minute)

# plot histogram treating Minute as a continuous variable
ggplot(underway_obvs, aes(x = Minute)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Minutes", x = "Minute of Day", y = "Frequency") +
  theme_minimal()

## histogram without outliers
# calculate Q1, Q3, and IQR (difference btwn Q3 and Q1)
Q1 <- quantile(underway_obvs$Minute, 0.25, na.rm = TRUE)
Q3 <- quantile(underway_obvs$Minute, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

# define the lower and upper bounds for outliers
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# filter out the outliers
underway_obvs_filtered <- underway_obvs %>%
  filter(Minute >= lower_bound & Minute <= upper_bound)

# plot histogram after removing outliers
ggplot(underway_obvs_filtered, aes(x = Minute)) + 
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black") +
  labs(title = "Histogram of Minutes (Outliers Removed)", x = "Minute", y = "Frequency") +
  theme_minimal()

## ---------------------------------------------------------------------------
# evaluating ship speed column
## ---------------------------------------------------------------------------
# check for non-numeric values or NA values
summary(underway_obvs$`Ship Speed`)
underway_obvs$`Ship Speed` <- as.numeric(underway_obvs$`Ship Speed`)

# plot histogram treating `Ship Speed` as a continuous variable
ggplot(underway_obvs, aes(x = `Ship Speed`)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Minutes", x = "Ship Speed", y = "Frequency") +
  theme_minimal()

## histogram without outliers
# calculate Q1, Q3, and IQR (difference btwn Q3 and Q1)
Q1 <- quantile(underway_obvs$`Ship Speed`, 0.25, na.rm = TRUE)
Q3 <- quantile(underway_obvs$`Ship Speed`, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

# # define the lower and upper bounds for outliers
# lower_bound <- Q1 - 1.5 * IQR
# upper_bound <- Q3 + 1.5 * IQR

# define the lower and upper bounds for outliers
lower_bound <- 0
upper_bound <- 80

# filter out the outliers
underway_obvs_filtered <- underway_obvs %>%
  filter(`Ship Speed` >= lower_bound & `Ship Speed` <= upper_bound)

# plot histogram after removing outliers
ship_speed_hist <- ggplot(underway_obvs_filtered, aes(x = `Ship Speed`)) + 
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black") +
  labs(title = "Histogram of Minutes (Outliers Removed)", x = "Ship Speed", y = "Frequency") +
  theme_minimal()+  
  theme(
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    legend.background = element_rect(fill = "white", color = NA),  
    strip.background = element_rect(fill = "white", color = "black"))

# ggsave("ship_speed_hist.png", plot = ship_speed_hist, dpi = 300)

## ---------------------------------------------------------------------------
# combined histogram: ship speed, minute count, distances
## ---------------------------------------------------------------------------
# reshape the data into long format
long_data <- underway_obvs %>%
  select(Cruise, Distance, `Ship Speed`, `Count Minute`) %>%
  pivot_longer(cols = c(Distance, `Ship Speed`, `Count Minute`), 
               names_to = "Variable", 
               values_to = "Value")

# plot the overlayed histograms with faceting by year
combined_hist_plot <- ggplot(long_data, aes(x = Value, fill = Variable, color = Variable)) +
  geom_histogram(position = "identity", alpha = 0.4, bins = 30) +
  facet_wrap(~Cruise, scales = "free") +  # Change `Cruise` to `Year` if necessary
  labs(title = "Overlayed Histograms of Distance, Ship Speed, and Count Minute by Year",
       x = "Value", y = "Frequency") +
  scale_fill_manual(values = c("lightblue", "lightgreen", "lightcoral")) + 
  scale_color_manual(values = c("lightblue", "lightgreen", "lightcoral")) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = NA),  
        panel.background = element_rect(fill = "white", color = NA))

print(combined_plot)

#ggsave(filename = "combined_hist_plot.png", plot = combined_hist_plot,units = "in",dpi = 50)
## ---------------------------------------------------------------------------
# combined histogram: ship speed & minute count
## ---------------------------------------------------------------------------
# reshape the data into long format for just Ship Speed and Count Minute
long_data <- underway_obvs %>%
  select(Cruise, `Ship Speed`, `Count Minute`) %>%
  pivot_longer(cols = c(`Ship Speed`, `Count Minute`), 
               names_to = "Variable", 
               values_to = "Value")

# plot the overlayed histograms for Ship Speed and Count Minute, faceted by year
ship_speed_min_combined_plot <- ggplot(long_data, aes(x = Value, fill = Variable, color = Variable)) +
  geom_histogram(position = "identity", alpha = 0.4, bins = 30) +
  facet_wrap(~Cruise, scales = "free") +  # Change `Cruise` to `Year` if necessary
  labs(title = "Overlayed Histograms of Ship Speed and Count Minute by Year",
       x = "Value", y = "Frequency") +
  scale_fill_manual(values = c("lightgreen", "lightblue")) + 
  scale_color_manual(values = c("lightgreen", "lightblue")) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = NA),  
        panel.background = element_rect(fill = "white", color = NA))

print(ship_speed_min_combined_plot)


## summary stats
speed_min_combined_summary_stats <- underway_obvs %>%
  group_by(Cruise) %>%
  summarise(
    Mean_Ship_Speed = mean(`Ship Speed`, na.rm = TRUE),
    Median_Ship_Speed = median(`Ship Speed`, na.rm = TRUE),
    SD_Ship_Speed = sd(`Ship Speed`, na.rm = TRUE),
    Mean_Count_Minute = mean(`Count Minute`, na.rm = TRUE),
    Median_Count_Minute = median(`Count Minute`, na.rm = TRUE),
    SD_Count_Minute = sd(`Count Minute`, na.rm = TRUE)
  )

print(speed_min_combined_summary_stats)

## density plot
ggplot(long_data, aes(x = Value, fill = Variable, color = Variable)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~Cruise, scales = "free") +
  labs(title = "Density Plots of Ship Speed and Count Minute by Year",
       x = "Value", y = "Density") +
  scale_fill_manual(values = c("lightgreen", "lightblue")) + 
  scale_color_manual(values = c("lightgreen", "lightblue")) +
  theme_minimal()

## box plots
ggplot(long_data, aes(x = as.factor(Cruise), y = Value, fill = Variable)) +
  geom_boxplot() +
  facet_wrap(~Variable, scales = "free") +
  labs(title = "Boxplots of Ship Speed and Count Minute by Year",
       x = "Year", y = "Value") +
  scale_fill_manual(values = c("lightgreen", "lightblue")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# create table
kable_table <- kable(speed_min_combined_summary_stats, 
                     caption = "Summary Statistics of Ship Speed and Count Minute by Year")
# write.csv(kable_table, file = "summary_stats.csv", row.names = FALSE)

## summaries
distances_summary <- summary(underway_obvs$Distance)
print(distances_summary)
shipspeed_summary <- summary(underway_obvs$`Ship Speed`)
print(shipspeed_summary)
minutecount_summary <- summary(underway_obvs$`Count Minute`)
print(minutecount_summary)

## ---------------------------------------------------------------------------
# summary stats
## ---------------------------------------------------------------------------

# summary statistics for JulianDay
julian_summary <- data.frame(
  Statistic = c("Min", "Max", "Average"),
  JulianDay = c(min(underway_obvs$JulianDay, na.rm = TRUE),
                max(underway_obvs$JulianDay, na.rm = TRUE),
                mean(underway_obvs$JulianDay, na.rm = TRUE))
)

# summary statistics for Hour
hour_summary <- data.frame(
  Statistic = c("Min", "Max", "Average"),
  Hour = c(min(underway_obvs$Hour, na.rm = TRUE),
           max(underway_obvs$Hour, na.rm = TRUE),
           mean(underway_obvs$Hour, na.rm = TRUE))
)

# summary statistics for Minute
minute_summary <- data.frame(
  Statistic = c("Min", "Max", "Average"),
  Minute = c(min(underway_obvs$Minute, na.rm = TRUE),
             max(underway_obvs$Minute, na.rm = TRUE),
             mean(underway_obvs$Minute, na.rm = TRUE))
)

# Combine all summaries into one data frame
summary_table <- merge(julian_summary, hour_summary, by = "Statistic", suffixes = c("_JulianDay", "_Hour"))
summary_table <- merge(summary_table, minute_summary, by = "Statistic")
colnames(summary_table)[colnames(summary_table) == "Minute"] <- "Minute"

# Print the table in a format suitable for copying
cat("Summary Statistics Table:\n")
cat("Statistic | JulianDay | Hour | Minute\n")
cat("--------------------------------------\n")
for (i in 1:nrow(summary_table)) {
  cat(sprintf("%-10s | %-9.2f | %-4.2f | %-6.2f\n",
              summary_table$Statistic[i],
              summary_table$JulianDay[i],
              summary_table$Hour[i],
              summary_table$Minute[i]))
}