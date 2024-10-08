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


## ---------------------------------------------------------------------------
# calculating distance between lat/long
# NOTE: manually put in decimal for 1 lat and 5 longs!
## ---------------------------------------------------------------------------
# underway_obvs <- underway_obvs %>%
#   filter(!is.na(Distance))

# make sure lat/long columns are numeric
underway_obvs <- underway_obvs %>%
  mutate(`Latitude Start` = as.numeric(`Latitude Start`),
         `Latitude End` = as.numeric(`Latitude End`),
         `Longitude Start` = as.numeric(`Longitude Start`),
         `Longitude End` = as.numeric(`Longitude End`))

# filter invalid values
underway_obvs <- underway_obvs %>%
  filter(`Longitude Start` >= -180 & `Longitude Start` <= 180,
         `Longitude End` >= -180 & `Longitude End` <= 180,
         `Latitude Start` >= -90 & `Latitude Start` <= 90,
         `Latitude End` >= -90 & `Latitude End` <= 90)

# calc differences
underway_obvs <- underway_obvs %>%
  mutate(Latitude_Diff = abs(`Latitude End` - `Latitude Start`),
         Longitude_Diff = abs(`Longitude End` - `Longitude Start`))

# create matrix of coordinates for consecutive points using Latitude and Longitude Start and End
start_coordinates <- cbind(underway_obvs$`Longitude Start`, underway_obvs$`Latitude Start`)
end_coordinates <- cbind(underway_obvs$`Longitude End`, underway_obvs$`Latitude End`)

# calc distances using Haversine formula
distances <- distHaversine(start_coordinates, end_coordinates)

# add Distance column
underway_obvs$Distance <- distances

# plot lat/long coordinates and set up plot window
plot(underway_obvs$`Longitude Start`, underway_obvs$`Latitude Start`,
     xlab = "Longitude", ylab = "Latitude",
     main = "Lat/Long and Distances", type = "n")  # Initialize the plot without points

# draw lines between consecutive points using segments
for (i in 1:nrow(underway_obvs)) {
  segments(underway_obvs$`Longitude Start`[i], underway_obvs$`Latitude Start`[i],
           underway_obvs$`Longitude End`[i], underway_obvs$`Latitude End`[i],
           col = "lightblue", lwd = 2)
}

# add points
points(underway_obvs$`Longitude Start`, underway_obvs$`Latitude Start`,
       pch = 19, col = "maroon")



### faceted by year
distances_facet <- ggplot(underway_obvs, aes(x = `Longitude Start`, y = `Latitude Start`)) +
  geom_segment(aes(xend = `Longitude End`, yend = `Latitude End`), 
               color = "lightblue", size = 0.5) +  # lines between points
  geom_point(color = "maroon", size = 0.5) +  # points at start locations
  labs(title = "Lat/Long Coordinates and Distances",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = NA),  
        panel.background = element_rect(fill = "white", color = NA)) +
  facet_wrap(~ Cruise)  # Facet by Year

#ggsave(filename = "distances_facet.png", plot = distances_facet, units = "in",dpi = 300)

# -----------------------------------------------------------------------------
# facet plots distances
# -----------------------------------------------------------------------------
# calculate the IQR for Distance
Q1 <- quantile(underway_obvs$Distance, 0.25, na.rm = TRUE)
Q3 <- quantile(underway_obvs$Distance, 0.75, na.rm = TRUE)
IQR_value <- IQR(underway_obvs$Distance, na.rm = TRUE)

# define the lower and upper bounds for outliers
# lower_bound <- Q1 - 1.5 * IQR_value
# upper_bound <- Q3 + 1.5 * IQR_value
lower_bound <- Q1 - 1.5
upper_bound <- Q3 + 1.5 

# filter to remove outliers
filtered_distance <- underway_obvs %>%
  filter(Distance >= lower_bound & Distance <= upper_bound)

# histogram of distances in total (over all years)
total_distance_hist <- ggplot(filtered_distance, aes(x = Distance)) +
  geom_histogram(binwidth = 10, fill = "lightcoral", color = "black")
print(total_distance_hist)
# 
# ggsave(filename = "total_distance_hist.png", plot = total_distance_hist,width = 20, height = 15,
#     units = "in",dpi = 300)

# faceted histogram of distances by year 
distance_hists_facet <- ggplot(filtered_distance, aes(x = Distance)) +
  geom_histogram(binwidth = 10, fill = "lightcoral", color = "black") +
  labs(title = "Histogram of Distances by Year", x = "Distance", y = "Frequency") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = NA),  
        panel.background = element_rect(fill = "white", color = NA)) +  
  facet_wrap(~ Cruise, scales = "free") +
  xlim(lower_bound, upper_bound)  
print(distance_hists_facet)
ggsave(filename = "distance_hists_facet.png", plot = distance_hists_facet,width = 30, height = 15,
       units = "in",dpi = 300)


# ggsave(filename = "distance_hists.png", plot = distance_hists,width = 20, height = 15, 
#    units = "in",dpi = 300)
# ggsave(filename = "distance_hists.png", plot = distance_hists, units = "in",dpi = 300)



### not sure this is working the way I'm wanting it to!!
# find common min and max lat/long values
min_longitude <- min(c(underway_obvs$`Longitude Start`, underway_obvs$`Longitude End`), na.rm = TRUE)
max_longitude <- max(c(underway_obvs$`Longitude Start`, underway_obvs$`Longitude End`), na.rm = TRUE)
min_latitude <- min(c(underway_obvs$`Latitude Start`, underway_obvs$`Latitude End`), na.rm = TRUE)
max_latitude <- max(c(underway_obvs$`Latitude Start`, underway_obvs$`Latitude End`), na.rm = TRUE)

# faceted scatter plot with lines between points by year 
facet_scatter_latlong <- ggplot(underway_obvs, aes(x = `Longitude Start`, y = `Latitude Start`)) +
  geom_point(pch = 19, color = "maroon") +
  geom_segment(aes(xend = `Longitude End`, yend = `Latitude End`), color = "lightblue", lwd = 2) +
  labs(title = "Lat/Long and Distances by Year", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = NA),  
        panel.background = element_rect(fill = "white", color = NA)) +
  facet_wrap(~ Cruise, scales = "free") +
  xlim(min_longitude, max_longitude) +  
  ylim(min_latitude, max_latitude)     
print(facet_scatter_latlong)

# ggsave(filename = "facet_scatter_latlong.png", plot = facet_scatter_latlong,width = 22, height = 15, 
#      units = "in",dpi = 300)
#ggsave(filename = "facet_scatter_latlong.png", plot = facet_scatter_latlong,units = "in",dpi = 300)




# -----------------------------------------------------------------------------
# distance calcs per event/year for trackline
# -----------------------------------------------------------------------------
# group by Event Number and Cruise
distance_summary <- underway_obvs %>%
  group_by(`Event Number`, Cruise) %>% # start and end lat/long for each event, each year
  summarise(
    start_lat = first(`Latitude Start`),
    start_long = first(`Longitude Start`),
    end_lat = last(`Latitude End`),
    end_long = last(`Longitude End`),
    duration_minutes = max(`Count Minute`) - min(`Count Minute`), # calc difference in minutes
    
    .groups = 'drop'  # drop grouping structure after summarizing
  )

distance_summary_withoutliers <- distance_summary %>% # calc distance between the start and end coordinates for each event, each year
  mutate(
    distance_km = distHaversine(
      cbind(start_long, start_lat), 
      cbind(end_long, end_lat)
    ) / 1000  # convert meters to kilometers
  )

print(distance_summary)
summary(distance_summary_withoutliers)
summary(distance_summary_withoutliers$duration_minutes)
summary(distance_summary_withoutliers$distance_km)

# # calc ranges
# lower_bound <- quantile(distance_summary$distance_km, 0.25)
# upper_bound <- quantile(distance_summary$distance_km, 0.75)
# 
# # filter the data to remove outliers
# distance_summary_nooutliers <- distance_summary %>%
#   filter(distance_km >= lower_bound & distance_km <= upper_bound)

# manually filter out outliers
distance_summary_nooutliers <- distance_summary_withoutliers %>%
  filter(distance_km >= 0 & distance_km <= 50)

# plot Distance by Event Number, each year
ggplot(distance_summary_nooutliers, aes(x = `Event Number`, y = distance_km)) +
  geom_bar(stat = "identity") +
  labs(title = "Distance Covered per Event", x = "Event Number", y = "Distance (meters)") +
  facet_wrap(~ Cruise)


# manually filter out outliers
duration_summary_nooutliers <- distance_summary_withoutliers %>%
  filter(duration_minutes >= 0 & duration_minutes <= 50)

summary(distance_summary_nooutliers$duration_minutes)
summary(distance_summary_nooutliers$distance_km)

# # plot Duration by Event Number, each year
# ggplot(duration_summary_withoutoutliers, aes(x = `Event Number`, y = duration_minutes)) +
#   geom_bar(stat = "identity") +
#   labs(title = "Time Duration per Event", x = "Event Number", y = "Duration (minutes)") +
#   facet_wrap(~ Cruise)

## variations of plots
# pretty colors
ggplot(duration_summary_nooutliers, aes(x = `Event Number`, y = duration_minutes, fill = Cruise)) +
  geom_bar(stat = "identity") +
  labs(title = "Time Duration per Event", x = "Event Number", y = "Duration (minutes)") +
  facet_wrap(~ Cruise)

# with smooth line
distance_pereventperyear_nooutliers <- 
  ggplot(distance_summary_nooutliers, aes(x = `Event Number`, y = distance_km)) +
  geom_bar(stat = "identity", fill = "blue") +  
  geom_smooth(method = "loess", color = "darkgreen", se = FALSE, size = 0.5) +  # smooth trend line
  labs(title = "Distance per Event", x = "Event Number", y = "Distance (km)") +
  facet_wrap(~ Cruise)
print(distance_pereventperyear_nooutliers)

# with smooth line
duration_pereventperyear_nooutliers <- 
  ggplot(duration_summary_nooutliers, aes(x = `Event Number`, y = duration_minutes)) +
  geom_bar(stat = "identity", fill = "blue") +  
  geom_smooth(method = "loess", color = "darkgreen", se = FALSE, size = 0.5) +  # smooth trend line
  labs(title = "Time Duration per Event", x = "Event Number", y = "Duration (minutes)") +
  facet_wrap(~ Cruise)
print(duration_pereventperyear_nooutliers)
# ggsave("distance_pereventperyear_nooutliers.png", plot = distance_pereventperyear_nooutliers, 
#       width = 10, height = 8, dpi = 300)

# ggsave("duration_pereventperyear_nooutliers.png", plot = duration_pereventperyear_nooutliers, 
#        width = 10, height = 8, dpi = 300)
summary(distance_summary_nooutliers$distance_km)
summary(distance_summary_nooutliers$duration_minutes)

# -----------------------------------------------------------------------------
# track lines for each event, faceted by year, separate plot for each spp
# -----------------------------------------------------------------------------
# map with first layer being stations

# layer on top of stations the tracklines per event, faceted by year for each spp dist

# event_tracklines <- ggplot(distance_summary_withoutoutliers, aes(x = start_long, y = start_lat)) +
#   geom_segment(aes(xend = end_long, yend = end_lat, color = as.factor(`Event Number`))) +  # Draw segments for each event
#   labs(title = "Event Tracklines by Year", x = "Longitude", y = "Latitude") +
#   facet_wrap(~ Cruise) +  
#   theme_minimal() +
#   theme(legend.position = "none")  

# ggsave("event_tracklines.png", plot = event_tracklines,
#        width = 10, height = 8, dpi = 300)

ggplot(distance_summary_nooutliers, aes(x = start_long, y = start_lat)) +
  geom_segment(aes(xend = end_long, yend = end_lat, color = as.factor(`Event Number`))) +  # Draw segments for each event
  labs(title = "Event Tracklines by Year", x = "Longitude", y = "Latitude") +
  facet_wrap(~ Cruise) +  # Facet by year
  theme_minimal(base_size = 15) +  # Set a minimal theme with a base font size
  theme(
    panel.background = element_rect(fill = "white", color = NA),  # White background for the panels
    plot.background = element_rect(fill = "white", color = NA),  # White background for the entire plot
    legend.background = element_rect(fill = "white", color = NA),  # White background for the legend
    strip.background = element_rect(fill = "white", color = "black"),  # White background for facet labels with a black border
    legend.position = "none" 
  )



event_tracklines_2 <- ggplot(distance_summary_nooutliers, aes(x = start_long, y = start_lat)) +
  geom_segment(aes(xend = end_long, yend = end_lat), color = "blue") +  # Draw segments in blue color for all events
  labs(title = "Event Tracklines by Year", x = "Longitude", y = "Latitude") +
  facet_wrap(~ Cruise) +  # Facet by year
  theme_minimal(base_size = 10) +  # Set a minimal theme with a smaller base font size
  theme(
    panel.background = element_rect(fill = "white", color = NA),  # White background for the panels
    plot.background = element_rect(fill = "white", color = NA),  # White background for the entire plot
    legend.background = element_rect(fill = "white", color = NA),  # White background for the legend
    strip.background = element_rect(fill = "white", color = "black"),  # White background for facet labels with a black border
    legend.position = "none",  # Remove legend if Event Number is too detailed
    axis.text = element_text(size = 8),  # Smaller axis text
    axis.title = element_text(size = 10),  # Smaller axis titles
    strip.text = element_text(size = 10)  # Smaller facet label text
  ) +
  coord_cartesian(
    ylim = c(min(distance_summary_nooutliers$start_lat, distance_summary_nooutliers$end_lat), 
             max(distance_summary_nooutliers$start_lat, distance_summary_nooutliers$end_lat)),
    xlim = c(min(distance_summary_nooutliers$start_long, distance_summary_nooutliers$end_long),
             max(distance_summary_nooutliers$start_long, distance_summary_nooutliers$end_long))
  )
print(event_tracklines_2)
# ggsave("event_tracklines_2.png", plot = event_tracklines_2,
#        width = 10, height = 8, dpi = 300)


## adding spp data to show distributions along trackline per year
# merge Species and Number column to distance_summary_withoutoutliers

# merge dataframes
merged_data <- distance_summary_nooutliers %>%
  left_join(
    underway_obvs %>% 
      select(Cruise, `Event Number`, Species, Number),  # Include all species here
    by = c("Cruise", "Event Number")
  )

# filter for only ADPE
# merged_data <- distance_summary_nooutliers %>%
#   left_join(
#     underway_obvs %>%
#       filter(Species == "ADPE") %>%
#       select(Cruise, `Event Number`, Species, Number),
#     by = c("Cruise", "Event Number")
#   )


## plotting
adpe_trackline <- ggplot(merged_data, aes(x = start_long, y = start_lat)) +
  geom_segment(aes(xend = end_long, yend = end_lat), color = "blue") +  # blue segments
  geom_point(aes(x = end_lat, y = start_lat, size = Number, color = Number), alpha = 0.6) +  # overlay species distribution
  labs(title = "Event Tracklines and ADPE Distribution by Year", x = "Longitude", y = "Latitude") +
  facet_wrap(~ Cruise) +  
  theme_minimal(base_size = 6) +  
  theme(
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    legend.background = element_rect(fill = "white", color = NA),  
    strip.background = element_rect(fill = "white", color = "black"),  
    legend.position = "right",  # Place the legend to the right
    axis.text = element_text(size = 4),  # Smaller axis text
    axis.title = element_text(size = 6),  # Smaller axis titles
    strip.text = element_text(size = 6)  # Smaller facet label text
  ) +
  coord_cartesian(
    ylim = c(min(merged_data$start_lat, merged_data$end_lat), 
             max(merged_data$start_lat, merged_data$end_lat)),
    xlim = c(min(merged_data$start_long, merged_data$end_long),
             max(merged_data$start_long, merged_data$end_long))
  ) +
  scale_color_viridis_c() +  # color scale
  scale_size_continuous(range = c(1, 6))  #range of point sizes
print(adpe_trackline)


#ggsave("adpe_trackline.png", plot = adpe_trackline, dpi = 300)

# filter for Cruise year 1995
filtered_1995 <- merged_data[merged_data$Cruise == 1995, ]

# plot for Cruise 1995
adpe_trackline_1995 <- ggplot(filtered_1995, aes(x = start_long, y = start_lat)) +
  geom_segment(aes(xend = end_long, yend = end_lat), color = "blue") +  # Tracklines
  geom_point(aes(x = end_long, y = end_lat, size = Number, color = Number), alpha = 0.6) +  # Species distribution
  labs(title = "Event Tracklines and ADPE Distribution for Cruise 1995", x = "Longitude", y = "Latitude") +
  theme_minimal(base_size = 6) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    strip.background = element_rect(fill = "white", color = "black"),
    legend.position = "right",
    axis.text = element_text(size = 4),
    axis.title = element_text(size = 6),
    strip.text = element_text(size = 6)
  ) +
  coord_cartesian(
    ylim = range(c(filtered_1995$start_lat, filtered_1995$end_lat)),  # Adjust y limits
    xlim = range(c(filtered_1995$start_long, filtered_1995$end_long))  # Adjust x limits
  ) +
  scale_color_viridis_c() +  # Color scale for species distribution
  scale_size_continuous(range = c(1, 6))  # Size range for points
print(adpe_trackline_1995)

#ggsave("adpe_trackline_1995.png", plot = adpe_trackline_1995, dpi = 300)

# plot
base_plot <- ggplot(merged_data, aes(x = start_long, y = start_lat)) +
  geom_segment(aes(xend = end_long, yend = end_lat), color = "blue", alpha = 0.3) +  
  labs(title = "Total Event Tracklines and Species Distribution by Cruise", x = "Longitude", y = "Latitude") +
  facet_wrap(~ Cruise) +  
  theme_minimal(base_size = 6) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    strip.background = element_rect(fill = "white", color = "black"),
    legend.position = "right",
    axis.text = element_text(size = 4),
    axis.title = element_text(size = 6),
    strip.text = element_text(size = 6)
  ) +
  coord_cartesian(
    ylim = range(merged_data$start_lat, merged_data$end_lat),  
    xlim = range(merged_data$start_long, merged_data$end_long)  # 
  )

# list to store plots
plots_list <- list()

# loop through each species
for (species in unique_species) {
  # Filter data for the current species
  species_data <- merged_data[merged_data$Species == species, ]
  
  # skip species if no data
  if (nrow(species_data) == 0) {
    cat("No data found for species:", species, "\n")
    next  # Skip to the next species
  }
  
  # overlay species distribution on base plot
  plot <- base_plot +
    geom_point(data = species_data, aes(x = end_long, y = end_lat, size = Number, color = Number), alpha = 0.6) +  # Species distribution
    labs(title = paste("Total Event Tracklines and", species, "Distribution by Cruise"), x = "Longitude", y = "Latitude") +
    scale_color_viridis_c() +  # Color scale for species distribution
    scale_size_continuous(range = c(1, 6))  # Size range for points
  
  # Append the plot to the list
  plots_list[[species]] <- plot
  
  # print debug message
  cat("Plotted species:", species, "\n")
}

# print all plots in plots_list
for (species in unique_species) {
  print(plots_list[[species]])
}

# ## looping through every species, faceted by Cruise
# unique_species <- unique(merged_data$Species)
# 
# # list to store plots
# plots_list <- list()
# 
# # loop through each species
# for (species in unique_species) {
#   # filter data for current species
#   species_data <- merged_data[merged_data$Species == species, ]
#   
#   # skip species if no data
#   if (nrow(species_data) == 0) {
#     cat("No data found for species:", species, "\n")
#     next  # Skip to the next species
#   }
# #  plot
# plot <- ggplot(species_data, aes(x = start_long, y = start_lat)) +
#   geom_segment(aes(xend = end_long, yend = end_lat), color = "blue") +  # Tracklines
#   geom_point(aes(x = end_long, y = end_lat, size = Number, color = Number), alpha = 0.6) +  # Species distribution
#   labs(title = paste("Event Tracklines and", species, "Distribution by Cruise"), x = "Longitude", y = "Latitude") +
#   facet_wrap(~ Cruise) +  # Facet by Cruise
#   theme_minimal(base_size = 6) +
#   theme(
#     panel.background = element_rect(fill = "white", color = NA),
#     plot.background = element_rect(fill = "white", color = NA),
#     legend.background = element_rect(fill = "white", color = NA),
#     strip.background = element_rect(fill = "white", color = "black"),
#     legend.position = "right",
#     axis.text = element_text(size = 4),
#     axis.title = element_text(size = 6),
#     strip.text = element_text(size = 6)
#   ) +
#   coord_cartesian(
#     ylim = range(merged_data$start_lat, merged_data$end_lat),  # Set global y limits
#     xlim = range(merged_data$start_long, merged_data$end_long)  # Set global x limits
#   ) +
#   scale_color_viridis_c() +  # Color scale for species distribution
#   scale_size_continuous(range = c(1, 6))  # Size range for points
# 
# # append plot to the list
# plots_list[[species]] <- plot
# 
# # what spp did ya plot
# cat("plotted species:", species, "\n")
# }
# 
# 
# # print(plots_list[[unique_species[1]]])
# # # print all plots in plots_list
# # for (species in unique_species) {
# #   print(plots_list[[species]])
# # }






