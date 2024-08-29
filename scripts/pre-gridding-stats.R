rm(list=ls())

library(ggplot2)
library(ggmap)
library(dplyr)
library(geosphere)
library(stringr)
library(sp)

## ---------------------------------------------------------------------------
# read in all files
## ---------------------------------------------------------------------------
# underway_data  <- read_csv(here::here("data/Cruise_Transect_Header_merged_withfixes.csv"),show_col_types = FALSE)
# underway/transect data
underway_data <- read.csv("data/Cruise_Transect_Header_merged_withfixes.csv", header = TRUE) %>% 
  subset(!is.na(as.numeric(Latitude.Start)) & 
           !is.na(as.numeric(Longitude.Start))) %>% 
  mutate(Latitude.Start = as.numeric(Latitude.Start), 
         Longitude.Start = as.numeric(Longitude.Start)) %>% 
  mutate(Latitude.Start = -abs(as.numeric(Latitude.Start)), 
         Longitude.Start = -abs(as.numeric(Longitude.Start)))
underway_data$Cruise <- factor(underway_data$Cruise, levels = unique(underway_data$Cruise))
  levels(underway_data$Cruise) <- c(1993:2019, 2021:2024)

# observation data
obvs_data <- read.csv("data/Cruise_Transect_Observations_merged.csv", header = TRUE) %>% 
  mutate(Count.Minute = as.numeric(Count.Minute))

# station data
station_data <- read.csv("data/LTERStns.csv", header = TRUE)



## ---------------------------------------------------------------------------
# calculating distnace between lat/long
## ---------------------------------------------------------------------------
## calculating distance between lat/long starts
# create a new column 'Distance (m)' to store the distances between consecutive points
# underway_data$Distance <- c(NA, 
#                             distHaversine(
#                               cbind(underway_data$Longitude.Start[-nrow(underway_data)], underway_data$Latitude.Start[-nrow(underway_data)]),
#                               cbind(underway_data$Longitude.Start[-1], underway_data$Latitude.Start[-1])
#                             ))


# create a matrix of coordinates for all points
coordinates <- cbind(underway_data$Longitude.Start, underway_data$Latitude.Start)

# compute distances between consecutive points
distances <- distHaversine(coordinates[-nrow(coordinates), ], coordinates[-1, ])
underway_data$Distance <- c(distances, NA) 



## ---------------------------------------------------------------------------
# plotting distances btwn lat/long coordinates
## ---------------------------------------------------------------------------
plot(underway_data$Longitude.Start, underway_data$Latitude.Start, type = "n", 
     xlab = "Longitude", ylab = "Latitude", main = "Lat/Long and Distances")

# draw lines between consecutive points
for (i in 1:(nrow(underway_data) - 1)) {
  segments(underway_data$Longitude.Start[i], underway_data$Latitude.Start[i],
           underway_data$Longitude.Start[i + 1], underway_data$Latitude.Start[i + 1],
           col = "lightblue", lwd = 2)
}
# add points to the plot
points(underway_data$Longitude.Start, underway_data$Latitude.Start, pch = 19, col = "maroon")


## ---------------------------------------------------------------------------
# histograms
## ---------------------------------------------------------------------------
## separating the Date/Time column contents into separate columns
# extract the Julian day, hour, and minute from the `YearDay.Hour.Minute column
underway_data <- underway_data %>%
  mutate(YearDay.Hour.Minute = str_pad(as.character(YearDay.Hour.Minute), width = 7, pad = "0"),  # Pad to 7 characters
         JulianDay = substr(YearDay.Hour.Minute, 1, 3),  # First 3 characters for JulianDay
         Hour = substr(YearDay.Hour.Minute, 4, 5),       # Next 2 characters for Hour
         Minute = substr(YearDay.Hour.Minute, 6, 7))     # Last 2 characters for Minute



## histogram of dates
underway_data$JulianDay <- as.numeric(underway_data$JulianDay)
ggplot(underway_data, aes(x = JulianDay)) + 
  geom_histogram(binwidth = 1, fill = "lightcoral", color = "black") +
  labs(title = "Histogram of Dates", x = "Date", y = "Frequency") +
  theme_minimal() +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 week")  # Adjust for date labels and breaks

## ---------------------------------------------------------------------------
# evaluating hours column
## ---------------------------------------------------------------------------
# histogram with outliers
underway_data$Hour <- as.numeric(underway_data$Hour)
hist(underway_data$Hour, breaks = 24, col = "lightblue", 
     main = "Histogram of Hours", xlab = "Hour of Day", ylab = "Frequency")
mean(as.numeric(underway_data$Hour), na.rm = TRUE)

underway_data$Hour <- as.numeric(underway_data$Minute)

# calculate Q1, Q3, and IQR (difference btwn Q3 and Q1)
Q1 <- quantile(underway_data$Hour, 0.25, na.rm = TRUE)
Q3 <- quantile(underway_data$Hour, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

# define the lower and upper bounds for outliers
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# filter out the outliers
underway_data_filtered <- underway_data %>%
  filter(Minute >= lower_bound & Minute <= upper_bound)

## histogram of hours
underway_data$Hour <- as.numeric(underway_data$Hour)
hist(underway_data$Hour, breaks = 24, col = "lightblue", 
     main = "Histogram of Hours", xlab = "Hour of Day", ylab = "Frequency")
mean(as.numeric(underway_data$Hour), na.rm = TRUE)

## ---------------------------------------------------------------------------
# evaluating minutes column
## ---------------------------------------------------------------------------
## histogram of minutes with outliers
underway_data$Minute <- as.numeric(underway_data$Minute)
ggplot(underway_data, aes(x = Minute)) + 
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black") +
  labs(title = "Histogram of Minutes", x = "Minute", y = "Frequency") +
  theme_minimal()
mean(as.numeric(underway_data$Minute), na.rm = TRUE)

## find outliers and remove based on 
# convert Minute to numeric
underway_data$Minute <- as.numeric(underway_data$Minute)

# calculate Q1, Q3, and IQR (difference btwn Q3 and Q1)
Q1 <- quantile(underway_data$Minute, 0.25, na.rm = TRUE)
Q3 <- quantile(underway_data$Minute, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

# define the lower and upper bounds for outliers
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# filter out the outliers
underway_data_filtered <- underway_data %>%
  filter(Minute >= lower_bound & Minute <= upper_bound)

# Plot the histogram after removing outliers
ggplot(underway_data_filtered, aes(x = Minute)) + 
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black") +
  labs(title = "Histogram of Minutes (Outliers Removed)", x = "Minute", y = "Frequency") +
  theme_minimal()
## ---------------------------------------------------------------------------
# plotting distances btwn lat/long coordinates
## ---------------------------------------------------------------------------

# summary statistics for JulianDay
julian_summary <- data.frame(
  Statistic = c("Min", "Max", "Average"),
  JulianDay = c(min(underway_data$JulianDay, na.rm = TRUE),
                max(underway_data$JulianDay, na.rm = TRUE),
                mean(underway_data$JulianDay, na.rm = TRUE))
)

# summary statistics for Hour
hour_summary <- data.frame(
  Statistic = c("Min", "Max", "Average"),
  Hour = c(min(underway_data$Hour, na.rm = TRUE),
           max(underway_data$Hour, na.rm = TRUE),
           mean(underway_data$Hour, na.rm = TRUE))
)

# summary statistics for Minute
minute_summary <- data.frame(
  Statistic = c("Min", "Max", "Average"),
  Minute = c(min(underway_data$Minute, na.rm = TRUE),
             max(underway_data$Minute, na.rm = TRUE),
             mean(underway_data$Minute, na.rm = TRUE))
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



## histogram of total
# extract the Julian Day, Hour, and Minute
underway_data$JulianDay <- as.numeric(substr(underway_data$YearDay.Hour.Minute, 1, 3))
underway_data$Hour <- as.numeric(substr(underway_data$YearDay.Hour.Minute, 4, 5))
underway_data$Minute <- as.numeric(substr(underway_data$YearDay.Hour.Minute, 6, 7))

# convert to total minutes since the start of the year
underway_data$total_minutes <- (underway_data$JulianDay - 1) * 24 * 60 + underway_data$Hour * 60 + underway_data$Minute

# create the histogram of total minutes since start of year
ggplot(underway_data, aes(x = total_minutes)) + 
  geom_histogram(binwidth = 60, fill = "lightblue", color = "black") +  # 60 minutes binwidth
  labs(title = "Histogram of Total Minutes Since Start of Year", x = "Total Minutes", y = "Frequency") +
  theme_minimal()


## histogram of ship speed
hist(underway_data$Ship.Speed, breaks = 24, col = "lightblue", 
     main = "Histogram of Ship Speed", xlab = "Ship Speed", ylab = "Frequency")

## ---------------------------------------------------------------------------
# plotting track lines with stations as a top layer
## ---------------------------------------------------------------------------

# plot all lat/long track lines first
ggplot(underway_data, aes(x = `Longitude Start`, y = `Latitude Start`)) +
  geom_point() +
  labs(title = "Basic Plot", x = "Longitude", y = "Latitude")

# overlay stations




# plot with faceting by year
plot <- ggplot(underway_data, aes(x = `Longitude Start`, y = `Latitude Start`)) +
  geom_point(color = "blue") +                     
  facet_wrap(~ Cruise, scales = "free") +            
  labs(title = "Latitude and Longitude Coordinates by Year",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal() +                                
  theme(strip.text = element_text(size = 8))   

print(plot)


## ---------------------------------------------------------------------------
# gridding attempt
## ---------------------------------------------------------------------------

# define survey parameters
radius <- 300 #meters
survey_duration <- 30 * 60  # 30 minutes converted to seconds

# calculatee distance traveled by ship
underway_data <- underway_data %>%
  mutate(
    Ship.Speed.mps = Ship.Speed * 0.514444,  # Convert knots to meters per second
    Distance.Traveled = Ship.Speed.mps * survey_duration  # Distance = speed * time
  )




# Define grid size in degrees based on the 300-meter radius
# 1 degree of latitude = 111 kilometers
# hEarth's circumference is ~ 40,000 kilometers 
# there are 360 degrees of latitude around the Earth
# each degree of latitude corresponds to approximately 111 kilometers
# 1 meter of latitude = to 1/111000 degrees (111 kilometers = 111,000 meters).

deg_per_meter_lat <- 1 / 111000
deg_per_meter_lon <- 1 / (111000 * cos(mean(underway_data$Latitude.Start * pi / 180)))

grid_size_lat <- deg_per_meter_lat * 300  # 300 meters in latitude degrees
grid_size_lon <- deg_per_meter_lon * 300  # 300 meters in longitude degrees

# Create grid cells
underway_data <- underway_data %>%
  mutate(
    Grid_Lat = floor(Latitude.Start / grid_size_lat) * grid_size_lat,
    Grid_Lon = floor(Longitude.Start / grid_size_lon) * grid_size_lon
  )


# preprocess Data to calculate counts for each grid cell
grid_counts <- underway_data %>%
  count(Grid_Lat, Grid_Lon)  # Count occurrences in each grid cell

# plot the grid based on starting positions
ggplot(grid_counts, aes(x = Grid_Lon, y = Grid_Lat)) +
  geom_tile(aes(fill = n), color = "black") +  # Use 'n' for counts
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "Gridded Data Based on Starting Positions", x = "Longitude", y = "Latitude")

# -----------------------------------------------------------------------------
seabird_100 <- read.csv("data/table_100.csv", header = TRUE, quote = "",
                        row.names = NULL, stringsAsFactors = FALSE)
colnames(seabird_100) <- c("studyName", "Event.Number", "Year.Month","CountMinute","Species",
                           "Number","Linkages","Behavior","Direction","Notes")
seabird_100_subset <- seabird_100[, c("Year.Month", "Event.Number", "CountMinute", 
                                      "Species", "Number")]
seabird_100_subset$Year.Month <- as.numeric(seabird_100_subset$Year.Month)
underway_data$Year.Month <- as.numeric(underway_data$Year.Month)


df_merged <- seabird_100_subset %>%
  left_join(underway_data, by = c("Year.Month", "Event.Number"), relationship = "many-to-many")
df_merged$Year.Month <- factor(df_merged$Year.Month, levels = unique(df_merged$Year.Month))
levels(df_merged$Year.Month) <- c(1993:2019, 2021:2024)

# Filter for the specific species and year
filtered_data <- df_merged %>%
  filter(Year.Month == 1994 & Species == "ADPE") %>%  
  mutate(
    Grid_Lat = floor(Latitude.Start / grid_size_lat) * grid_size_lat,
    Grid_Lon = floor(Longitude.Start / grid_size_lon) * grid_size_lon
  )

filtered_data$Number <- as.numeric(filtered_data$Number)
# Aggregate counts by grid location (lat/lon) using the 'Number' column
grid_counts <- filtered_data %>%
  group_by(Grid_Lat, Grid_Lon) %>%
  summarise(Count = sum(Number, na.rm = TRUE), .groups = "drop")

# Plot the grid based on counts for the specific species and year
ggplot(grid_counts, aes(x = Grid_Lon, y = Grid_Lat)) +
  geom_tile(aes(fill = Count), color = "black") +  # 'Count' represents the density
  scale_fill_viridis_c() +  # Color scale for density
  theme_minimal() +
  labs(
    title = "Density of ADPE in 1994",  # Update with species and year
    x = "Longitude",
    y = "Latitude",
    fill = "Count"
  )





df_merged$Number <- as.numeric(df_merged$Number)
# Modify the aggregation to keep species as a grouping factor
grid_counts <- df_merged %>%
  filter(Year.Month == 1994) %>%
  mutate(
    Grid_Lat = floor(Latitude.Start / grid_size_lat) * grid_size_lat,
    Grid_Lon = floor(Longitude.Start / grid_size_lon) * grid_size_lon
  ) %>%
  group_by(Species, Grid_Lat, Grid_Lon) %>%
  summarise(Number = sum(Number, na.rm = TRUE), .groups = "drop")

# Plot the grid based on counts for multiple species
ggplot(grid_counts, aes(x = Grid_Lon, y = Grid_Lat)) +
  geom_tile(aes(fill = Number), color = "black") +
  scale_fill_viridis_c() +
  facet_wrap(~ Species) +  
  theme_minimal() +
  labs(
    title = "Density of Multiple Species in 1994",
    x = "Longitude",
    y = "Latitude",
    fill = "Count"
  )










# Plot with facetting by species
ggplot(grid_counts, aes(x = Grid_Lon, y = Grid_Lat)) +
  geom_tile(aes(fill = Number), color = "black") +
  scale_fill_viridis_c() +
  facet_wrap(~ Species) +
  theme_minimal() +
  labs(
    title = "Density of Species in 1994",
    x = "Longitude",
    y = "Latitude",
    fill = "Count"
  )
