rm(list=ls())

## ---------------------------------------------------------------------------
# packages needed
## ---------------------------------------------------------------------------
library(tidyverse)
library(dplyr) 
library(ggplot2)
library(sf)

## ---------------------------------------------------------------------------
# load and prep data
## ---------------------------------------------------------------------------
data <- read.csv("your_data.csv")





## ---------------------------------------------------------------------------
# calculating area for grid
## ---------------------------------------------------------------------------

# Convert observation time from minutes to seconds
data$Time_seconds <- data$Time_minutes * 60

# Calculate distance traveled
data$Distance_m <- data$Speed_mps * data$Time_seconds

# Define the swath width
swath_width <- 300 # meters

# Calculate the area covered
data$Area_covered <- data$Distance_m * swath_width

## ---------------------------------------------------------------------------
# create grid & aggregate data
## ---------------------------------------------------------------------------
# Define grid cell size (e.g., 1000 meters)
cell_size <- 1000

# Convert lat/lon to a spatial object
data_sf <- st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 4326)

# Define grid boundaries based on the extent of your data
bbox <- st_bbox(data_sf) # Bounding box of the data

# Create grid
grid <- st_make_grid(st_as_sfc(bbox), cellsize = cell_size, what = "polygons")
grid_sf <- st_sf(geometry = st_sfc(grid, crs = 4326))

# Spatial join: assign each observation to a grid cell
data_grid <- st_join(data_sf, grid_sf)

# Aggregate data by grid cell
aggregated_data <- data_grid %>%
  group_by(grid_id = st_intersects(geometry, grid_sf)) %>%
  summarize(
    Total_count = sum(Number_of_individuals),
    Area_covered_sum = sum(Area_covered)
  )

# View the result
print(aggregated_data)


## ---------------------------------------------------------------------------
# plotting
## ---------------------------------------------------------------------------

# Optionally, plot the results
library(ggplot2)
ggplot() +
  geom_sf(data = grid_sf, fill = "white", color = "black") +
  geom_sf(data = aggregated_data, aes(fill = Total_count)) +
  theme_minimal()




library(ggplot2)
library(sf)

# Example of plotting a heatmap
ggplot() +
  geom_sf(data = grid_sf, fill = "white", color = "black") +
  geom_sf(data = aggregated_data, aes(fill = Total_count), color = NA) +
  scale_fill_viridis_c(name = "Total Count") +
  labs(title = "Seabird Sightings Heatmap", x = "Longitude", y = "Latitude") +
  theme_minimal()



# Create a density plot
# show the density of seabird sightings, you can use a density plot, 
# which provides a smooth representation of density
ggplot(data_sf) +
  stat_density_2d(aes(x = Longitude, y = Latitude, fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_viridis_c(name = "Density") +
  labs(title = "Seabird Density Plot", x = "Longitude", y = "Latitude") +
  theme_minimal()



# plot individual sightings with varying sizes and colors based on counts
ggplot(data_sf) +
  geom_sf(aes(size = Number_of_individuals, color = Species), alpha = 0.6) +
  scale_size_continuous(name = "Number of Individuals") +
  scale_color_viridis_d(name = "Species") +
  labs(title = "Seabird Sightings", x = "Longitude", y = "Latitude") +
  theme_minimal()

## ---------------------------------------------------------------------------
# plotting temporal patterns
## ---------------------------------------------------------------------------
# Convert Time column to Date or DateTime if needed
# data$Date <- as.Date(data$Date)

## time series plot
ggplot(data, aes(x = Date, y = Total_count)) +
  geom_line() +
  geom_point() +
  labs(title = "Seabird Sightings Over Time", x = "Date", y = "Total Count") +
  theme_minimal()


## seasonal plot
# Extract month or season from Date
data$Month <- format(data$Date, "%b")

# Plot by month
ggplot(data, aes(x = Month, y = Total_count, group = Year, color = Year)) +
  geom_line() +
  geom_point() +
  labs(title = "Monthly Seabird Sightings", x = "Month", y = "Total Count") +
  theme_minimal()


## ---------------------------------------------------------------------------
# combining spatial and temporal
## ---------------------------------------------------------------------------

# Example using gganimate
animated_plot <- ggplot(data_sf) +
  geom_sf(aes(size = Number_of_individuals, color = Species), alpha = 0.6) +
  transition_time(Date) +
  labs(title = "Seabird Sightings Over Time", x = "Longitude", y = "Latitude") +
  theme_minimal()

# Example with facets by year
ggplot() +
  geom_sf(data = grid_sf, fill = "white", color = "black") +
  geom_sf(data = aggregated_data, aes(fill = Total_count), color = NA) +
  facet_wrap(~ Year) +
  scale_fill_viridis_c(name = "Total Count") +
  labs(title = "Seabird Sightings by Year", x = "Longitude", y = "Latitude") +
  theme_minimal()

animate(animated_plot, duration = 20, fps = 10)
