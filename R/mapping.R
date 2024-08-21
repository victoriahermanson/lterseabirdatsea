rm(list=ls())

grid_df <- read_csv(here::here("data/LTERGrid2.csv"),show_col_types = FALSE)

library(ggplot2)
library(maps) 
library(rnaturalearth)

# Example dataframe of coordinates
coords <- data.frame(grid_df$Latitude, grid_df$Longitude)
world_map <- map_data("world")

# Calculate limits based on coordinates with a buffer
lon_margin <- 10  # Adjust the margin as needed
lat_margin <- 10  # Adjust the margin as needed

xlim <- range(coords$grid_df.Latitude) + c(-lon_margin, lon_margin)
ylim <- range(coords$grid_df.Longitude) + c(-lat_margin, lat_margin)

# Plot world map
p <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), 
               fill = "white", color = "black") +
#  coord_fixed(ratio = 1.3)  # Adjust ratio for better world map aspect
  coord_fixed(ratio = 2) +  
  xlim(xlim) + ylim(ylim) 

# Add points for coordinates
p <- p +
  geom_point(data = coords, aes(x = grid_df.Latitude, y = grid_df.Longitude), color = "red", size = 3)

# Customize plot appearance (optional)
p <- p +
  theme_minimal() +
  labs(title = "Locations on World Map", x = "Longitude", y = "Latitude")

# Print the plot
print(p)
