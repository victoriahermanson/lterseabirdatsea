## V. Hermanson
## IBCSO bathymetry v2, https://doi.org/10.1594/PANGAEA.937574
## BAS/ADD coastline v7.8, https://doi.org/10.5285/3ea85903-e068-4242-a63f-46767e16f85c
## Map of LTER grid stations & gridded seabird

# SET YOUR WORKING DIRECTORY TO SOURCE FILE LOCATION
setwd("/Users/victoriahermanson/Documents/github/GitHub/lterseabirdatsea/data")


# Package names
# use terra instead of raster pkg
# terra works with raster and sf uses shapefiles (points, lines, polygons)
packages <- c( "ncdf4","httr", "terra", "tidyterra", "ggplot2", "sf",
               "sfheaders", "raster", "tidyverse")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Load packages 
invisible(lapply(packages, library, character.only = TRUE))

# Read in and prep data

# coordinate reference system
# defining settings - WGS1984 (standard lat/long)
# epsg codes
crs <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

bathy <- rast('IBCSO_v2_bed.nc')

coast <- vect("add_coastline_medium_res_polygon_v7_8.shp")

projCoast <- project(coast, crs)

projBathy <- project(bathy, crs)


cropBathy <- crop(projBathy,
                  ext(-85, -55, -72, -60))

maskBathy <- mask(cropBathy, projCoast, inverse = TRUE)

# Set color breaks

h <- hist(values(maskBathy * -1), 100, plot=FALSE)
breaks <- h$breaks
n <- length(breaks)-1

# Define color palette

library(cmocean)

colors <- cmocean("gray", direction = -1)

# Set bathy color scale

cols <- colors(n)

cols

# Read station locations

stns <- read_csv("LTERStns.csv") 

levels(stns$Offshore) <- c("Coast", "Shelf", "Slope")

#pdf("SamplingMap.pdf", width = 5, height = 3.4)

ggplot() +
  geom_spatraster(data = maskBathy * -1, show.legend = FALSE) +
  scale_fill_gradient(low = "#FFFFFDFF", high = "#595858FF",
                      name = "Depth (m)") +
  theme_classic(base_size = 14) + 
  scale_x_continuous(limits = c(-80, -59), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-70.25, -63), expand = c(0, 0)) +
  geom_sf(data = projCoast, fill = "grey", col = "black") +
  geom_point(data = stns,
             aes(x = Longitude, y = Latitude,
                 col = factor(Offshore,
                              levels = c("Coast", "Shelf", "Slope")))) +
  labs(color = 'Region') +
  scale_color_manual(values = c("Slope" = "#ef8a62", "Shelf"= "#beaed4",
                                "Coast" = "#67a9cf")) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())

#dev.off()



