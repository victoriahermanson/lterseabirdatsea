rm(list=ls())

## ---------------------------------------------------------------------------
# packages needed
## ---------------------------------------------------------------------------
library(ggplot2)
library(tidyverse)
library(dplyr)      
library(ggmap)      # for geographical mapping
library(viridis)    # for color palettes (optional)
#library(vroom)
library(readr)
library(ggspatial)

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

## ---------------------------------------------------------------------------
# read in all files
## ---------------------------------------------------------------------------
#seabird_100 <- read_csv(here::here("data/table_100.csv"),show_col_types = FALSE)
seabird_100 <- read_csv(here::here("data/table_100.csv"),show_col_types = FALSE)
seabird_100_subset <- seabird_100[, c("Cruise", "Event Number", "Count Minute (min)", 
                                      "Species", "Number")]
seabird_95 <- read_csv(here::here("data/table_95.csv"),show_col_types = FALSE)

seabird_header <- read_csv(here::here("data/Cruise_Transect_Header_merged_withfixes.csv"),show_col_types = FALSE)
seabird_header <- seabird_header[, c("Cruise", "Event Number", "Latitude Start", "Longitude Start")]

seabird_header <- seabird_header %>%
  mutate(`Event Number` = ifelse(is.na(`Event Number`) | !grepl("^\\d+$", `Event Number`), NA, as.numeric(`Event Number`)))


# Convert Cruise column in df_latlong to character
seabird_header <- seabird_header %>%
  mutate(Cruise = as.character(Cruise))
seabird_header <- seabird_header %>%
  mutate(`Event Number` = as.character(`Event Number`))
print(seabird_header)


## ---------------------------------------------------------------------------
# prep data
## ---------------------------------------------------------------------------
# line up lat/long start to observations dataframe
df_merged <- seabird_100_subset %>%
  left_join(seabird_header, by = c("Cruise", "Event Number"), relationship = "many-to-many")

## rename the cruise numbers to actual years
df_merged$Cruise <- factor(df_merged$Cruise, levels = unique(df_merged$Cruise))
levels(df_merged$Cruise) <- c("1993","1994", "1995", "1996", "1997", "1998","1999", 
                                      "2000", "2001", "2002", "2003","2004","2005","2006",
                                      "2007","2008","2009","2010","2011","2012","2013",
                                      "2014","2015","2016","2017","2018","2019","2021",
                                      "2022","2023","2024")

# manually specify bird species to include
# would it be better to exclude certain spp by identifying a pattern?
# species_of_interest <- c("SPSK", "WISP", "ADPE", "ANTE", "GIPE", "BBAL", "LMSA", "GHAL",
#                          "CAPE", "SOFU", "GEPE", "ANPR", "BLPE", "BBSP", "CHPE", "KEGU",
#                          "BESH", "UNSK", "ROAL", "WCPE", "UNPE", "DIPE", "UNSE", "SNPE",
#                          "ANPE", "BRSK", "ARTE", "UNAL", "EMPE", "TBPR", "FAPR", "MAPE",
#                          "WAAL", "UNPR", "BNSW", "UNSP", "SOAL", "UNTE", "TPPR", "AMSH",
#                          "SGPE", "SPSK", "GIPR")

species_of_interest <- c("GEPE","ADPE","CHPE")

# filter to include only species of interest
df_merged <- df_merged[df_merged$Species %in% species_of_interest, ]

# write_csv(df_merged, "merged_clean_df.csv")


df_merged$`Longitude Start` <- as.numeric(df_merged$`Longitude Start`)
df_merged$`Latitude Start` <- as.numeric(df_merged$`Latitude Start`)
df_merged$Number <- as.numeric(df_merged$Number)

# clean up data
# convert empty strings to NA (if applicable)
df_merged$`Longitude Start` <- as.numeric(ifelse(df_merged$`Longitude Start` == "", NA, df_merged$`Longitude Start`))
df_merged$`Latitude Start` <- as.numeric(ifelse(df_merged$`Latitude Start` == "", NA, df_merged$`Latitude Start`))

# remove rows with invalid coordinates or zeros
df_merged_clean <- df_merged[!(is.na(df_merged$`Longitude Start`) | is.na(df_merged$`Latitude Start`) |
                                 df_merged$`Longitude Start` == 0 | df_merged$`Latitude Start` == 0), ]

# subset the data - can specify certain spp here too if needed
species_of_interest <- df_merged_clean$Species
uniquespp_subset <- df_merged_clean[!is.null(df_merged_clean$Species) & df_merged_clean$Species %in% species_of_interest, ]
# uniquespp_subset <- df_merged[df_merged$Species %in% species_of_interest, ]



## ---------------------------------------------------------------------------
# Lat Lon - each unique species (presence/absence or all records are a dot) 
## ---------------------------------------------------------------------------

# determine number of unique species for palette
num_species <- length(unique(df_merged$Species))

# create a color palette with enough distinct colors
palette <- scales::hue_pal()(num_species)

# plotting presence of species with expanded color palette
ggplot(df_merged, aes(x = `Longitude Start`, y = `Latitude Start`, color = Species)) +
  geom_point() +
  scale_color_manual(values = palette) +
  labs(x = "Longitude", y = "Latitude", title = "Presence of Species") +
  theme_minimal()



## ---------------------------------------------------------------------------
# Lat Lon each species - size of circle is # of individuals
## ---------------------------------------------------------------------------

# plot presence of species with size representation based on ind count
ggplot(df_merged, aes(x = `Longitude Start`, y = `Latitude Start`, color = Species, size = Number)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = palette) +
  scale_size_continuous(range = c(1, 10)) +  
  labs(x = "Longitude", y = "Latitude", 
  title = "Species Distribution with Size by Number of Individuals") +
  facet_wrap(~ Cruise, nrow = 4) +  # facet by Cruise with 4 plots per row
  theme_minimal()


## ---------------------------------------------------------------------------
# Lat Lon each species - color circle by year OR separate plot for each year
## ---------------------------------------------------------------------------


# plot presence of species with size representation based on ind count
ggplot(df_merged, aes(x = `Longitude Start`, y = `Latitude Start`, color = Species, size = Number)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = palette) +
  scale_size_continuous(range = c(1, 10)) +  
  labs(x = "Longitude", y = "Latitude", 
       title = "Species Distribution with Size by Number of Individuals") +
  theme_minimal()










## ---------------------------------------------------------------------------
# Lat Lon - each unique species (presence/absence or all records are a dot) 
## ---------------------------------------------------------------------------
# plot lat/long
plot(uniquespp_subset$`Longitude Start`, uniquespp_subset$`Latitude Start`, 
     pch=19, col=as.factor(uniquespp_subset$Species), 
     xlab="Longitude", ylab="Latitude", main="Distribution of Bird Species")

# add counts
# for (i in 1:nrow(uniquespp_subset)) {
#   text(uniquespp_subset[i, "Longitude Start"], uniquespp_subset[i, "Latitude Start"], 
#        labels = uniquespp_subset[i, "Number"], pos = 4, cex = 0.7, col = "black")
# }

# add a legend for species
legend("topright", legend=unique(uniquespp_subset$Species), pch=19, 
       col=as.factor(unique(uniquespp_subset$Species)), cex=0.4, text.width=0.5,
       inset=c(0, 0), xpd=TRUE)



## ---------------------------------------------------------------------------
# Lat Lon each species - size of circle is # of individuals
## ---------------------------------------------------------------------------

# remove rows with invalid coordinates or zeros
df_merged_clean <- df_merged[!(is.na(df_merged$`Longitude Start`) | is.na(df_merged$`Latitude Start`) |
                                 df_merged$`Longitude Start` == 0 | df_merged$`Latitude Start` == 0), ]

# subset for spp of interest, or all
# species_of_interest <- df_merged_clean$Species
species_of_interest <-  c("ADPE", "CHPE", "GEPE")  
uniquespp_subset <- df_merged_clean[df_merged_clean$Species %in% species_of_interest, ]

# calculate total counts per species over the years
species_counts <- table(uniquespp_subset$Species)

# color palette for the gradient based on counts
color_palette <- colorRampPalette(c("lightblue", "darkblue"))(length(unique(uniquespp_subset$Species)))

# plot lat/long with varying point sizes and colors based on counts
plot(uniquespp_subset$`Longitude Start`, uniquespp_subset$`Latitude Start`, 
     pch=19, cex=log10(species_counts[as.character(uniquespp_subset$Species)] + 1),
     col=color_palette[as.factor(uniquespp_subset$Species)],
     xlab="Longitude", ylab="Latitude", main="Distribution of Penguins")

# add legend
legend("topright", legend=unique(uniquespp_subset$Species), 
       pch=19, col=color_palette, 
       cex=0.8, text.width=0.5, inset=c(0.0, 0.0), xpd=TRUE)



## ---------------------------------------------------------------------------
# Lat Lon each species - color circle by year OR separate plot for each year
## ---------------------------------------------------------------------------

# remove rows with invalid coordinates or zeros
df_merged_clean <- df_merged %>%
  filter(!is.na(`Longitude Start`) & !is.na(`Latitude Start`) &
           `Longitude Start` != 0 & `Latitude Start` != 0)

# calculate species counts per year
species_counts <- df_merged_clean %>%
  group_by(Cruise, Species) %>%
  summarise(count = n(), .groups = "drop") %>%
  ungroup()

# Merge species counts back to the main dataframe
df_merged_clean <- left_join(df_merged_clean, species_counts, by = c("Cruise", "Species"))

# create a ggplot object
p <- ggplot(df_merged_clean, aes(x = `Longitude Start`, y = `Latitude Start`, 
                                 color = as.factor(Species), size = log10(count + 1))) +
  geom_point() +
  scale_color_brewer(type = "qual", palette = "Set1") +
  labs(x = "Longitude", y = "Latitude", title = "Distribution of Bird Species") +
  theme_minimal() +
  facet_wrap(~ Cruise, ncol = 3)
print(p)



## ---------------------------------------------------------------------------
### input bathymetry and raster information for map
## ---------------------------------------------------------------------------
# Define CRS
crs <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

# Load and prepare data
bathy <- rast(here::here("data/IBCSO_v2_bed.nc"))
coast <- vect(here::here("data/add_coastline_medium_res_polygon_v7_8.shp"))

# Project data
projCoast <- project(coast, crs)
projBathy <- project(bathy, crs)

# Crop and mask bathymetry
cropBathy <- crop(projBathy, ext(-85, -55, -72, -60))
maskBathy <- mask(cropBathy, projCoast, inverse = TRUE)

# Set color breaks for bathymetry
h <- hist(values(maskBathy * -1), 100, plot=FALSE)
breaks <- h$breaks
n <- length(breaks) - 1

# # Define color palette
# colors <- cmocean("dense", direction = -1)
# cols <- colors(n)

# filter for specific spp
species_name <- "GEPE"
df_merged_spp <- df_merged %>%
  filter(Species == species_name) %>%
  dplyr::select(Species, Cruise, Number, `Longitude Start`, `Latitude Start`)

df_merged_spp$`Longitude Start` <- as.numeric(as.character(df_merged_spp$`Longitude Start`))
df_merged_spp$`Latitude Start` <- as.numeric(as.character(df_merged_spp$`Latitude Start`))
df_merged_spp$Number <- as.numeric(as.character(df_merged_spp$Number))

gepe <- ggplot() +
  geom_spatraster(data = maskBathy * -1, show.legend = FALSE) +
  scale_fill_gradient(low = "#c6e2ff", high = "#003366", name = "Depth (m)") +
  theme_classic(base_size = 14) +
  scale_x_continuous(limits = c(-80, -59), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-70.25, -63), expand = c(0, 0)) +
  geom_sf(data = projCoast, fill = "grey", col = "black") +
  geom_point(data = df_merged_spp,
             aes(x = `Longitude Start`, y = `Latitude Start`,
                 size = Number, color = Number), alpha = 0.7) +
  scale_size_continuous(name = "Count", range = c(2, 10), guide = "none") +
  scale_color_gradient(low = "red", high = "darkred", name = "Number") +
  facet_wrap(~ Cruise) +  # facet by cruise
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "right",
        strip.text = element_text(size = 10),  # adjust facet label size
        strip.background = element_rect(fill = "grey90")) + # adjust facet label background
  ggtitle("Counts of GEPE by Location and Year")

print(gepe)

# ggsave(filename = "gepe-facet-dist.png", plot = gepe,width = 20, height = 15, 
#   units = "in",dpi = 300)


# Calculate zoom limits based on data lat/long
# x_min <- min(df_merged_spp$`Longitude Start`) - 0.5
# x_max <- max(df_merged_spp$`Longitude Start`) + 0.5
# y_min <- min(df_merged_spp$`Latitude Start`) - 0.5
# y_max <- max(df_merged_spp$`Latitude Start`) + 0.5
# 
# # Plot
# ggplot() +
#   geom_spatraster(data = maskBathy * -1, show.legend = FALSE) +
#   scale_fill_gradient(low = "#c6e2ff", high = "#003366", name = "Depth (m)") +
#   theme_classic(base_size = 14) +
#   geom_sf(data = projCoast, fill = "grey", col = "black") +
#   geom_point(data = df_merged_spp,
#              aes(x = `Longitude Start`, y = `Latitude Start`,
#                  size = Number, color = Number), alpha = 0.7) +
#   scale_size_continuous(name = "Species Count", range = c(2, 10), guide = "none") +  # Update size legend name
#   scale_color_gradient(low = "red", high = "darkred", name = "Species Density") +  # Update color legend name
#   facet_wrap(~ Cruise) +
#   ggtitle("Density of Species by Location and Cruise") +  # Add title
#   coord_sf(xlim = c(x_min, x_max), ylim = c(y_min, y_max), expand = FALSE) +  # Zoom in using coord_sf
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         legend.position = "right",
#         strip.text = element_text(size = 12), 
#         strip.background = element_rect(fill = "grey90"),
#         plot.title = element_text(size = 16, face = "bold", hjust = 0.5))  # Customize title appearance


