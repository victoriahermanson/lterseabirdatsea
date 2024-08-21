

#------------------------------------------------------------------------------------
## Read data files
#------------------------------------------------------------------------------------
lter_grid_csv      <- "data/LTERGrid2.csv"
at_sea_seabird_95_csv <- "data/table_95.csv"

# LTER cruise grid lat/longs
lter_grid <- read.csv(file=lter_grid_csv, header=FALSE, stringsAsFactors=TRUE)

# file 95 at sea seabird sightings
at_sea_95 <- read.csv(file=at_sea_seabird_95_csv, header=TRUE, stringsAsFactors=TRUE)

