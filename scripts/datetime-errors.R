rm(list=ls())

# read-csv
library(tidyverse)
seabird_95_header <- read_csv(here::here("data/Cruise_Transect_Header_merged.csv"))
seabird_95_header <- seabird_95_header[, -c(15:35)]

# renaming column headers
colnames(seabird_95_header) <- c("study","cruisename","year-month","stationstart", 
                          "stationend","eventnumber", "day-time", "total-time", 
                          "shipspeed", "shipcourse", "latstart", "longstart", 
                          "latend", "longend")

# seabird_95 %>% 
#   select(`year-month, `day-time`)

# cleaning-dates-times

library(lubridate)
lter_datetime <- function(ym, dhm) {
  year2 <- as.numeric(substr(ym, 1, 2))
  year4 <- ifelse(year2 > 90, 1900 + year2, 2000 + year2)
  dhm6 <- ifelse(str_length(dhm) == 5, paste0("0", dhm), dhm)
  ymd_hm(paste0(year4, substr(ym, 3, 4), dhm6), tz = "UTC")
}
seabird_95_datetime <- seabird_95_header %>% 
#  mutate(sighting_utc = lter_datetime(`year-month`, `day-time`))
  mutate(sighting_utc = lter_datetime(`study`, `day-time`))
#sighting_utc = ymd_hm(paste(`Year / Month`, `Day / Time`), tz = "UTC"))

# datetime-errors

seabird_95_datetime %>% 
#  select(`eventnumber`, `year-month`, `day-time`, sighting_utc) %>% 
  select(`eventnumber`, `study`, `day-time`, sighting_utc) %>% 
  filter(is.na(sighting_utc)) %>% 
  DT::datatable()

# write.csv(seabird_95_datetime, "datetime-fixes.csv", row.names = FALSE)

# 3 digits for julian day, 2 digits for hour, 2 digits for min

