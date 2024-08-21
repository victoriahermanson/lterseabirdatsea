rm(list=ls())


library(ggplot2)
library(tidyverse)
seabird_100 <- read_csv(here::here("data/Cruise_Transect_Observations_merged.csv"),show_col_types = FALSE)

# convert 'Cruise' column to factor and rename levels
seabird_100$Cruise <- factor(seabird_100$Cruise, levels = unique(seabird_100$Cruise))
levels(seabird_100$Cruise) <- c("1993","1994", "1995", "1996", "1997", "1998","1999", 
                                "2000", "2001", "2002", "2003","2004","2005","2006",
                                "2007","2008","2009","2010","2011","2012","2013",
                                "2014","2015","2016","2017","2018","2019","2021",
                                "2022","2023","2024")
print(head(seabird_100))

##-----------------------------------------------------------------------------
# make a table of all unique species seen in the survey and their breeding season info
# are all the birds we see breeding during the LTER cruise
# are they raising chicks/incubating eggs etc. or not breeding at all
##-----------------------------------------------------------------------------
# manually specify bird species to include
# would it be better to exclude certain spp by identifying a pattern?
bird_species <- c("SPSK", "WISP", "ADPE", "ANTE","GIPE","BBAL","LMSA","GHAL",
                  "CAPE","SOFU","GEPE","ANPR","BLPE","BBSP","CHPE","KEGU","BESH",
                  "UNSK","ROAL","WCPE","UNPE","DIPE","UNSE","SNPE","ANPE",
                  "BRSK","ARTE","UNAL","EMPE","TBPR","FAPR","MAPE","WAAL",
                  "UNPR","BNSW","UNSP","SOAL","UNTE","TPPR","AMSH","SGPE",
                  "SPSK","GIPR")

# filter the dataset to include only the bird species
bird_data <- seabird_100 %>%
  filter(Species %in% bird_species)

# get unique bird species seen overall
unique_bird_species <- unique(bird_data$Species)

# create table of unique bird species seen overall
unique_species_table <- data.frame(Species = unique_bird_species)
print(unique_species_table)

# write to csv
# write.csv(unique_species_table, "birds_breedinginfo.csv", row.names = FALSE)
##-----------------------------------------------------------------------------
# standardize data to use for EOF analysis
##-----------------------------------------------------------------------------
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


## isolating one species time series plot
# species_of_interest <- "CHPE"  
# 
# # subset data for the species of interest
# species_data <- peng_df[peng_df$Group.2 == species_of_interest, ]


mean_species <- colMeans(peng_df[, -1])
var_species <- apply(peng_df[, -1], 2, var)
# Plot time series of anomalies in relation to original points
par(mfrow = c(3, 1), mar = c(4, 4, 2, 1))  # Set up multi-panel plot
for (i in 2:ncol(peng_df)) {
  plot(peng_df$Year, peng_df[, i], type = "l", col = "blue", 
       xlab = "Year", ylab = "Abundance Count",
       main = paste("Species", i - 1), ylim = range(c(peng_df[, -1], mean_species - 2 * sqrt(var_species), mean_species + 2 * sqrt(var_species))))
  abline(h = mean_species[i - 1], col = "red", lty = 2)  # Add mean line
  lines(peng_df$Year, anomalies[, i - 1] + mean_species[i - 1], type = "l", col = "green")  # Plot anomalies
  legend("topright", legend = c("Original", "Mean", "Anomalies"), col = c("blue", "red", "green"), lty = c(1, 2, 1))
}

# Subtract the mean from each value in each vector
data_stand <- data_stand %>%
  mutate_all(funs(. - mean(.)))


#-----------------------------------------------------------------------------
## compute variance and covariance for each species count column (vector)
# variances on diagonals
# covariances on off diagonals
var1 <- var(merged_df$x_df1)
var2 <- var(merged_df$x_df2)
var3 <- var(merged_df$x)
cov_12 <- cov(merged_df$x_df1, merged_df$x_df2)
cov_13 <- cov(merged_df$x_df1, merged_df$x)
cov_23 <- cov(merged_df$x_df2, merged_df$x)

## matrix for all 3 species
# create matrix to store variances and covariances
matrix_data_total <- matrix(NA, nrow = 3, ncol = 3)
#write.csv(matrix_data_total, file = "my_matrix.csv", row.names = FALSE)

# fill in the matrix
matrix_data_total[1, 1] <- var1
matrix_data_total[2, 2] <- var2
matrix_data_total[3, 3] <- var3
matrix_data_total[1, 2] <- cov_12
matrix_data_total[2, 1] <- cov_12
matrix_data_total[1, 3] <- cov_13
matrix_data_total[3, 1] <- cov_13
matrix_data_total[2, 3] <- cov_23
matrix_data_total[3, 2] <- cov_23
print(matrix_data_total)

e_values <- eigen(matrix_data_total)
print(e_values)

## calculate percentage variance of Modes
A_sum <- sum(3262.6151, 491.8924, 142.2814)
perc_var_modes <- e_values$values/(A_sum)

# plot set up
par(mfrow = c(1, 3), mar = c(5, 6, 4, 2) + 0.1)

# create a list of labels for each bar plot
bird_labels <- c("ADPE", "CHPE", "GEPE")

# plot eigenvectors for modes 1, 2, and 3
for (i in 1:3) {
  # Plot eigenvector
  barplot(e_values$vectors[, i], 
          xlim = c(-1, 1),
          main = paste("Mode", i),
          xlab = "Value",
          ylab = " Principal Component",
          horiz = TRUE,
          names.arg = bird_labels)  # Add labels for each bar
}
##-----------------------------------------------------------------------------
# plot year (x axis) vs. total # seen that year. A separate plot for each species
# You can do this in ggplot or run a for loop. 
# Plots dont need to be pretty, I just want to look at basic trends in time. 
# This should also help with your research. 
##-----------------------------------------------------------------------------

# aggregate data to get total sightings per cruise number (representing years) for each species
sightings_per_cruise <- bird_data %>%
  group_by(Species, Cruise) %>%
  summarise(total_sightings = n())

# convert CruiseNumber to factor with specific order
cruise_order <- c("9301", "9401", "9501", "9601", "9701", "9801", "9901", "0001", 
                  "0101", "0201", "0301", "0401", "0501", "0601", "0701", "0801", 
                  "0901", "1001", "1101", "1201", "1301", "1401", "1501", "1601", 
                  "1701", "1801")

sightings_per_cruise$Cruise <- factor(sightings_per_cruise$Cruise, levels = cruise_order)

# function to plot all species in facets
plot_all_species <- function() {
  p <- ggplot(sightings_per_cruise, aes(x = Cruise, y = total_sightings)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    labs(title = "Total Sightings by Species",
         x = "Cruise Number (Year)",
         y = "Total Sightings") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    facet_wrap(~ Species, scales = "free_y", ncol = 5)
  
  print(p)
}

# display all plots in one window
plot_all_species()

# save file
# ggsave("total_sightings_plot.png", plot = p, width = 12, height = 8, units = "in", dpi = 300)

## subset data to only include the spp counts per cruise year
subset_seabird_data <- sightings_per_cruise[sightings_per_cruise$Cruise %in% cruise_order, ]


#-------------------------------------------------------------------------------
# start EOF analysis
#-------------------------------------------------------------------------------

# compute anomalies
bird_data_anomalies <- subset_seabird_data %>%
  group_by(Species) %>%
  mutate(sightings_anomaly = total_sightings - mean(total_sightings))

# Sample dataframe
bird_data <- data.frame(
  species = bird_data_anomalies$Species,
  year = bird_data_anomalies$Cruise,
  count = bird_data_anomalies$total_sightings,
  anomalies = bird_data_anomalies$sightings_anomaly
)

# Compute the covariance matrix (S matrix)
S <- cov(numeric_data)
# Compute the correlation matrix (R matrix)
R <- cor(numeric_data)
# Compute eigenvalues and eigenvectors of S matrix
eig_S <- eigen(S)

# Extract eigenvalues and eigenvectors
eigenvalues <- eig_S$values
eigenvectors <- eig_S$vectors












