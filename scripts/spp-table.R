rm(list=ls())

install.packages("knitr")

library(knitr)

# read in file
spptable_df <- read_csv(here::here("data/LTERGrid2.csv"),show_col_types = FALSE)

# Create a LaTeX-formatted table
kable(spptable_df, format = "latex", align = c("l", "c", "c", "c"), 
      caption = "Summary of Results", booktabs = TRUE)

# Export to CSV
write.csv(spptable_df, file = "spptable_df.csv", row.names = FALSE)

# Export to Excel (requires 'openxlsx' package)
# install.packages("openxlsx")
library(openxlsx)
write.xlsx(spptable_df, file = "spptable_df.xlsx", rowNames = FALSE)