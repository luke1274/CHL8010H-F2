library(dplyr)


# Set working directory to where maternal.R is located
a <- source("Maternal Mortality/R/maternal.R")

# Set working directory to where disaster.R is located
b <- source("Armed Conflict/R/disaster.R")

c <- source("Armed Conflict/R/conflict.R")

covariates <- read.csv("Armed Conflict/original/covariates.csv")

colnames(merged_data)
colnames(conflict_data_bin)
colnames(covariates)
colnames(subset_grouped)

final_merged_data <- merged_data %>%
  full_join(subset_grouped, by = c("ISO", "year")) %>%   # Merge with conflict data
  full_join(covariates, by = c("ISO", "year")) %>% 
  full_join(conflict_data_bin, by = c("ISO", "year"))  

