library(dplyr)


# Set working directory to where maternal.R is located
a <- source("Maternal Mortality/R/maternal.R")

# Set working directory to where disaster.R is located
b <- source("Armed Conflict/R/disaster.R")

colnames(merged_data)
colnames(conflict_data_bin)