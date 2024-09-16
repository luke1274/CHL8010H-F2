library(dplyr)

maternal_data <- read.csv("original/maternalmortality.csv")
maternal_data_filtered <- maternal_data %>% select(Country.Name, X2000:X2019) 
