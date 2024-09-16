library(dplyr)

maternal_data <- read.csv("original/maternalmortality.csv")
maternal_data_filtered <- disaster_data %>% filter(Year>= 2000 & Year <= 2019, Disaster.Type %in% c("Drought","Earthquake"))
