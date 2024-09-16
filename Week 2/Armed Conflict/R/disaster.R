library(dplyr)

disaster_data <- read.csv("original/disaster.csv")
disaster_set_filtered <- disaster_data %>% filter(Year>= 2000 & Year <= 2019, Disaster.Type %in% c("Drought","Earthquake"))

subset <- disaster_data %>% select(Year, ISO, Disaster.Type)


subset <- subset %>% mutate(
  drought = ifelse(DisasterTyoe == "Drought", 1, 0),
  earthquake = ifelse(Disaster.Type == "Earthquake,1,0")
                                     )