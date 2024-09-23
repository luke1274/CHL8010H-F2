library(dplyr)

disaster_data <- read.csv("Armed Conflict/original/disaster.csv")
disaster_data_filtered <- disaster_data %>% filter(Year>= 2000 & Year <= 2019, Disaster.Type %in% c("Drought","Earthquake"))

subset <- disaster_data_filtered %>% select(Year, ISO, Disaster.Type)


subset <- subset %>% mutate(
  drought = ifelse(Disaster.Type == "Drought", 1, 0),
  earthquake = ifelse(Disaster.Type == "Earthquake",1,0)
                                     )



subset_grouped <- subset %>%
  group_by(Year,ISO) %>% summarize(drought = max(drought), earthquake = max(earthquake))

subset_grouped <- subset_grouped %>% rename(
  year = Year)


#Week 3


# DONT RUN THIS AGAIN

