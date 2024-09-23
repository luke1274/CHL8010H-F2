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


#Week 3
conflict_data <- read.csv("Armed Conflict/original/conflictdata.csv")

conflict_data_bin <- conflict_data
conflict_data_bin <- subset(conflict_data_bin, select = -c(conflict_id)) #Get rid of conflict_id

conflict_data_bin <- conflict_data_bin %>% group_by(year, ISO) %>% 
  summarize(
    best = sum(best, na.rm = TRUE)
  ) %>% mutate(conflict = ifelse(best >= 25, 1, 0))

# DONT RUN THIS AGAIN
conflict_data_bin <- conflict_data_bin %>% mutate(year = year + 1)

