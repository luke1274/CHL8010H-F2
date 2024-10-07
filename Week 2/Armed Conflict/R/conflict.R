conflict_data <- read.csv("Armed Conflict/original/conflictdata.csv")

conflict_data_bin <- conflict_data
conflict_data_bin <- subset(conflict_data_bin, select = -c(conflict_id)) #Get rid of conflict_id

conflict_data_bin <- conflict_data_bin %>% group_by(year, ISO) %>% 
  mutate(year = year + 1) %>%
  summarize(
    best = sum(best, na.rm = TRUE)
  ) %>% mutate(conflict = ifelse(best >= 25, 1, 0))

conflict_data_bin