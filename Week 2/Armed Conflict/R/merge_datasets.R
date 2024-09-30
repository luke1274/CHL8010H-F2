library(dplyr)

#Ensure WKDir is set to directory where both Armed Conflict and Maternal Mortality directory are housed (Week 2)
a <- source("Maternal Mortality/R/maternal.R")

b <- source("Armed Conflict/R/disaster.R")

c <- source("Armed Conflict/R/conflict.R")

covariates <- read.csv("Armed Conflict/original/covariates.csv")

colnames(merged_data)
colnames(conflict_data_bin)
colnames(covariates)
colnames(subset_grouped)

final_merged_data <- merged_data %>%
  full_join(subset_grouped, by = c("ISO", "year")) %>%   # Merge with conflict data
  full_join(conflict_data_bin, by = c("ISO", "year"))%>% 
  left_join(covariates, select(ISO, year, country_names), by = c("ISO", "year"))


#Reorder the column names to be similar to the example in week4_eda_inclass.pdf
corrected_order <- c("country_name", "ISO", "region", "year", "gdp1000", 
                   "OECD", "OECD2023", "popdens", "urban", "agedep", 
                   "male_edu", "temp", "rainfall1000", "totdeath", "armconf1", 
                   "matmor", "infmor", "neomor", "un5mor", "drought", 
                   "earthquake")

#Replacing every instance of N/A in "earthquake" and "drought" with 0
finaldata <- finaldata %>%
  mutate(earthquake = replace_na(earthquake, 0),
         drought = replace_na(drought, 0))
#Remove every row with N/A country_name
finaldata <- finaldata %>%
  filter(!is.na(country_name))


finaldata_reordered <- finaldata %>%
  select(any_of(corrected_order), everything())
names(finaldata_reordered)


finaldata <- finaldata_reordered
finaldata %>%
  dplyr::filter(country_name == "Canada")

names(finaldata)

