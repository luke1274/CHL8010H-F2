library(dplyr)
library(tidyr)

maternal_data <- read.csv("maternalmortality.csv")
maternal_data_filtered <- maternal_data %>% select(Country.Name, X2000:X2019) 

reshaped_data <- maternal_data_filtered %>%
  pivot_longer(
    cols = X2000:X2019,
    names_to = "Year",
    names_prefix = "X",
    values_to = "MatMor",
    values_drop_na = FALSE
  )

reshaped_data$Year <- as.numeric(reshaped_data$Year)

head(reshaped_data)
