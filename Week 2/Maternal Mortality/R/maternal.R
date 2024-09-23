library(dplyr)
library(tidyr)
library(purrr)
library(countrycode)

maternal_data <- read.csv("Maternal Mortality/original/maternalmortality.csv")
maternal_data_filtered <- maternal_data %>% select(Country.Name, X2000:X2019) 

reshaped_maternal_data <- maternal_data_filtered %>%
  pivot_longer(
    cols = X2000:X2019,
    names_to = "Year",
    names_prefix = "X",
    values_to = "MatMor",
    values_drop_na = FALSE
  )

reshaped_maternal_data$Year <- as.numeric(reshaped_maternal_data$Year)

head(reshaped_maternal_data)



#Week 3 Stuff
reshape_data <- function(x){
  data_filtered <- x %>% select(Country.Name, X2000:X2019)
  reshaped_data <- data_filtered %>%
    pivot_longer(
      cols = X2000:X2019,
      names_to = "Year",
      names_prefix = "X",
      values_to = "MatMor",
      values_drop_na = FALSE
    )
  reshaped_data$Year <- as.numeric(reshaped_data$Year)
  return(reshaped_data)
}

infant_data <- read.csv("Maternal Mortality/original/infantmortality.csv")
neonatal_data <- read.csv("Maternal Mortality/original/neonatalmortality.csv")
under5_data <- read.csv("Maternal Mortality/original/under5mortality.csv")

reshaped_infant_data <- reshape_data(infant_data)
reshaped_neonatal_data <- reshape_data(neonatal_data)
reshaped_under5_data <- reshape_data(under5_data)
reshaped_maternal_data <- reshape_data(maternal_data)

combined_data <- list(reshaped_infant_data, reshaped_neonatal_data, reshaped_under5_data, reshaped_maternal_data)

merged_data <- reduce(combined_data, full_join, by = c("Country.Name", "Year"))
merged_data <-
  merged_data %>% rename(
    year = Year,
    Infant_Mortality_Rate = MatMor.x,
    NeoNatal_Mortality_Rate = MatMor.y,
    Under5_Mortality_Rate = MatMor.x.x,
    Maternal_Mortality_Rate = MatMor.y.y
  )


merged_data$ISO <- countrycode(merged_data$Country.Name,
                                   origin = "country.name",
                                   destination = "iso3c")

merged_data <- subset(merged_data, select = -c(Country.Name)) #Getting rid of Country.Name

