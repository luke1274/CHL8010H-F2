library(here)
library(plm)
library(mice)
library(texreg)
library(here)
library(boot)
library(dplyr)
set.seed(2024)
data <- source("Armed Conflict/R/merge_datasets.R")

data <- as.data.frame(data)




data2017_IM <- data |>
  dplyr::filter(value.year == 2017) |>
  dplyr::filter(!is.na(value.Infant_Mortality_Rate)) 

data2017_U5 <- data |>
  dplyr::filter(value.year == 2017) |>
  dplyr::filter(!is.na(value.Under5_Mortality_Rate))

data2017_NM <- data |>
  dplyr::filter(value.year == 2017) |>
  dplyr::filter(!is.na(value.NeoNatal_Mortality_Rate)) 





getmeddiffIM <- function(data, indices) {
  sample_data <- data[indices, ]
  group_meds <- tapply(sample_data$value.Infant_Mortality_Rate, sample_data$value.conflict, FUN = function(x) median(x, na.rm = TRUE))
  meddiff <- group_meds[2] - group_meds[1]
  return(meddiff)
}

getmeddiffU5 <- function(data, indices) {
  sample_data <- data[indices, ]
  group_meds <- tapply(sample_data$value.Under5_Mortality_Rate, sample_data$value.conflict, FUN = function(x) median(x, na.rm = TRUE))
  meddiff <- group_meds[2] - group_meds[1]
  return(meddiff)
}

getmeddiffNM <- function(data, indices) {
  sample_data <- data[indices, ]
  group_meds <- tapply(sample_data$value.NeoNatal_Mortality_Rate, sample_data$value.conflict, FUN = function(x) median(x, na.rm = TRUE))
  meddiff <- group_meds[2] - group_meds[1]
  return(meddiff)
}


bootoutIM <- boot(data2017_IM, statistic = getmeddiffIM, strata = data2017_IM$value.conflict, R = 1000)
bootoutU5 <- boot(data2017_U5, statistic = getmeddiffU5, strata = data2017_U5$value.conflict, R = 1000)
bootoutNM <- boot(data2017_NM, statistic = getmeddiffNM, strata = data2017_NM$value.conflict, R = 1000)

print(bootoutIM)



#CI
boot.ci(boot.out = bootoutIM, conf = 0.95, type = c("basic", "perc", "bca"))
boot.ci(boot.out = bootoutU5, conf = 0.95, type = c("basic", "perc", "bca"))
boot.ci(boot.out = bootoutNM, conf = 0.95, type = c("basic", "perc", "bca"))