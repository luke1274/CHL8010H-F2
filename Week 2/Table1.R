library(dplyr)
library(tidyr)

# Load the dataset
a <- source("Armed Conflict/R/merge_datasets.R")
df <- as.data.frame(a[[1]])

# Function to summarize counts based on year bins and conflict status
summarize_year_bins <- function(data, conflict_status_label) {
  data %>%
    mutate(year_bin = case_when(
      year >= 2000 & year <= 2004 ~ "2000-2004",
      year >= 2005 & year <= 2009 ~ "2005-2009",
      year >= 2010 & year <= 2014 ~ "2010-2014",
      year >= 2015 & year <= 2019 ~ "2015-2019",
      TRUE ~ NA_character_  # NA for years outside the range
    )) %>%
    filter(!is.na(year_bin)) %>%  # Remove rows with NA year bins
    group_by(year_bin) %>%
    summarise(count = n(), .groups = 'drop') %>%
    mutate(conflict_status = conflict_status_label) %>%
    mutate(column_name = paste(conflict_status, year_bin, sep = "_")) %>%
    select(column_name, count)
}

# Summarize for both conflict and no conflict
conflict_summary <- summarize_year_bins(df[df$conflict == 1, ], "Conflict")
noconflict_summary <- summarize_year_bins(df[df$conflict == 0, ], "No_Conflict")

# Combine the summaries into a single table
final_table <- full_join(conflict_summary, noconflict_summary, by = "column_name")
final_table[is.na(final_table)] <- 0

# Reshape the table to wide format by handling the `count_Yes` and `count_No` separately
final_table_wide <- final_table %>%
  pivot_wider(names_from = column_name, values_from = c(count_Yes, count_No), values_fill = 0)

# Add a single row called "N Countries"
final_table_wide <- final_table_wide %>%
  summarise(across(everything(), sum, na.rm = TRUE)) %>%
  mutate(N_Countries = "N Countries") %>%
  select(N_Countries, everything())

final_table_wide <- final_table_wide %>%
  select(where(~ any(. != 0)))


names(final_table_wide) <- c("","Yes Conflict 2000-2004", "Yes Conflict 2005-2009", "Yes Conflict 2010-2014", "Yes Conflict 2015-2019",
                             "No Conflict 2000-2004", "No Conflict 2005-2009", "No Conflict 2010-2014", "No Conflict 2015-2019")




#Earthquake
# Summarize for both conflict and no conflict
earthquake_conflict_summary <- c("Earthquake", sum(df$conflict == 1 & df$earthquake == 1 & df$year >= 2000 & df$year <= 2004),
                                 sum(df$conflict == 1 & df$earthquake == 1 & df$year >= 2005 & df$year <= 2009),
                                 sum(df$conflict == 1 & df$earthquake == 1 & df$year >= 2010 & df$year <= 2014),
                                 sum(df$conflict == 1 & df$earthquake == 1 & df$year >= 2015 & df$year <= 2019), 
                                 sum(df$conflict == 0 & df$earthquake == 1 & df$year >= 2000 & df$year <= 2004),
                                 sum(df$conflict == 0 & df$earthquake == 1 & df$year >= 2005 & df$year <= 2009),
                                 sum(df$conflict == 0 & df$earthquake == 1 & df$year >= 2010 & df$year <= 2014),
                                 sum(df$conflict == 0 & df$earthquake == 1 & df$year >= 2015 & df$year <= 2019))


final_table_wide <- rbind(final_table_wide, earthquake_conflict_summary)



