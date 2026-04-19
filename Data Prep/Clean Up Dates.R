library(tidyverse)

Clean_Up_Dates <- Outcome_Categories_Global_Protest %>%
  mutate(duration_days = case_when(
    str_detect(Duration, regex("active", ignore_case = TRUE)) ~ "Active",
    str_detect(Duration, regex("^1 day", ignore_case = TRUE)) ~ "1",
    str_detect(Duration, regex("^2 days", ignore_case = TRUE)) ~ "2",
    str_detect(Duration, regex("^3 days", ignore_case = TRUE)) ~ "3",
    str_detect(Duration, regex("^1 week", ignore_case = TRUE)) ~ "7",
    str_detect(Duration, regex("^2 weeks", ignore_case = TRUE)) ~ "14",
    str_detect(Duration, regex("^3 weeks", ignore_case = TRUE)) ~ "21",
    str_detect(Duration, regex("^1 month", ignore_case = TRUE)) ~ "30",
    str_detect(Duration, regex("^2 months", ignore_case = TRUE)) ~ "60",
    str_detect(Duration, regex("^3 months", ignore_case = TRUE)) ~ "90",
    str_detect(Duration, regex("^4 months", ignore_case = TRUE)) ~ "120",
    str_detect(Duration, regex("^6 months", ignore_case = TRUE)) ~ "180",
    str_detect(Duration, regex("^1 year, 3 months", ignore_case = TRUE)) ~ "450",
    str_detect(Duration, regex("^14 months", ignore_case = TRUE)) ~ "420",
    str_detect(Duration, regex("^15 months", ignore_case = TRUE)) ~ "450",
    TRUE ~ NA_character_
  ))


No_Dates <- Clean_Up_Dates %>%
  filter(is.na(duration_days)) %>%
  select(Duration)