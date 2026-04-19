library(tidyverse)

Clean_Up_Dates <- Outcome_Categories_Global_Protest %>%
  mutate(duration_days = case_when(
    str_detect(Duration, regex("active", ignore_case = TRUE)) ~ "Active",
    str_detect(Duration, regex("^1 day", ignore_case = TRUE)) ~ "1",
    str_detect(Duration, regex("^2 days", ignore_case = TRUE)) ~ "2",
    str_detect(Duration, regex("^3 days", ignore_case = TRUE)) ~ "3",
    str_detect(Duration, regex("^4 days", ignore_case = TRUE)) ~ "4",
    str_detect(Duration, regex("^5 days", ignore_case = TRUE)) ~ "5",
    str_detect(Duration, regex("^6 days", ignore_case = TRUE)) ~ "6",
    str_detect(Duration, regex("^8 days", ignore_case = TRUE)) ~ "8",
    str_detect(Duration, regex("^9 days", ignore_case = TRUE)) ~ "9",
    str_detect(Duration, regex("^10 days", ignore_case = TRUE)) ~ "10",
    str_detect(Duration, regex("^11 days", ignore_case = TRUE)) ~ "11",
    str_detect(Duration, regex("^55 days", ignore_case = TRUE)) ~ "55",
    str_detect(Duration, regex("^1 week", ignore_case = TRUE)) ~ "7",
    str_detect(Duration, regex("^2 weeks", ignore_case = TRUE)) ~ "14",
    str_detect(Duration, regex("^2.5 weeks", ignore_case = TRUE)) ~ "18",
    str_detect(Duration, regex("^3 weeks", ignore_case = TRUE)) ~ "21",
    str_detect(Duration, regex("^4 weeks", ignore_case = TRUE)) ~ "28",
    str_detect(Duration, regex("^5 weeks", ignore_case = TRUE)) ~ "35",
    str_detect(Duration, regex("^6 weeks", ignore_case = TRUE)) ~ "42",
    str_detect(Duration, regex("^1 month", ignore_case = TRUE)) ~ "30",
    str_detect(Duration, regex("^2 months", ignore_case = TRUE)) ~ "60",
    str_detect(Duration, regex("^3 months", ignore_case = TRUE)) ~ "90",
    str_detect(Duration, regex("^3.5 months", ignore_case = TRUE)) ~ "105",
    str_detect(Duration, regex("^4 months", ignore_case = TRUE)) ~ "120",
    str_detect(Duration, regex("^4.5 months", ignore_case = TRUE)) ~ "135",
    str_detect(Duration, regex("^5 months", ignore_case = TRUE)) ~ "150",
    str_detect(Duration, regex("^6 months", ignore_case = TRUE)) ~ "180",
    str_detect(Duration, regex("^7 months", ignore_case = TRUE)) ~ "210",
    str_detect(Duration, regex("^8 months", ignore_case = TRUE)) ~ "240",
    str_detect(Duration, regex("^9 months", ignore_case = TRUE)) ~ "270",
    str_detect(Duration, regex("^10 months", ignore_case = TRUE)) ~ "300",
    str_detect(Duration, regex("^11 months", ignore_case = TRUE)) ~ "330",
    str_detect(Duration, regex("^13 months", ignore_case = TRUE)) ~ "390",
    str_detect(Duration, regex("^1 year, 3 months", ignore_case = TRUE)) ~ "450",
    str_detect(Duration, regex("^14 months", ignore_case = TRUE)) ~ "420",
    str_detect(Duration, regex("^15 months", ignore_case = TRUE)) ~ "450",
    str_detect(Duration, regex("^16 months", ignore_case = TRUE)) ~ "480",
    str_detect(Duration, regex("^1 year", ignore_case = TRUE)) ~ "360",
    str_detect(Duration, regex("^1.5 year", ignore_case = TRUE)) ~ "540",
    str_detect(Duration, regex("^2 year", ignore_case = TRUE)) ~ "720",
    str_detect(Duration, regex("^2.5 year", ignore_case = TRUE)) ~ "900",
    str_detect(Duration, regex("^3 year", ignore_case = TRUE)) ~ "1080",
    str_detect(Duration, regex("^4 year", ignore_case = TRUE)) ~ "1440",
    str_detect(Duration, regex("^1-2 weeks", ignore_case = TRUE)) ~ "18",
    str_detect(Duration, regex("^ten days", ignore_case = TRUE)) ~ "10",
    str_detect(Duration, regex("^two weeks", ignore_case = TRUE)) ~ "14",
    TRUE ~ NA_character_
  ))


No_Dates <- Clean_Up_Dates %>%
  filter(is.na(duration_days)) %>%
  select(Duration)