library(tidyverse)

Global_Protests <- read_csv("Data Prep/Global Protest Tracker - View Data.csv")

Clean_Up_Dates <- Global_Protests %>%
  mutate(
    # keep track of whether protest is still ongoing
    duration_status = case_when(
      str_detect(Duration, regex("active|ongoing|continuing", ignore_case = TRUE)) ~ "Active",
      is.na(Duration) ~ "No Data",
      TRUE ~ "Ended"
    ),
    # clean text a little
    Duration_clean = Duration %>%
      str_to_lower() %>%
      str_replace_all("–|—", "-") %>%
      str_replace_all(",", "") %>%
      str_trim(),
    # turn written numbers into digits
    Duration_clean = Duration_clean %>%
      str_replace_all("\\bone\\b", "1") %>%
      str_replace_all("\\btwo\\b", "2") %>%
      str_replace_all("\\bthree\\b", "3") %>%
      str_replace_all("\\bfour\\b", "4") %>%
      str_replace_all("\\bfive\\b", "5") %>%
      str_replace_all("\\bsix\\b", "6") %>%
      str_replace_all("\\bseven\\b", "7") %>%
      str_replace_all("\\beight\\b", "8") %>%
      str_replace_all("\\bnine\\b", "9") %>%
      str_replace_all("\\bten\\b", "10") %>%
      str_replace_all("\\beleven\\b", "11"),
    # extract first number
    duration_number = readr::parse_number(Duration_clean),
    
    # convert to days
    duration_days = case_when(
      duration_status == "Active" ~ NA_real_,
      str_detect(Duration_clean, "day") ~ duration_number,
      str_detect(Duration_clean, "week") ~ duration_number * 7,
      str_detect(Duration_clean, "month") ~ duration_number * 30,
      str_detect(Duration_clean, "year") ~ duration_number * 365,
      TRUE ~ NA_real_
    ) |> as.integer()
  ) %>%
  select(-Duration_clean, -duration_number)


#control mechanism to avoid dateless cases
No_Dates <- Clean_Up_Dates %>%
  filter(duration_status != "Active", is.na(duration_days)) %>%
  select(Country, `Protest Name`, Duration)

write_csv(Clean_Up_Dates, "Data/Clean_Up_Dates.csv")
write_csv(No_Dates, "Data/No_Dates_QA.csv")

View(Clean_Up_Dates)
