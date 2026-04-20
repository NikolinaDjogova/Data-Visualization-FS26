library(tidyverse)

Global_Protests <- read_csv("Data Prep/Global Protest Tracker - View Data.csv")

Clean_Up_Dates <- Clean_Up_Dates %>%
  mutate(
    peak_size_clean = `Peak Size` %>%
      str_to_lower() %>%
      str_trim() %>%
      str_replace_all(",", "") %>%
      str_replace_all(">", "") %>%
      str_replace_all("<", "") %>%
      str_replace_all("unknown", NA_character_),
    
    peak_size_clean = case_when(
      peak_size_clean == "thousands" ~ "1000",
      peak_size_clean == "tens of thousands" ~ "10000",
      str_detect(peak_size_clean, "million") ~ str_replace_all(peak_size_clean, "million", "") %>% str_trim(),
      TRUE ~ peak_size_clean
    ),
    
    peak_size_numeric = case_when(
      str_detect(`Peak Size`, regex("million", ignore_case = TRUE)) ~ as.numeric(peak_size_clean) * 1000000,
      TRUE ~ as.numeric(peak_size_clean)
    ),
    
    size_category_clean = case_when(
      is.na(peak_size_numeric) ~ "Unknown",
      peak_size_numeric < 1000 ~ "<1k",
      peak_size_numeric < 10000 ~ "1k–10k",
      peak_size_numeric < 100000 ~ "10k–100k",
      peak_size_numeric < 1000000 ~ "100k–1M",
      peak_size_numeric >= 1000000 ~ "1M+"
    ),
    
    size_category_clean = factor(
      size_category_clean,
      levels = c("<1k", "1k–10k", "10k–100k", "100k–1M", "1M+", "Unknown")
    )
  ) %>%
  select(-peak_size_clean)

summary(Clean_Up_Dates$peak_size_numeric)
table(Clean_Up_Dates$size_category_clean, useNA = "ifany")