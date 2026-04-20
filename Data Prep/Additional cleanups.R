library(tidyverse)

Global_Protests <- read_csv("Data Prep/Global Protest Tracker - View Data.csv")

# Clean up the year so we can see trends over time 
Clean_Up_Dates <- Clean_Up_Dates %>%
  mutate(
    year_short = stringr::str_extract(`Start Month-Year`, "\\d{2}"),
    
    year = case_when(
      is.na(year_short) ~ NA_real_,
      as.numeric(year_short) <= 26 ~ 2000 + as.numeric(year_short),  # 00–26 → 2000–2026
      TRUE ~ 1900 + as.numeric(year_short)  # fallback (not really needed here)
    )
  ) %>%
  select(-year_short)

# Violent response (binary clean)
Clean_Up_Dates <- Clean_Up_Dates %>%
  mutate(
    violent_response = case_when(
      `Violent government response` == "X" ~ TRUE,
      is.na(`Violent government response`) ~ FALSE
    )
  )
# Motivation
Clean_Up_Dates <- Clean_Up_Dates %>%
  mutate(
    main_motivation = case_when(
      str_detect(`Economic motivation?`, "X") ~ "Economic",
      str_detect(`Political motivation?`, "X") ~ "Political",
      str_detect(`Corruption motivation?`, "X") ~ "Corruption",
      str_detect(`Gender motivation?`, "X") ~ "Gender",
      TRUE ~ "Other"
    )
  )

# ordering freedom status 
Clean_Up_Dates <- Clean_Up_Dates %>%
  mutate(
    freedom_status = case_when(
      str_detect(freedom_rating_status, regex("^free$", ignore_case = TRUE)) ~ "Free",
      str_detect(freedom_rating_status, regex("^not free$", ignore_case = TRUE)) ~ "Not Free",
      str_detect(freedom_rating_status, regex("^partly free", ignore_case = TRUE)) ~ "Partly Free",
      TRUE ~ NA_character_
    ),
    
    freedom_status = factor(
      freedom_status,
      levels = c("Not Free", "Partly Free", "Free")
    )
  )

# creating a log version
## duration is highly skewed, so log makes patterns clearer in plots, we might not use this tho!!!! 
Clean_Up_Dates <- Clean_Up_Dates %>%
  mutate(
    log_duration = log(duration_days + 1))
    
# size is also skewed
Clean_Up_Dates <- Clean_Up_Dates %>%
  mutate(
    log_size = log(peak_size_numeric + 1)
  )

#check
sum(is.na(Clean_Up_Dates$duration_days))
sum(is.na(Clean_Up_Dates$peak_size_numeric))
sum(is.na(Clean_Up_Dates$year))

# cleaning up the names 
install.packages("janitor")
library(janitor)

Clean_Up_Dates <- Clean_Up_Dates %>%
  clean_names()

# categorizing duration for easier analysis
Clean_Up_Dates <- Clean_Up_Dates %>%
  mutate(
    duration_category = case_when(
      duration_days <= 7 ~ "Short",
      duration_days <= 30 ~ "Medium",
      duration_days <= 90 ~ "Long",
      duration_days > 90 ~ "Very Long"
    )
  )

summary(Clean_Up_Dates)

write_csv(Clean_Up_Dates, "Data/final_clean_dataset.csv")