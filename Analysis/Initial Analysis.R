library(tidyverse)

Clean_Up_Dates <- read_csv("Data/final_clean_dataset.csv")

glimpse(Clean_Up_Dates)

sum(is.na(Clean_Up_Dates$duration_days))
sum(is.na(Clean_Up_Dates$peak_size_numeric))
sum(is.na(Clean_Up_Dates$freedom_status))
sum(is.na(Clean_Up_Dates$violent_response))
sum(is.na(Clean_Up_Dates$main_motivation))
sum(is.na(Clean_Up_Dates$year))

# Basic summaries
summary(Clean_Up_Dates$duration_days)
summary(Clean_Up_Dates$peak_size_numeric)
table(Clean_Up_Dates$freedom_status, useNA = "ifany")
table(Clean_Up_Dates$size_category_clean, useNA = "ifany")
table(Clean_Up_Dates$violent_response, useNA = "ifany")
table(Clean_Up_Dates$main_motivation, useNA = "ifany")

# Optional: if you want to exclude active protests from duration analysis
analysis_data <- Clean_Up_Dates %>%
  filter(duration_status == "Ended")

# Histogram
ggplot(analysis_data, aes(x = duration_days)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Distribution of Protest Duration",
    x = "Duration (days)",
    y = "Number of protests"
  )

# Boxplot for duration overall
ggplot(analysis_data, aes(y = duration_days)) +
  geom_boxplot() +
  labs(
    title = "Overall Distribution of Protest Duration",
    y = "Duration (days)"
  )

# Summary stats for duration
analysis_data %>%
  summarise(
    n_protests = n(),
    min_duration = min(duration_days, na.rm = TRUE),
    median_duration = median(duration_days, na.rm = TRUE),
    mean_duration = mean(duration_days, na.rm = TRUE),
    max_duration = max(duration_days, na.rm = TRUE)
  )

# DURATION BY PROTEST SIZE

# Boxplot
ggplot(analysis_data, aes(x = size_category_clean, y = duration_days)) +
  geom_boxplot() +
  labs(
    title = "Protest Duration by Protest Size",
    x = "Peak protest size",
    y = "Duration (days)"
  )

# Median duration by size category
duration_by_size <- analysis_data %>%
  group_by(size_category_clean) %>%
  summarise(
    n = n(),
    median_duration = median(duration_days, na.rm = TRUE),
    mean_duration = mean(duration_days, na.rm = TRUE)
  )

print(duration_by_size)

# Bar plot of median duration by size
ggplot(duration_by_size, aes(x = size_category_clean, y = median_duration)) +
  geom_col() +
  labs(
    title = "Median Protest Duration by Protest Size",
    x = "Peak protest size",
    y = "Median duration (days)"
  )

#DURATION BY FREEDOM STATUS

# Boxplot
ggplot(analysis_data, aes(x = freedom_status, y = duration_days)) +
  geom_boxplot() +
  labs(
    title = "Protest Duration by Freedom Status",
    x = "Freedom status",
    y = "Duration (days)"
  )

# Summary table
duration_by_freedom <- analysis_data %>%
  group_by(freedom_status) %>%
  summarise(
    n = n(),
    median_duration = median(duration_days, na.rm = TRUE),
    mean_duration = mean(duration_days, na.rm = TRUE)
  )

print(duration_by_freedom)

# Bar plot of median duration by freedom status
ggplot(duration_by_freedom, aes(x = freedom_status, y = median_duration)) +
  geom_col() +
  labs(
    title = "Median Protest Duration by Freedom Status",
    x = "Freedom status",
    y = "Median duration (days)"
  )

#DURATION BY VIOLENT RESPONSE

# Boxplot
ggplot(analysis_data, aes(x = violent_response, y = duration_days)) +
  geom_boxplot() +
  labs(
    title = "Protest Duration by Violent Government Response",
    x = "Violent government response",
    y = "Duration (days)"
  )

# Summary table
duration_by_violence <- analysis_data %>%
  group_by(violent_response) %>%
  summarise(
    n = n(),
    median_duration = median(duration_days, na.rm = TRUE),
    mean_duration = mean(duration_days, na.rm = TRUE)
  )

print(duration_by_violence)

# Bar plot of median duration by violence
ggplot(duration_by_violence, aes(x = violent_response, y = median_duration)) +
  geom_col() +
  labs(
    title = "Median Protest Duration by Violent Government Response",
    x = "Violent government response",
    y = "Median duration (days)"
  )

#DURATION BY MAIN MOTIVATION

# Boxplot
ggplot(analysis_data, aes(x = main_motivation, y = duration_days)) +
  geom_boxplot() +
  labs(
    title = "Protest Duration by Main Motivation",
    x = "Main motivation",
    y = "Duration (days)"
  )

# Summary table
duration_by_motivation <- analysis_data %>%
  group_by(main_motivation) %>%
  summarise(
    n = n(),
    median_duration = median(duration_days, na.rm = TRUE),
    mean_duration = mean(duration_days, na.rm = TRUE)
  )

print(duration_by_motivation)


#DURATION BY OUTCOME CATEGORY

# Boxplot
ggplot(analysis_data, aes(x = protest_outcome_category, y = duration_days)) +
  geom_boxplot() +
  labs(
    title = "Protest Duration by Outcome Category",
    x = "Outcome category",
    y = "Duration (days)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Summary table
duration_by_outcome <- analysis_data %>%
  group_by(protest_outcome_category) %>%
  summarise(
    n = n(),
    median_duration = median(duration_days, na.rm = TRUE),
    mean_duration = mean(duration_days, na.rm = TRUE)
  )

print(duration_by_outcome)


#DURATION OVER TIME

# Median duration by year
duration_by_year <- analysis_data %>%
  group_by(year) %>%
  summarise(
    n = n(),
    median_duration = median(duration_days, na.rm = TRUE),
    mean_duration = mean(duration_days, na.rm = TRUE)
  )

print(duration_by_year)

# Line chart
ggplot(duration_by_year, aes(x = year, y = median_duration)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Median Protest Duration Over Time",
    x = "Year",
    y = "Median duration (days)"
  )


# SAVE SUMMARY TABLES

write_csv(duration_by_size, "Data/duration_by_size.csv")
write_csv(duration_by_freedom, "Data/duration_by_freedom.csv")
write_csv(duration_by_violence, "Data/duration_by_violence.csv")
write_csv(duration_by_motivation, "Data/duration_by_motivation.csv")
write_csv(duration_by_outcome, "Data/duration_by_outcome.csv")
write_csv(duration_by_year, "Data/duration_by_year.csv")


# Notes 
# Most protests are pretty short (median 7 days)
# Larger protests tend to last longer than smaller protests
# Protests that receive a violent government response tend to last longer 
# Protests duration varies just slightly across regime types, with protests in "Not Free" countries lasting a bit longer on average than those in "Free" or "Partly Free" countries
# No clear relationship between protest motivation and duration

