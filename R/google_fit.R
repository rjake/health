# get data from google takeout
# https://takeout.google.com/settings/takeout
# Takeout/Fit/Daily Aggregations/Daily Summaries.csv

library(tidyverse)

raw_fit <- 
  read.csv("input/Daily Summaries.csv")  %>%
  rename_all(
    ~tolower(.) %>% 
      str_replace_all("\\.+", "_") %>% 
      str_remove("_$")
  )

prep_fit <-
  raw_fit %>% 
  mutate(date = as.Date(date)) %>% 
  filter(date >= "2019-08-26") %>% 
  #select_if(~mean(is.na(.), na.rm = TRUE) < 0.1) %>% 
  mutate(
    distance_miles = distance_m / 1609,
    inactive_duration_hours = inactive_duration_ms / (1000 * 60 * 60),
    walking_duration_hours = walking_duration_ms / (1000 * 60 * 60),
    biking_duration_hours = biking_duration_ms / (1000 * 60 * 60),
    hiking_duration_hours = hiking_duration_ms / (1000 * 60 * 60)
  ) %>% 
  select(
    date,
    calories_burned = calories_kcal,
    steps = step_count,
    distance_miles,
    move_minutes_count,
    ends_with("hours")
  )

write_csv(prep_fit, "output/google_fit.csv")
