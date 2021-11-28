library(googlesheets)
library(tidyverse)
library(lubridate)

options(scipen = 999)

#read in data
#google sheet to read in
# https://docs.google.com/spreadsheets/u/0/?q=sleep%20as%20Android

google_key <- gs_key("1JmbjrQjrjv3bFwp5V_bQjOWo-9Mu_yqBCl6dsJMbvJM")

#get data from googlesheets
raw_sleep <-
  gs_read(google_key,
    ws = "Sleep as Android Spreadsheet",
    lookup = T,
    cell_cols(1:15),
    check.names = F
  ) %>%
  data.frame() %>%
  filter(Id != "Id")

# clean data
clean_sleep <-
  raw_sleep %>%
  set_names(tolower) %>%
  select(
    start_time = from, 
    end_time = to, 
    rating
  ) %>%
  # mutate(
  #   start_time = as.POSIXct(from, format = "%d. %m. %Y %H:%M"), 
  #   end_time = as.POSIXct(to, format = "%d. %m. %Y %H:%M")
  # )
  mutate_at(
    vars(ends_with("time")), 
    as.POSIXct, 
    format = "%d. %m. %Y %H:%M"
  ) %>% 
  filter(start_time >= "2019-08-25") %>% 
  filter(start_time != "2020-02-15 12:09:00") %>% 
  mutate(
    rating = as.numeric(rating),
    hour_start = hour(start_time),
    hour_end = hour(end_time),
    date_wake = floor_date(end_time, "day"),
    date_bedtime = date_wake %m-% days(1),
    date_char = as.character(date_bedtime),
    duration = 
      difftime(end_time, start_time, units = "hours") %>%
      as.numeric(),
    nap = duration < 3 & between(hour_start, 10, 19)
  )

count(clean_sleep, date_bedtime, sort = T)
table(clean_sleep$nap)


# consolidate duplicate records
agg_sleep <-
  clean_sleep %>%
  arrange(date_bedtime) %>% 
  group_by(date_bedtime, date_wake) %>%
  summarise(
    broken_sleep_ind = as.integer(n() > 1),
    bedtime = min(start_time[!nap]),
    wake = max(end_time[!nap]),
    nap_ind = max(nap),
    rating = max(rating)
  ) %>%
  ungroup()


# final output
final_sleep <-
  agg_sleep %>%
  mutate(
    duration_mins = difftime(wake, bedtime, units = "mins") %>%
      as.integer(),
    duration_hours = round(duration_mins / 60, 1),
    bedtime_hour = hour(bedtime),
    dist_from_6pm_mins =
      difftime(bedtime, (date_bedtime %m+% hours(18)), units = "mins") %>% 
      as.integer(), 
    mins_from_8_hours = duration_mins - (8 * 60),
    day_of_week = wday(date_bedtime, label = TRUE),
    work_day = 
      ifelse(
        day_of_week %in% c("Fri", "Sat"),
        "Fri-Sat", "Worknight"
      )
  ) # %>% select(-c(bedtime, wake))

write_csv(final_sleep, "output/sleep.csv")
