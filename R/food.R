library(tidyverse)
library(lubridate)
library(googlesheets)
#library(scales)

gs_eating <- 
  gs_key(Sys.getenv("google_eating"))

eating_raw <-
  gs_read(
    gs_eating,
    ws = "raw",
    lookup = TRUE,
    check.names = FALSE
  )

string_date <- "day.*, \\d{4}$"
string_meal <- "^(Meal .|Snacks .M)$"

eating_prep <- 
  eating_raw %>% 
  select(-order) %>% 
  filter(!is.na(food)) %>% 
  mutate(
    date = 
      ifelse(str_detect(food, string_date), food, NA) %>% 
      mdy(),
    meal =  ifelse(str_detect(food, string_meal) & is.na(calories), food, NA)
  ) %>% 
  fill(date, meal) %>% 
  filter(
    !is.na(carbs),
    !str_detect(food, "Add Food Quick")
  ) %>% 
  group_by(date, meal) %>% 
  mutate(
    n = n(),
    row = ifelse(str_detect(meal, "Snacks"), row_number(), 1)
  ) %>% 
  ungroup() %>% 
#  filter(str_detect(food, "chocolate chip cookies")) %>% 
  mutate(
    unique = paste(meal, date, row),
    calories = as.integer(calories),
    fat = as.integer(fat),
    sugar = 
      ifelse(str_detect(food, "chocolate chip cookies"), carbs * 0.8, sugar) %>% 
      as.integer(sugar)
  ) 

food_stats <- function(df, f1 = summarise, f2 = sum, ...) {
  # browser()
  df %>% 
    f1(
      calories = f2(calories),
      carbs = f2(carbs),
      fat = f2(fat),
      protein = f2(protein),
      sodium = f2(sodium),
      sugar = f2(sugar),
      ...
    )
}
  

agg_meals <-
  eating_prep %>% 
  group_by(date, unique, meal, row, n) %>% 
  food_stats() %>% 
  ungroup() %>% 
  mutate(
    time = case_when(
      meal == "Meal 1" ~ 9.0,
      meal == "Meal 2" ~ 12.0,
      meal == "Meal 3" ~ 18.0,
      meal == "Meal 4" ~ 22.0,
      meal == "Snacks AM" ~ 9.5 + (2 * row/n),
      meal == "Snacks PM" ~ 13.5 + (10 * row/n),
      TRUE ~ -99
    ),
    time_breaks = cut(time, breaks = c(0, 12, 18, 24), right = FALSE)
  ) 

unique(agg_meals$time_breaks)

filter(agg_meals, date == "2020-08-12") %>% arrange(time)  

agg_timeframes <-
  agg_meals %>% 
#  filter(date %in% c(as.Date("2019-09-19") + 1:3)) %>% 
  arrange(date, time) %>%
  group_by(date) %>% 
  mutate(
    pct_snacks = sum(str_detect(meal, "Snack")) / n(),
    total_calories = sum(calories),
    missing_breakfast_ind = max(!any(str_detect(meal, "Meal 1"))),
    missing_lunch_ind = max(!any(str_detect(meal, "Meal 2")))
  ) %>%
  group_by(
    date, time_breaks, 
    pct_snacks, total_calories,
    missing_breakfast_ind,
    missing_lunch_ind
  ) %>% 
  food_stats() %>% 
  ungroup() %>%
  complete(date, nesting(time_breaks)) %>%
  group_by(date) %>% 
  fill(
    pct_snacks, total_calories, 
    missing_breakfast_ind,
    missing_lunch_ind,
    .direction = "downup"
  ) %>% 
  ungroup() %>% 
  mutate_if(is.numeric, replace_na, 0) %>% 
  group_by(date) %>% 
  food_stats(mutate, cumsum) %>% 
  ungroup() %>% 
  mutate(time_breaks = str_extract(time_breaks, "[\\d\\.]+(?=\\))")) %>%
  filter(time_breaks != 24) 


food_metrics <-
  agg_timeframes %>% 
  pivot_longer(cols = -c(date:missing_lunch_ind)) %>% 
  pivot_wider(names_from = c(name, time_breaks), values_from = value) %>% 
  arrange(date) %>% 
  mutate(
    excessive_calories_ind = total_calories > 1700,
    prior_day_calories = lag(total_calories, default = 1700)
  ) 


write_csv(food_metrics, "output/food.csv")
