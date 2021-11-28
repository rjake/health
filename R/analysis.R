library(tidyverse)

raw_food <- read_csv("output/food.csv")
raw_sleep <- read_csv("output/sleep.csv")
raw_fit <- read_csv("output/google_fit.csv")

clean_sleep <-
  raw_sleep %>% 
  select(
    date = date_wake, 
    sleep_broken_ind = broken_sleep_ind,
    sleep_duration_mins = duration_mins, 
    sleep_dist_from_6pm_mins = dist_from_6pm_mins
  ) %>% 
  mutate(date = as.Date(with_tz(date)))


all_data <-
  raw_food %>% 
  inner_join(clean_sleep) %>% 
  inner_join(raw_fit)


all_data %>% 
  mutate(date = as.integer(date)) %>% 
  pivot_longer(cols = -c(total_calories)) %>%
#  filter(name == "steps") %>% 
  drop_na() %>% 
  group_by(name) %>%
  filter(n() > 10) %>% 
  mutate(cor = cor(total_calories, value)) %>% 
  ungroup() %>% 
  mutate(
    abs_cor = abs(cor),
    metric = fct_reorder(name, cor, .desc = TRUE)
  ) %>% 
  filter(metric != "excessive_calories_ind") %>% 
  ggplot(aes(value, total_calories)) +
  ylim(0, 2500) +
  facet_wrap(
    facets = ~metric + paste("cor = ", round(cor, 2)), 
    scales = "free_x", nrow = 4
  ) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "lm")

all_data %>% 
  mutate(date = floor_date(date, "week")) %>% 
  ggplot(aes(date, total_calories)) +
  geom_line(aes(group = date), alpha = 0.3) +
  geom_point(alpha = 0.3) +
  stat_summary(geom = "smooth", fun = mean, color = "red") +
  stat_summary(geom = "point", fun = mean, color = "red") +
  ylim(0, 2500)


all_data %>% 
  mutate(
    diff = 
      total_calories - 
      lag(total_calories, default = first(total_calories))
    ) %>% 
  ggplot(aes(date, diff, color = diff > 0)) +
  geom_col(alpha = 0.3)


cor_food <-
  all_data %>% 
  select_if(~(is.numeric(.) | is.logical(.))) %>% 
  cor(use = "pairwise.complete.obs")


cor_food[2, , drop = FALSE] %>% 
  corrplot::corrplot()


whereiation::variation_plot(
  all_data, 
  "total_calories",
  ignore_cols = c("excessive_calories_ind", "date")
)

cor.test(raw_food$total_calories, raw_food$calories_18) %>% 
  broom::tidy()
