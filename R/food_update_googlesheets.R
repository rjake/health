

food_count <- 
  eating_prep %>% 
  count(food) %>% 
  filter(n > 1)

pct <- function(x) as.integer(round(x, 3) * 100)

calorie_calc <- function(x) {
  x_vars <- (names(x)[!str_detect(names(x), "meal|date|sodium|sugar|unique")])
  
  x %>%
    group_by_at((x_vars)) %>% 
    summarise(n = n()) %>% 
    ungroup() %>%
    mutate(
      cc = carbs * 4,
      cf = fat * 9,
      cp = protein * 4,
      calories = (cc + cf + cp),
      pc = pct(cc / calories),
      pf = pct(cf / calories),
      pp = pct(cp / calories),
      sweetspot = 
        ifelse((pc > 45 & pf > 25 & pp > 15), "Yes", "No"),
      category =
        paste0(
          ifelse(pc > 30, "C", ""),
          ifelse(pf > 30, "F", ""),
          ifelse(pp > 30, "P", "")
        ),
      ideal = 1-((abs(pc-50) + abs(pf - 30) + abs(pp - 20))/100)
    ) %>%
    filter(calories > 50) %>% 
    select(n, everything()) %>%
    rename(
      `carbs (g)` = carbs,
      `fat (g)` = fat,
      `protein (g)` = protein,
      `% carbs` = pc,
      `% fat` = pf,
      `% protein` = pp
    ) %>% 
    select(-c(cc, cp, cf)) %>% 
    select(n, food, sweetspot, calories, category, everything())
}

as_item <-
  calorie_calc(
    eating_prep %>% select(-unique)
  )
as_meal <-
  calorie_calc(
    #x <-
    eating_prep %>%
      group_by(unique) %>%
      summarise(
        food = paste0(food, collapse = "\n"),
        carbs = sum(carbs),
        protein = sum(protein),
        fat = sum(fat)
      ) %>%
      ungroup() 
  ) 

viewxl::view_in_xl(as_item)
viewxl::view_in_xl(as_meal)
