# STA313 A2 - data cleaning
library(tidyverse)

# 1. Read the data
data_url <- "https://raw.githubusercontent.com/mjmoon/sta313-project-datasets/main/ONT-SCHOOL/ont-school-demo-info.csv"
raw <- read_csv(data_url, show_col_types = FALSE, guess_max = 50000)

# 2. Keep only the columns we need, with short names
schools <- raw |>
  select(year, school_number, school_level, school_language, school_type,
         school_special_condition_code,
         g3_read    = percentage_of_grade_3_students_achieving_the_provincial_standard_in_reading,
         g6_read    = percentage_of_grade_6_students_achieving_the_provincial_standard_in_reading,
         low_income = percentage_of_school_aged_children_who_live_in_low_income_households)

# 3. 2023-24 is stored as proportions (0.73); every other year is percent (73)
schools <- schools |>
  mutate(across(c(g3_read, g6_read, low_income),
                ~ if_else(year == "2023-2024", .x * 100, .x)),
         start_year = as.integer(substr(year, 1, 4)))

# 4. Keep regular English-language public and Catholic elementary schools.
#    Drop 2019-20 and 2020-21: no EQAO tests (COVID-19), values copied from 2018-19.
elem <- schools |>
  filter(school_level == "Elementary",
         school_language == "English",
         school_special_condition_code == "Not applicable",
         school_type %in% c("Public", "Catholic"),
         !year %in% c("2019-2020", "2020-2021"))

# 5. Income groups: split schools into fifths by their 2017-18 low-income share.
#    Each school keeps the same group in every year.
income_groups <- elem |>
  filter(year == "2017-2018", !is.na(low_income)) |>
  mutate(income_group = factor(ntile(low_income, 5),
                               labels = c("Most affluent fifth", "Second fifth",
                                          "Middle fifth", "Fourth fifth",
                                          "Highest-poverty fifth"))) |>
  select(school_number, income_group)

elem <- elem |> inner_join(income_groups, by = "school_number")

c(rows = nrow(elem), schools = n_distinct(elem$school_number))
