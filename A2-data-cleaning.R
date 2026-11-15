# STA313 A2 - data cleaning for ont-school-demo-info.csv
# Paste this into the first code chunks of A2.Rmd (after the setup chunk).
# Output objects:
#   schools  : every school-year row, short column names, numbers fixed
#   elem     : analysis set (see step 4), one row per school per test year
#   cleaning_log : row counts after each step, for the methodology section

# 0. Packages ---------------------------------------------------------------
pkgs <- c("tidyverse", "scales")
missing_pkgs <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(missing_pkgs) > 0) {
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
}
library(tidyverse)
library(scales)

# 1. Read ------------------------------------------------------------------
# The file is over 10 MB, so the handout requires reading it from a URL.
data_url <- paste0("https://raw.githubusercontent.com/mjmoon/",
                   "sta313-project-datasets/main/ONT-SCHOOL/",
                   "ont-school-demo-info.csv")
raw <- read_csv(data_url, show_col_types = FALSE,
                col_types = cols(.default = col_character()))
# Everything is read as text first so nothing is silently guessed wrong.
# Expected: 39,072 rows, 36 columns.

# 2. Fix numbers -------------------------------------------------------------
pct_cols <- names(raw)[str_starts(names(raw), "perc")]

schools <- raw |>
  mutate(
    across(all_of(pct_cols), ~ suppressWarnings(as.numeric(.x))),
    # 2023-24 is the only year stored as proportions (0-1) instead of 0-100
    across(all_of(pct_cols), ~ if_else(year == "2023-2024", .x * 100, .x)),
    # enrolment uses "SP" when suppressed; that becomes NA
    enrolment  = suppressWarnings(as.numeric(enrolment)),
    latitude   = as.numeric(latitude),
    longitude  = as.numeric(longitude),
    start_year = as.integer(str_sub(year, 1, 4))
  ) |>
  # 3. Short names -------------------------------------------------------------
  rename(
    first_lang_not_eng = percentage_of_students_whose_first_language_is_not_english,
    newcomer_non_eng   = percentage_of_students_who_are_new_to_canada_from_a_non_english_speaking_country,
    special_ed         = percentage_of_students_receiving_special_education_services,
    gifted             = percentage_of_students_identified_as_gifted,
    g3_read            = percentage_of_grade_3_students_achieving_the_provincial_standard_in_reading,
    g3_write           = percentage_of_grade_3_students_achieving_the_provincial_standard_in_writing,
    g3_math            = percentage_of_grade_3_students_achieving_the_provincial_standard_in_mathematics,
    g6_read            = percentage_of_grade_6_students_achieving_the_provincial_standard_in_reading,
    g6_write           = percentage_of_grade_6_students_achieving_the_provincial_standard_in_writing,
    g6_math            = percentage_of_grade_6_students_achieving_the_provincial_standard_in_mathematics,
    low_income         = percentage_of_school_aged_children_who_live_in_low_income_households,
    parents_no_degree  = percentage_of_students_whose_parents_have_no_degree_diploma_or_certificate
  ) |>
  select(year, start_year, board_number, board_name, school_number, school_name,
         school_type, school_special_condition_code, school_level, school_language,
         city, enrolment, latitude, longitude,
         first_lang_not_eng, newcomer_non_eng, special_ed, gifted,
         g3_read, g3_write, g3_math, g6_read, g6_write, g6_math,
         low_income, parents_no_degree)

# 4. Analysis set ------------------------------------------------------------
# Keep: elementary, English-language, regular (no special condition),
#       public or Catholic schools.
# Drop: 2019-20 and 2020-21. EQAO did not test in those years (COVID-19) and
#       99.9% of their Grade 3 reading values are exact copies of 2018-19.
regular_elem <- schools |>
  filter(school_level == "Elementary",
         school_language == "English",
         school_special_condition_code == "Not applicable",
         school_type %in% c("Public", "Catholic"))

copy_check <- regular_elem |>
  filter(year %in% c("2018-2019", "2019-2020")) |>
  select(school_number, year, g3_read) |>
  pivot_wider(names_from = year, values_from = g3_read) |>
  drop_na()
share_copied <- mean(copy_check$`2018-2019` == copy_check$`2019-2020`)
# Expected: about 0.999

test_years <- c("2017-2018", "2018-2019", "2021-2022",
                "2022-2023", "2023-2024", "2024-2025")

# 5. Income groups -------------------------------------------------------------
# The low-income measure comes from the census and switches to a newer census
# in 2022-23 (medians drop from about 15% to 10%). To compare schools fairly
# over time, each school is placed in a fifth (quintile) using its 2017-18
# value, and keeps that group in every year.
income_labels <- c("Most affluent fifth", "Second fifth", "Middle fifth",
                   "Fourth fifth", "Highest-poverty fifth")

income_base <- regular_elem |>
  filter(year == "2017-2018", !is.na(low_income)) |>
  mutate(income_group = factor(ntile(low_income, 5), levels = 1:5,
                               labels = income_labels)) |>
  select(school_number, low_income_2017 = low_income, income_group)

elem <- regular_elem |>
  filter(year %in% test_years) |>
  inner_join(income_base, by = "school_number") |>
  mutate(lang_group = cut(first_lang_not_eng, breaks = c(-Inf, 10, 30, Inf),
                          labels = c("10% or fewer", "10% to 30%",
                                     "More than 30%")))

# 6. Log for the methodology section ----------------------------------------
cleaning_log <- tibble(
  step = c("All school-year rows",
           "English regular public/Catholic elementary",
           "Test years only (drop 2019-20, 2020-21)",
           "With a 2017-18 low-income value (final)"),
  rows = c(nrow(schools),
           nrow(regular_elem),
           nrow(filter(regular_elem, year %in% test_years)),
           nrow(elem)),
  schools = c(n_distinct(schools$school_number),
              n_distinct(regular_elem$school_number),
              n_distinct(filter(regular_elem, year %in% test_years)$school_number),
              n_distinct(elem$school_number))
)
cleaning_log
# Expected (checked by re-running the same steps in Python on Nov 15):
#   All school-year rows                          39,072 rows  5,106 schools
#   English regular public/Catholic elementary    28,721 rows  3,725 schools
#   Test years only                               21,552 rows  3,723 schools
#   With a 2017-18 low-income value (final)       20,836 rows  3,525 schools
# In the final set: Grade 3 reading has 18,471 non-missing values,
# Grade 6 reading has 17,690 (the rest are suppressed: fewer than 10 students).
