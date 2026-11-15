# Shared chart style for A2 (Callon + Isla use the same file)
# Load it after the data cleaning code: source("A2-style.R")

# Colours: income fifths (blue = affluent, rust = high-poverty)
income_cols <- c("Most affluent fifth"   = "#2E6F9E",
                 "Second fifth"          = "#7FA7C6",
                 "Middle fifth"          = "#ADADAD",
                 "Fourth fifth"          = "#D9886F",
                 "Highest-poverty fifth" = "#B5432A")

# Colours: share of students whose first language is not English (light to dark)
lang_cols <- c("10% or fewer"  = "#CFC3E6",
               "10% to 30%"    = "#9479C4",
               "More than 30%" = "#4B2C8A")

# Same caption under every chart
source_note <- paste("Source: Ontario Ministry of Education school information",
                     "(STA313 datasets). Averages of English-language public",
                     "and Catholic elementary schools.")

# 2017 -> "2017-18"
year_label <- function(y) paste0(y, "-", substr(y + 1, 3, 4))

# One theme for every chart
theme_story <- theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 12.5),
        plot.title.position = "plot",
        plot.subtitle = element_text(colour = "grey30"),
        plot.caption = element_text(colour = "grey45", size = 7.5, hjust = 0),
        plot.caption.position = "plot",
        panel.grid.minor = element_blank(),
        legend.position = "top",
        legend.justification = "left")
