library(gt)
library(cowplot)
library(ggpubr)
library(dplyr)
library(gtExtras)

setwd("C:/Users/Bryce Haase/Desktop/UMass Basketball")

practice_data <- read.csv("Practice Differences.csv")

practice_data %>%
  gt() %>%
  cols_label(Stat = "Stat",
             Part_1 = "November 2nd to 5th",
             Part_2 = "November 6th to 9th",
             Diff = "Difference"
  ) %>%
  tab_header(
    title = md("Practice Stat Differences"),
    subtitle = "November 2-5 & November 6-9"
  )  %>%
  data_color(
    columns = c(Diff),
    #rows = where(is.numeric),
    #rows = c(full_practice_data[1:14,]),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::red_material",
        direction = -1
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_align(
    align = "left",
    columns = vars(Stat, Part_1, Part_2, Diff)
  ) %>%
  cols_width(vars(Stat) ~ px(65),
             vars(Diff) ~ px(65),
             vars(Part_1, Part_2) ~ px(75),
  ) %>%
  tab_style(
    style = list(
      cell_borders(
        side =  "top",
        color = 'gray55',
        weight = px(2)
      )
    ),
    locations = cells_body(
      rows = Stat == "League Average"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "floralwhite"),
    locations = cells_body(
      rows = Stat == "League Average")
  ) %>%
  tab_options(
    table.background.color = "floralwhite",
    column_labels.font.size = 10.5,
    table.font.size = 10,
    heading.title.font.size  = 24,
    heading.title.font.weight = 'bold',
    heading.subtitle.font.size = 11,
    table.font.names = "Consolas",
    table.font.color = 'black',
    #table.border.top.color = "transparent",
    data_row.padding = px(2),
    footnotes.font.size = 8,
    source_notes.font.size = 9,
    footnotes.padding = px(1),
  ) %>%
  gtsave("November Practice Comparison.png")
