library(gt)
library(cowplot)
library(ggpubr)
library(dplyr)
library(gtExtras)

setwd("C:/Users/Bryce Haase/Desktop/UMass Basketball")

data <- read.csv("Advanced Practice Data.csv")
data$Number <- as.character(data$Number)


data %>%
  gt() %>%
  cols_label(Stat = "Stat",
             Number = "#",
             Rank = "NCAA Rank",
             Explanation = "Description"
  ) %>%
  tab_header(
    title = md("Advanced Scrimmage Numbers"),
    subtitle = "Vs. Holy Cross"
  )  %>%
  # text_transform(
  #   locations = cells_body(vars(url)),
  #   fn = function(x) {
  #     web_image(url = x,
  #               height = px(22.5))
  #   }
  # ) %>%
  data_color(
    columns = vars(Rank),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "RColorBrewer::RdYlGn",
        direction  = -1
      ) %>% as.character(),
      domain = c(358,0),
      na.color = "#00441BFF"
    )
  ) %>%
  cols_align(
    align = "left",
    columns = vars(Stat, Number, Rank, Explanation)
  ) %>%
  cols_align(
    align = "center",
    columns = vars(Rank)
  ) %>%
  cols_width(vars(Rank) ~ px(65),
             vars(Explanation) ~ px(300),
             vars(Stat) ~ px(100),
             vars(Number) ~ px(45)
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
  gtsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Advanced Scrimmage Data.png", expand = 0)
