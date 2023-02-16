library(toRvik)
library(tidyr)
library(tidyverse)
library(bigballR)
library(ncaahoopR)
library(gt)
library(cowplot)
library(ggpubr)
library(dplyr)
library(gtExtras)

setwd("C:/Users/Bryce Haase/Desktop/UMass Basketball")

full_comp <- read.csv("Manual Team Stats Comp.csv")

Opp = "Loyola Chicago"

full_comp %>%
  gt() %>%
  cols_label(stat = "Stat", UMass = "UMass", Opp = Opp
  ) %>%
  tab_header(
    title = md("Team Stats Comparison"),
    #subtitle = table_subtitle
  )  %>%
  data_color(
    columns = vars(UMass, Opp),
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
    columns = vars(stat)
  ) %>%
  cols_align(
    align = "center",
    columns = vars(UMass, Opp)
  ) %>%
  cols_width(vars(stat, UMass, Opp) ~ px(125),
             #vars(Player) ~ px(115),
             #vars(Charges, Rim, Three) ~ px(45),
             #vars(Successful_screen, Unsuccessful_screen) ~ px(45),
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
      rows = UMass == "League Average"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "floralwhite"),
    locations = cells_body(
      rows = UMass == "League Average")
  ) %>%
  tab_options(
    table.background.color = "floralwhite",
    column_labels.font.size = 10.5,
    table.font.size = 10,
    heading.title.font.size  = 15,
    heading.title.font.weight = 'bold',
    heading.subtitle.font.size = 11,
    table.font.names = "Consolas",
    table.font.color = 'black',
    #table.border.top.color = "transparent",
    data_row.padding = px(2),
    footnotes.font.size = 8,
    source_notes.font.size = 9,
    footnotes.padding = px(1),
    #column_labels.hidden = TRUE
  ) %>%
  gtsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Team Comp.png", expand = 0)