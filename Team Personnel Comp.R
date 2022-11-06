library(dplyr)
library(tidyverse)
library(bigballR)
library(toRvik)
library(ncaahoopR)
library(gt)
library(cowplot)
library(ggpubr)
library(dplyr)
library(gtExtras)

personnel <- read.csv("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Team_personnel_comp.csv")

personnel %>%
  gt() %>%
  cols_label(url = "",
             Player = "",
             Height = "",
             url_2 = "",
             Player_2 = "",
             Height_2 = ""
  ) %>%
  tab_header(
    title = md("Personnel Comparison"),
    subtitle = ""
  )  %>%
  tab_spanner(label = "UMass", columns = vars(url, Player, Height)) %>%
  tab_spanner(label = "Towson", columns = vars(Height_2, Player_2, url_2)) %>%
  # tab_spanner(label = "4 Fouls", columns = vars(four_record, four_win_per)) %>%
  # tab_spanner(label = "5 Fouls", columns = vars(five_record, five_win_per)) %>%
  text_transform(
    locations = cells_body(vars(url)),
    fn = function(x) {
      web_image(url = x,
                height = px(22.5))
    }
  ) %>%
  text_transform(
    locations = cells_body(vars(url_2)),
    fn = function(x) {
      web_image(url = x,
                height = px(22.5))
    }
  ) %>%
  # data_color(
  #   columns = c(two_win_per, three_win_per, four_win_per, five_win_per),
  #   colors = scales::col_numeric(
  #     palette = paletteer::paletteer_d(
  #       palette = "ggsci::green_material"
  #     ) %>% as.character(),
  #     domain = NULL
  #   )
  # ) %>%
  cols_align(
    align = "left",
    columns = vars(url, url_2, Player, Player_2, Height, Height_2)
  ) %>%
  cols_width(vars(url, url_2, Height, Height_2) ~ px(45),
             vars(Player, Player_2) ~ px(115),
             #vars(Charges) ~ px(45),
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
      rows = url == "League Average"
    )
  ) %>%
    tab_style(
      style = cell_borders(
        sides = c("right"),
        color = "gray55",
        weight = px(3),
        style = "solid"
      ),
      locations = cells_body(
        columns = vars(Height),
        rows = everything()
      )
    ) %>%
  tab_style(
    style = cell_fill(color = "floralwhite"),
    locations = cells_body(
      rows = url == "League Average")
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
  ) %>%
  gtsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Personnel Comp.png", expand = 0)