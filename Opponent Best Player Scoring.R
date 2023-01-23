library(bigballR)
library(toRvik)
library(gt)
library(cowplot)
library(ggpubr)
library(RGraphics)
library(gridExtra)
library(stringr)
library(dplyr)
library(ncaahoopR)

bart_player_stats <- bart_player_season(year=2023, stat = 'all')

bart_player_stats <- bart_player_stats %>%
  filter(team %in% c("Central Connecticut", "Towson", "Colorado", "Murray St.", "Charlotte", "South Florida", "Harvard", "Albany", "UMass Lowell", "Hofstra", "North Texas", "Dartmouth", "St. Bonaventure", "Saint Louis", "George Washington", "La Salle"),
         player != "Aaron Estrada") %>%
  arrange(desc(ppg)) %>%
  slice(1:15) %>%
  select(player, ppg)

bart_player_stats$points <- c(26,25,13,23,15,15,27,31,11,17,13,24,9,20,8)

bart_player_stats <- bart_player_stats %>%
  mutate(diff = round(points-ppg,1),
         ppg = round(ppg,1))

#print(mean(bart_player_stats$diff))

bart_player_stats %>%
  gt()  %>%
  cols_label(player = "Player",
             ppg = "PPG",
             points = "Points",
             diff = "Diff") %>%
  tab_header(
    title = "UMass vs High Scorers",
    subtitle = "Points Allowed vs Highest Scoring Opponents"
  )  %>%
  # text_transform(
  #   locations = cells_body(vars(url)),
  #   fn = function(x) {
  #     web_image(url = x,
  #               height = px(22.5))
  #   }
  # ) %>%
  # fmt_percent(
  #   columns = vars(off_rtg_perc, halfcourt, trans),
  #   decimals = 0
  # )  %>%
  # fmt_percent(
  #   columns = vars(efg, tov, OReb, FTR),
  #   decimals = 1
  # )  %>%
  data_color(
    columns = vars(diff),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "RColorBrewer::RdYlGn",
        direction  = -1
      ) %>% as.character(),
      domain = c(-8, 15),
      na.color = "#00441BFF"
    )
  ) %>%
  cols_align(
    align = "left",
    columns = vars(player, ppg, points, diff)
  ) %>%
  cols_width(vars(ppg, points, diff) ~ px(70),
             #vars(off_rtg_perc, efg_perc, tov_perc, OReb_perc, FTR_perc) ~ px(30),
             vars(player) ~ px(125)) %>%
  tab_style(
    style = list(
      cell_borders(
        side =  "top",
        color = 'gray55',
        weight = px(2)
      )
    ),
    locations = cells_body(
      rows = player == "League Average"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "floralwhite"),
    locations = cells_body(
      rows = player == "League Average")
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
  gtsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Best Players.png", expand = 0)
