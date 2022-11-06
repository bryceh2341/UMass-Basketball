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
library(data.table)

# team <- "Towson"
# team_schedule <- get_team_schedule(season = "2021-22", team.name = team)
# 
# lineups <- get_lineups(play_by_play_data = get_play_by_play(team_schedule$Game_ID), include_transition = T)
# twoman <- get_player_combos(Lineup_Data = get_lineups(play_by_play_data = get_play_by_play(team_schedule$Game_ID), include_transition = T),n = 2,include_transition = T, min_mins = 100)
# threeman <- get_player_combos(Lineup_Data = get_lineups(play_by_play_data = get_play_by_play(team_schedule$Game_ID), include_transition = T),n = 3,include_transition = T, min_mins = 100)
# 
# 
# team_lineups <- lineups %>%
#   filter(Team == team) %>%
#   arrange(desc(Mins)) %>%
#   mutate(ORTG = round(ORTG,1),
#          DRTG = round(DRTG,1),
#          NETRTG = round(NETRTG,1),
#          Mins = round(Mins,0),
#          #P1 = gsub(".", " ", P1, fixed=TRUE),
#          P1 = sub("^\\S+\\s+", '', gsub(".", " ", P1, fixed=TRUE)),
#          P2 = sub("^\\S+\\s+", '', gsub(".", " ", P2, fixed=TRUE)),
#          P3 = sub("^\\S+\\s+", '', gsub(".", " ", P3, fixed=TRUE)),
#          P4 = sub("^\\S+\\s+", '', gsub(".", " ", P4, fixed=TRUE)),
#          P5 = sub("^\\S+\\s+", '', gsub(".", " ", P5, fixed=TRUE)),
#          Lineup = paste0(P1, " - ", P2," - ", P3," - ", P4," - ", P5)) %>%
#   slice(1:10) %>%
#   select(Lineup, Mins, ORTG, DRTG, NETRTG)
# 
# twoman <- twoman %>%
#   filter(Team == team) %>%
#   arrange(desc(Mins)) %>%
#   mutate(ORTG = round(ORTG,1),
#          DRTG = round(DRTG,1),
#          NETRTG = round(NETRTG,1),
#          Mins = round(Mins,0),
#          #P1 = gsub(".", " ", P1, fixed=TRUE),
#          P1 = sub("^\\S+\\s+", '', gsub(".", " ", P1, fixed=TRUE)),
#          P2 = sub("^\\S+\\s+", '', gsub(".", " ", P2, fixed=TRUE)),
#          Lineup = paste0(P1, " - ", P2)) %>%
#   slice(1:10) %>%
#   ungroup() %>%
#   select(Lineup, Mins, ORTG, DRTG, NETRTG)
# 
# threeman <- threeman %>%
#   filter(Team == team) %>%
#   arrange(desc(Mins)) %>%
#   mutate(ORTG = round(ORTG,1),
#          DRTG = round(DRTG,1),
#          NETRTG = round(NETRTG,1),
#          Mins = round(Mins,0),
#          #P1 = gsub(".", " ", P1, fixed=TRUE),
#          P1 = sub("^\\S+\\s+", '', gsub(".", " ", P1, fixed=TRUE)),
#          P2 = sub("^\\S+\\s+", '', gsub(".", " ", P2, fixed=TRUE)),
#          P3 = sub("^\\S+\\s+", '', gsub(".", " ", P3, fixed=TRUE)),
#          Lineup = paste0(P1, " - ", P2," - ", P3)) %>%
#   slice(1:10) %>%
#   ungroup() %>%
#   select(Lineup, Mins, ORTG, DRTG, NETRTG)

team_lineups %>%
  gt() %>%
  cols_label(Lineup = "Lineup", Mins = "Mins", ORTG = "ORtg.", DRTG = "DRtg.", NETRTG = "Net Rtg."
  ) %>%
  tab_header(
    title = md(paste0(team, " Lineups")),
    #subtitle = table_subtitle
  )  %>%
  data_color(
    columns = c(NETRTG),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "RColorBrewer::RdYlGn",
        direction  = 1
      ) %>% as.character(),
      domain = c(min(team_lineups$NETRTG),max(team_lineups$NETRTG)),
      na.color = "#00441BFF"
    )
  ) %>%
  data_color(
    columns = c(ORTG),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "RColorBrewer::RdYlGn",
        direction  = 1
      ) %>% as.character(),
      domain = c(min(team_lineups$ORTG),max(team_lineups$ORTG)),
      na.color = "#00441BFF"
    )
  ) %>%
  data_color(
    columns = c(DRTG),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "RColorBrewer::RdYlGn",
        direction  = -1
      ) %>% as.character(),
      domain = c(min(team_lineups$DRTG),max(team_lineups$DRTG)),
      na.color = "#00441BFF"
    )
  ) %>%
  cols_align(
    align = "left",
    columns = vars(Mins, ORTG, DRTG, NETRTG)
  ) %>%
  cols_align(
    align = "center",
    columns = vars(Lineup)
  ) %>%
  cols_width(vars(Lineup,) ~ px(300),
             vars(NETRTG) ~ px(60),
             vars(ORTG, DRTG, Mins) ~ px(45),
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
      rows = Lineup == "League Average"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "floralwhite"),
    locations = cells_body(
      rows = Lineup == "League Average")
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
  gtsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Team Lineups.png", expand = 0)

twoman %>%
  gt() %>%
  cols_label(Lineup = "Lineup", Mins = "Mins", ORTG = "ORtg.", DRTG = "DRtg.", NETRTG = "Net Rtg."
  ) %>%
  tab_header(
    title = md(paste0(team, " Two-Man Lineups")),
    #subtitle = table_subtitle
  )  %>%
  data_color(
    columns = c(NETRTG),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "RColorBrewer::RdYlGn",
        direction  = 1
      ) %>% as.character(),
      domain = c(min(twoman$NETRTG),max(twoman$NETRTG)),
      na.color = "#00441BFF"
    )
  ) %>%
  data_color(
    columns = c(ORTG),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "RColorBrewer::RdYlGn",
        direction  = 1
      ) %>% as.character(),
      domain = c(min(twoman$ORTG),max(twoman$ORTG)),
      na.color = "#00441BFF"
    )
  ) %>%
  data_color(
    columns = c(DRTG),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "RColorBrewer::RdYlGn",
        direction  = -1
      ) %>% as.character(),
      domain = c(min(twoman$DRTG),max(twoman$DRTG)),
      na.color = "#00441BFF"
    )
  ) %>%
  cols_align(
    align = "left",
    columns = vars(Mins, ORTG, DRTG, NETRTG)
  ) %>%
  cols_align(
    align = "center",
    columns = vars(Lineup)
  ) %>%
  cols_width(vars(Lineup,) ~ px(135),
             vars(NETRTG) ~ px(60),
             vars(ORTG, DRTG, Mins) ~ px(45),
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
      rows = Lineup == "League Average"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "floralwhite"),
    locations = cells_body(
      rows = Lineup == "League Average")
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
  gtsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Two Man Lineups.png", expand = 0)

threeman %>%
  gt() %>%
  cols_label(Lineup = "Lineup", Mins = "Mins", ORTG = "ORtg.", DRTG = "DRtg.", NETRTG = "Net Rtg."
  ) %>%
  tab_header(
    title = md(paste0(team, " Three-Man Lineups")),
    #subtitle = table_subtitle
  )  %>%
  data_color(
    columns = c(NETRTG),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "RColorBrewer::RdYlGn",
        direction  = 1
      ) %>% as.character(),
      domain = c(min(threeman$NETRTG),max(threeman$NETRTG)),
      na.color = "#00441BFF"
    )
  ) %>%
  data_color(
    columns = c(ORTG),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "RColorBrewer::RdYlGn",
        direction  = 1
      ) %>% as.character(),
      domain = c(min(threeman$ORTG),max(threeman$ORTG)),
      na.color = "#00441BFF"
    )
  ) %>%
  data_color(
    columns = c(DRTG),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "RColorBrewer::RdYlGn",
        direction  = -1
      ) %>% as.character(),
      domain = c(min(threeman$DRTG),max(threeman$DRTG)),
      na.color = "#00441BFF"
    )
  ) %>%
  cols_align(
    align = "left",
    columns = vars(Mins, ORTG, DRTG, NETRTG)
  ) %>%
  cols_align(
    align = "center",
    columns = vars(Lineup)
  ) %>%
  cols_width(vars(Lineup,) ~ px(200),
             vars(NETRTG) ~ px(60),
             vars(ORTG, DRTG, Mins) ~ px(45),
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
      rows = Lineup == "League Average"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "floralwhite"),
    locations = cells_body(
      rows = Lineup == "League Average")
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
  gtsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Three Man Lineups.png", expand = 0)