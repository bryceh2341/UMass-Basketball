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


team.name = "Saint Joseph's"

setwd("C:/Users/Bryce Haase/Desktop/UMass Basketball")

team_stats <- read.csv("Manual Team Stats.csv")

team_shooting <- team_stats %>%
  select(team, close_share, rim_a_rank, rim_per, rim_per_rank, far_share, mid_a_rank, mid_per, mid_per_rank, three_share, three_a_rank, three_per, three_per_rank) %>%
  slice(2:3)

# team_four_factors <- team_four_factors %>%
#   select(team, off_efg, off_efg_rank, off_to, off_to_rank, off_or, off_or_rank, off_ftr, off_ftr_rank, def_efg, def_efg_rank, def_to, def_to_rank, def_or, def_or_rank, def_ftr, def_ftr_rank)

# misc_stats <- misc_stats %>%
#   select(tempo, tempo_rank, Transition, Transition_Rank, Second_Chance, Second_Chance_Rank, Bench, Bench_Rank)

team_four_factors <- team_stats %>%
  select(team, off_efg, off_efg_rank, off_to, off_to_rank, off_or, off_or_rank, off_ftr, off_ftr_rank) %>%
  slice(2:3)

# team_rating <- bart_ratings(year = 2023)
# 
# 
# team_rating <- team_rating %>%
#   mutate (SOS = round(rank(desc(ov_cur_sos)),0)) %>%
#   filter(team == team.name) %>%
#   select(team, conf, barthag_rk, adj_o_rk, adj_d_rk, SOS)
# 
# team_rating <- merge(basic_stats, team_rating, by="team")
team_rating <- team_stats %>%
  select(wins, losses, conf, barthag_rk, adj_o_rk, adj_d_rk, SOS) %>%
  slice(1)

plot_1 <- team_rating %>%
  gt() %>%
  cols_label(wins = "Wins", losses = "Losses", conf = "Conf.", barthag_rk = "Rank", adj_o_rk = "Offense", adj_d_rk = "Defense", SOS = "SOS"
  ) %>%
  tab_header(
    title = md("Team Info"),
    #subtitle = table_subtitle
  )  %>%
  data_color(
    columns = vars(barthag_rk, adj_o_rk, adj_d_rk, SOS),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "RColorBrewer::RdYlGn",
        direction  = -1
      ) %>% as.character(),
      domain = c(363,0),
      na.color = "#00441BFF"
    )
  ) %>%
  cols_align(
    align = "left",
    columns = vars(wins, losses, conf, barthag_rk, adj_o_rk, adj_d_rk, SOS)
  ) %>%
  cols_width(vars(wins, losses, conf, barthag_rk, adj_o_rk, adj_d_rk, SOS) ~ px(55),
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
      rows = wins == "League Average"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "floralwhite"),
    locations = cells_body(
      rows = wins == "League Average")
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
  gtsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Team Info.png", expand = 0)

plot_2 <- team_four_factors %>%
  gt() %>%
  cols_label(team = "", off_efg = "eFG%", off_efg_rank = "Rank", off_to = "Tov%", off_to_rank = "Rank", off_or = "Reb%", off_or_rank = "Rank", off_ftr = "FTr", off_ftr_rank = "Rank"
  ) %>%
  tab_header(
    title = md("Four Factors"),
    #subtitle = table_subtitle
  )  %>%
  data_color(
    columns = vars(off_efg_rank, off_to_rank, off_or_rank, off_ftr_rank),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "RColorBrewer::RdYlGn",
        direction  = -1
      ) %>% as.character(),
      domain = c(363,0),
      na.color = "#00441BFF"
    )
  ) %>%
  cols_align(
    align = "left",
    columns = vars(team, off_efg, off_efg_rank, off_to, off_to_rank, off_or, off_or_rank, off_ftr, off_ftr_rank)
  ) %>%
  cols_width(vars(team, off_efg, off_efg_rank, off_to, off_to_rank, off_or, off_or_rank, off_ftr, off_ftr_rank) ~ px(45),
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
      rows = team == "League Average"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "floralwhite"),
    locations = cells_body(
      rows = team == "League Average")
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
  gtsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Team Four Factors.png", expand = 0)

plot_3 <- team_shooting %>%
  gt() %>%
  cols_label(team = "", close_share = "Rim", rim_a_rank = "Rank", rim_per = "Rim%", rim_per_rank = "Rank", far_share = "Mid", mid_a_rank = "Rank", mid_per = "Mid%", mid_per_rank = "Rank", three_share = "3PA", three_a_rank = "Rank", three_per = "3P%", three_per_rank = "Rank"
  ) %>%
  tab_header(
    title = md("Shooting"),
    #subtitle = table_subtitle
  )  %>%
  data_color(
    columns = vars(rim_a_rank, rim_per_rank, mid_a_rank, mid_per_rank, three_a_rank, three_per_rank),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "RColorBrewer::RdYlGn",
        direction  = -1
      ) %>% as.character(),
      domain = c(363,0),
      na.color = "#00441BFF"
    )
  ) %>%
  cols_align(
    align = "left",
    columns = vars(team, close_share, rim_a_rank, rim_per, rim_per_rank, far_share, mid_a_rank, mid_per, mid_per_rank, three_share, three_a_rank, three_per, three_per_rank)
  ) %>%
  cols_width(vars(team, close_share, rim_a_rank, rim_per, rim_per_rank, far_share, mid_a_rank, mid_per, mid_per_rank, three_share, three_a_rank, three_per, three_per_rank) ~ px(45),
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
      rows = team == "League Average"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "floralwhite"),
    locations = cells_body(
      rows = team == "League Average")
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
  gtsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Team Shooting.png", expand = 0)

# plot_4 <- misc_stats %>%
#   gt() %>%
#   cols_label(tempo = "Tempo", tempo_rank = "Rank", Transition = "Transition%", Transition_Rank = "Rank", Second_Chance = "2nd %", Second_Chance_Rank = "Rank", Bench = "Bench%", Bench_Rank = "Rank",
#              Second_PPP = "2nd PPP", Second_PPP_Rank = "Rank", Second_Conv = "2nd Conv.%", Second_Conv_Rank = "Rank",
#   ) %>%
#   tab_header(
#     title = md("Misc"),
#     #subtitle = table_subtitle
#   )  %>%
#   data_color(
#     columns = vars(tempo_rank, Transition_Rank, Second_Chance_Rank, Bench_Rank, Second_PPP_Rank, Second_Conv_Rank),
#     colors = scales::col_numeric(
#       palette = paletteer::paletteer_d(
#         palette = "RColorBrewer::RdYlGn",
#         direction  = -1
#       ) %>% as.character(),
#       domain = c(358,0),
#       na.color = "#00441BFF"
#     )
#   ) %>%
#   cols_align(
#     align = "left",
#     columns = vars(tempo, tempo_rank, Transition, Transition_Rank, Second_Chance, Second_Chance_Rank,Second_PPP, Second_PPP_Rank, Second_Conv, Second_Conv_Rank, Bench, Bench_Rank)
#   ) %>%
#   cols_width(vars(tempo, tempo_rank, Transition_Rank, Second_Chance_Rank, Bench, Bench_Rank,Second_Chance,  Second_PPP_Rank, Second_Conv_Rank,) ~ px(45),
#              vars(Transition, Second_Conv, ) ~ px(75),
#              vars(Second_PPP, ) ~ px(55),
#              #vars(Successful_screen, Unsuccessful_screen) ~ px(45),
#   ) %>%
#   tab_style(
#     style = list(
#       cell_borders(
#         side =  "top",
#         color = 'gray55',
#         weight = px(2)
#       )
#     ),
#     locations = cells_body(
#       rows = tempo == "League Average"
#     )
#   ) %>%
#   tab_style(
#     style = cell_fill(color = "floralwhite"),
#     locations = cells_body(
#       rows = tempo == "League Average")
#   ) %>%
#   tab_options(
#     table.background.color = "floralwhite",
#     column_labels.font.size = 10.5,
#     table.font.size = 10,
#     heading.title.font.size  = 15,
#     heading.title.font.weight = 'bold',
#     heading.subtitle.font.size = 11,
#     table.font.names = "Consolas",
#     table.font.color = 'black',
#     #table.border.top.color = "transparent",
#     data_row.padding = px(2),
#     footnotes.font.size = 8,
#     source_notes.font.size = 9,
#     footnotes.padding = px(1),
#     #column_labels.hidden = TRUE
#   ) %>%
#   gtsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Team Misc.png", expand = 0)

plot_1 <- ggdraw() + draw_image("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Team Info.png", scale = 1)
plot_2 <- ggdraw() + draw_image("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Team Four Factors.png", scale = 1)
plot_3 <- ggdraw() + draw_image("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Team Shooting.png", scale = 1)
# plot_4 <- ggdraw() + draw_image("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Team Misc.png", scale = 1)
total_plot<-ggarrange(plot_1, plot_2, plot_3, ncol=1, nrow=3) + bgcolor("floralwhite")
#annotate_figure(total_plot, top = text_grob("Practice Leaders", color = "black", face = "bold", family = "Consolas", size = 25))
ggsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Team Profile.png")