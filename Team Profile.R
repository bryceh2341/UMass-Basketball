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

team_stats <- bart_team_box(year=2022)
teams <- bart_teams(year = 2022, conf = NULL)

team.name = "Colorado"

basic_stats <- team_stats %>%
  filter(team %in% c(teams$team)) %>%
  select(team, wins, losses)

#Possessions per game, 

team_shooting <- bart_team_shooting(year = 2022, conf = NULL, team = NULL)

team_shooting <- separate(team_shooting, col=close_fg, into=c('rim_makes', 'rim_attempts'), sep='-')
team_shooting <- separate(team_shooting, col=far_fg, into=c('mid_makes', 'mid_attempts'), sep='-')
team_shooting <- separate(team_shooting, col=three_fg, into=c('three_makes', 'three_attempts'), sep='-')
team_shooting <- separate(team_shooting, col=close_fg_d, into=c('rim_makes_d', 'rim_attempts_d'), sep='-')
team_shooting <- separate(team_shooting, col=far_fg_d, into=c('mid_makes_d', 'mid_attempts_d'), sep='-')
team_shooting <- separate(team_shooting, col=three_fg_d, into=c('three_makes_d', 'three_attempts_d'), sep='-')

team_shooting[c('rim_makes', 'rim_attempts', 'mid_makes', 'mid_attempts', 'three_makes', 'three_attempts', 'rim_makes_d', 'rim_attempts_d', 'mid_makes_d', 'mid_attempts_d', 'three_makes_d', 'three_attempts_d')] <− lapply(team_shooting[c('rim_makes', 'rim_attempts', 'mid_makes', 'mid_attempts', 'three_makes', 'three_attempts', 'rim_makes_d', 'rim_attempts_d', 'mid_makes_d', 'mid_attempts_d', 'three_makes_d', 'three_attempts_d')],as.numeric)

team_shooting <- team_shooting %>%
  filter(team %in% c(teams$team)) %>%
  mutate(rim_per = rim_makes/rim_attempts,
         mid_per = mid_makes/mid_attempts,
         three_per = three_makes/three_attempts,
         rim_per_d = rim_makes_d/rim_attempts_d,
         mid_per_d = mid_makes_d/mid_attempts_d,
         three_per_d = three_makes_d/three_attempts_d,
         rim_per_rank = round(rank(desc(rim_per)),0),
         mid_per_rank = round(rank(desc(mid_per)),0),
         three_per_rank = round(rank(desc(three_per)),0),
         rim_per_d_rank = round(rank(desc(rim_per_d)),0),
         mid_per_d_rank = round(rank(desc(mid_per_d)),0),
         three_per_d_rank = round(rank(desc(three_per_d)),0),
         rim_a_rank = round(rank(desc(close_share)),0),
         mid_a_rank = round(rank(desc(far_share)),0),
         three_a_rank = round(rank(desc(three_share)),0),
         rim_a_d_rank = round(rank(close_share_d),0),
         mid_a_d_rank = round(rank(far_share_d),0),
         three_a_d_rank = round(rank(three_share_d),0),
         ) %>%
  select(team, close_share, rim_a_rank, rim_per, rim_per_rank, far_share, mid_a_rank, mid_per, mid_per_rank, three_share, three_a_rank, three_per, three_per_rank, close_share_d, rim_a_d_rank, rim_per_d, rim_per_d_rank, far_share_d, mid_a_d_rank, mid_per_d, mid_per_d_rank, three_share_d, three_a_d_rank, three_per_d, three_per_d_rank) %>%
  filter(team == team.name)
team_shooting[nrow(team_shooting) + 1,] <-  list("Offense", team_shooting$close_share[1], team_shooting$rim_a_rank[1], round(team_shooting$rim_per[1]*100,1), team_shooting$rim_per_rank[1], team_shooting$far_share[1], team_shooting$mid_a_rank[1], round(team_shooting$mid_per[1]*100), team_shooting$mid_per_rank[1], team_shooting$three_share[1], team_shooting$three_a_rank[1], round(team_shooting$three_per[1]*100), team_shooting$three_per_rank[1],0,0,0,0,0,0,0,0,0,0,0,0)
team_shooting[nrow(team_shooting) + 1,] <-  list("Defense", team_shooting$close_share_d[1], team_shooting$rim_a_d_rank[1], round(team_shooting$rim_per_d[1]*100,1), team_shooting$rim_per_d_rank[1], team_shooting$far_share_d[1], team_shooting$mid_a_d_rank[1], round(team_shooting$mid_per_d[1]*100,1), team_shooting$mid_per_d_rank[1], team_shooting$three_share_d[1], team_shooting$three_a_d_rank[1], round(team_shooting$three_per_d[1]*100,1), team_shooting$three_per_d_rank[1],0,0,0,0,0,0,0,0,0,0,0,0)

team_shooting <- team_shooting %>%
  slice(2:3) %>%
  select(team, close_share, rim_a_rank, rim_per, rim_per_rank, far_share, mid_a_rank, mid_per, mid_per_rank, three_share, three_a_rank, three_per, three_per_rank)

team_four_factors <- bart_factors(year=2022)

team_tempo <- team_four_factors %>%
  select(team, tempo)
team_tempo <- data.frame(team = team_tempo$team, tempo = team_tempo$tempo)


team_four_factors <- team_four_factors %>%
  mutate(off_efg_rank = round(rank(desc(off_efg)),0), 
         off_to_rank = round(rank(off_to),0), 
         off_or_rank = round(rank(desc(off_or)),0), 
         off_ftr_rank = round(rank(desc(off_ftr)),0), 
         def_efg_rank = round(rank(def_efg),0), 
         def_to_rank = round(rank(desc(def_to)),0),
         def_or_rank = round(rank(def_or),0), 
         def_ftr_rank = round(rank(def_ftr),0)) %>%
  select(team, off_efg, off_efg_rank, off_to, off_to_rank, off_or, off_or_rank, off_ftr, off_ftr_rank, def_efg, def_efg_rank, def_to, def_to_rank, def_or, def_or_rank, def_ftr, def_ftr_rank) %>%
  filter(team == team.name)
team_four_factors[nrow(team_four_factors)+1,] <- list("Offense", team_four_factors$off_efg[1], team_four_factors$off_efg_rank[1], team_four_factors$off_to[1], team_four_factors$off_to_rank[1], team_four_factors$off_or[1], team_four_factors$off_or_rank[1], team_four_factors$off_ftr[1], team_four_factors$off_ftr_rank[1],0,0,0,0,0,0,0,0)
team_four_factors[nrow(team_four_factors)+1,] <- list("Defense", team_four_factors$def_efg[1], team_four_factors$def_efg_rank[1], team_four_factors$def_to[1], team_four_factors$def_to_rank[1], team_four_factors$def_or[1], team_four_factors$def_or_rank[1], team_four_factors$def_ftr[1], team_four_factors$def_ftr_rank[1],0,0,0,0,0,0,0,0)

misc_stats <- team_stats %>%
  filter(team %in% c(teams$team)) %>%
  mutate(Transition = round(fast_brk_pts/pts*100,1),
         Second_Chance = round(second_chance_pts/pts*100,1),
         Second_PPP = round(second_chance_pts/oreb,2),
         Second_Conv = round(second_chance_fgm/(second_chance_fga+(team_four_factors$off_to[1]/100*second_chance_fga))*100,1),
         #Second_Conv = round(second_chance_fgm/oreb*100,1),
         Bench = round(bench_pts/pts*100,1),
         Transition_Rank = round(rank(desc(Transition)),0),
         Second_Chance_Rank = round(rank(desc(Second_Chance)),0),
         Second_PPP_Rank = round(rank(desc(Second_PPP)),0),
         Second_Conv_Rank = round(rank(desc(Second_Conv)),0),
         Bench_Rank = round(rank(desc(Bench)),0)
  ) %>%
  select(team, Transition, Second_Chance, Second_PPP, Second_Conv, Bench, Transition_Rank, Second_Chance_Rank, Second_PPP_Rank, Second_Conv_Rank, Bench_Rank)

misc_stats <- merge(team_tempo, misc_stats, by="team")

misc_stats <- misc_stats %>%
  mutate(tempo_rank = round(rank(desc(tempo)),0)
  ) %>%
  filter(team == team.name) %>%
  select(tempo, tempo_rank, Transition, Transition_Rank, Second_Chance, Second_Chance_Rank, Second_PPP, Second_PPP_Rank, Second_Conv, Second_Conv_Rank, Bench, Bench_Rank)



team_four_factors <- team_four_factors %>%
  slice(2:3) %>%
  select(team, off_efg, off_efg_rank, off_to, off_to_rank, off_or, off_or_rank, off_ftr, off_ftr_rank)

team_rating <- bart_ratings(year = 2022)


team_rating <- team_rating %>%
  mutate (SOS = round(rank(desc(ov_cur_sos)),0)) %>%
  filter(team == team.name) %>%
  select(team, conf, barthag_rk, adj_o_rk, adj_d_rk, SOS)

team_rating <- merge(basic_stats, team_rating, by="team")
team_rating <- team_rating %>%
  select(wins, losses, conf, barthag_rk, adj_o_rk, adj_d_rk, SOS)

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
      domain = c(358,0),
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
      domain = c(358,0),
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
      domain = c(358,0),
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

plot_4 <- misc_stats %>%
  gt() %>%
  cols_label(tempo = "Tempo", tempo_rank = "Rank", Transition = "Transition%", Transition_Rank = "Rank", Second_Chance = "2nd %", Second_Chance_Rank = "Rank", Bench = "Bench%", Bench_Rank = "Rank",
             Second_PPP = "2nd PPP", Second_PPP_Rank = "Rank", Second_Conv = "2nd Conv.%", Second_Conv_Rank = "Rank",
  ) %>%
  tab_header(
    title = md("Misc"),
    #subtitle = table_subtitle
  )  %>%
  data_color(
    columns = vars(tempo_rank, Transition_Rank, Second_Chance_Rank, Bench_Rank, Second_PPP_Rank, Second_Conv_Rank),
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
    columns = vars(tempo, tempo_rank, Transition, Transition_Rank, Second_Chance, Second_Chance_Rank,Second_PPP, Second_PPP_Rank, Second_Conv, Second_Conv_Rank, Bench, Bench_Rank)
  ) %>%
  cols_width(vars(tempo, tempo_rank, Transition_Rank, Second_Chance_Rank, Bench, Bench_Rank,Second_Chance,  Second_PPP_Rank, Second_Conv_Rank,) ~ px(45),
             vars(Transition, Second_Conv, ) ~ px(75),
             vars(Second_PPP, ) ~ px(55),
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
      rows = tempo == "League Average"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "floralwhite"),
    locations = cells_body(
      rows = tempo == "League Average")
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
  gtsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Team Misc.png", expand = 0)

plot_1 <- ggdraw() + draw_image("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Team Info.png", scale = 1)
plot_2 <- ggdraw() + draw_image("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Team Four Factors.png", scale = 1)
plot_3 <- ggdraw() + draw_image("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Team Shooting.png", scale = 1)
plot_4 <- ggdraw() + draw_image("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Team Misc.png", scale = 1)
total_plot<-ggarrange(plot_1, plot_2, plot_3, plot_4, ncol=1, nrow=4) + bgcolor("floralwhite")
#annotate_figure(total_plot, top = text_grob("Practice Leaders", color = "black", face = "bold", family = "Consolas", size = 25))
ggsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Team Profile.png")