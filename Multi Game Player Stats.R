library(hoopR)
library(bigballR)
library(ncaahoopR)
library(gt)
library(cowplot)
library(ggpubr)

ncaa_game_id <- c(5366223, 5364247, 5363492)

setwd("C:/Users/Bryce Haase/Desktop/UMass Basketball")

#team_stats <- get_team_stats(play_by_play_data = get_play_by_play(ncaa_game_id), include_transition = T)
player_stats <- get_player_stats(get_play_by_play(ncaa_game_id), multi.games = T, simple = F)

home_basic <- player_stats %>%
  filter(Team == "Massachusetts") %>%
  mutate(MINS = round(MINS,1),
         REB = ORB+DRB,
         FG = paste0(FGM,"/",FGA),
         TP = paste0(TPM,"/",TPA),
         FG. = round(FG.*100,1),
         TP. = round(TP.*100,1)
  ) %>%
  select(Player, MINS, PTS, REB, AST, FG, FG., TP, TP., STL, BLK, TOV, PF, ORB, DRB)
home_basic <- merge(home_basic, home_roster, by="Player")
home_basic <- home_basic %>%
  select(player_image, CleanName, MINS, PTS, REB, AST, FG, FG., TP, TP., STL, BLK, TOV, PF, ORB, DRB) %>%
  arrange(desc(MINS))

home_basic %>%
  gt() %>%
  cols_label(player_image = "", CleanName = "Player", MINS = "Mins", PTS = "Pts", REB = "Reb",
             AST = "Ast", FG = "FG", FG. = "FG%", TP = "3pt", TP. = "3pt%", STL = "Stl", BLK = "Blk",
             TOV = "Tov", PF = "PF", ORB = "OReb", DRB = "DReb"
  ) %>%
  tab_header(
    title = md(paste0(player_stats$Home[1]," Basic Stats")),
    # subtitle = table_subtitle
  )  %>%
  text_transform(
    locations = cells_body(vars(player_image)),
    fn = function(x) {
      web_image(url = x,
                height = px(22.5))
    }
  ) %>%
  data_color(
    columns = c(PTS),
    #rows = where(is.numeric),
    #rows = c(full_practice_data[1:14,]),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(TP.),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(FG.),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_align(
    align = "left",
    columns = vars(player_image, CleanName, MINS, PTS, REB, AST, FG, FG., TP, TP., STL, BLK, TOV, PF, ORB, DRB)
  ) %>%
  cols_width(vars(player_image, MINS, PTS, REB, AST, FG, FG., TP, TP., STL, BLK, TOV, PF, ORB, DRB) ~ px(38),
             vars(CleanName) ~ px(115),
             #vars(Charges, Rim, Three, Mid,) ~ px(45),
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
      rows = CleanName == "League Average"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "floralwhite"),
    locations = cells_body(
      rows = CleanName == "League Average")
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
  gtsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Basic Player Home.png")

home_shooting <- player_stats %>%
  filter(Team == "Massachusetts",
         FGA > 0) %>%
  mutate(TS. = round(TS.*100,1),
         eFG. = round(eFG.*100,1),
         RIM = paste0(RIMM, "/", RIMA),
         RIM. = round(RIM.*100,1),
         MID = paste0(MIDM, "/", MIDA),
         MID. = round(MID.*100,1),
         TP = paste0(TPM, "/", TPA),
         TP. = round(TP.*100,1),
         FT = paste0(FTM, "/", FTA),
         FT. = round(FT.*100,1),
  ) %>%
  select(Player, PTS, TS., eFG., RIM, RIM., MID, MID., TP, TP., FT, FT.)
home_shooting <- merge(home_shooting, home_roster, by="Player")
home_shooting <- home_shooting %>%
  select(player_image, CleanName, PTS, TS., eFG., RIM, RIM., MID, MID., TP, TP., FT, FT.) %>%
  arrange(desc(PTS))

home_shooting %>%
  gt() %>%
  cols_label(player_image = "", CleanName = "Player", PTS = "Pts", TS. = "TS%", eFG. = "eFG%", 
             RIM = "Rim", RIM. = "Rim%", MID = "Mid", MID. = "Mid%", TP = "3pt", TP. = "3pt%", FT = "FT", FT. = "FT%"
  ) %>%
  tab_header(
    title = md(paste0(player_stats$Home[1]," Shooting Stats")),
    # subtitle = table_subtitle
  )  %>%
  text_transform(
    locations = cells_body(vars(player_image)),
    fn = function(x) {
      web_image(url = x,
                height = px(22.5))
    }
  ) %>%
  data_color(
    columns = c(PTS),
    #rows = where(is.numeric),
    #rows = c(full_practice_data[1:14,]),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(TS.),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(eFG.),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(RIM.),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(MID.),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(TP.),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(FT.),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_align(
    align = "left",
    columns = vars(player_image, CleanName, PTS, TS., eFG., RIM, RIM., MID, MID., TP, TP., FT, FT.)
  ) %>%
  cols_width(vars(player_image, PTS, TS., eFG., RIM, RIM., MID, MID., TP, TP., FT, FT.) ~ px(38),
             vars(CleanName) ~ px(115),
             #vars(Charges, Rim, Three, Mid,) ~ px(45),
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
      rows = CleanName == "League Average"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "floralwhite"),
    locations = cells_body(
      rows = CleanName == "League Average")
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
  gtsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Shooting Player Home.png")

home_lineups <- lineups %>%
  filter(Team == "Massachusetts") %>%
  arrange(desc(Mins)) %>%
  mutate(ORTG = round(ORTG,1),
         DRTG = round(DRTG,1),
         NETRTG = round(NETRTG,1),
         Mins = round(Mins,0),
         #P1 = gsub(".", " ", P1, fixed=TRUE),
         P1 = sub("^\\S+\\s+", '', gsub(".", " ", P1, fixed=TRUE)),
         P2 = sub("^\\S+\\s+", '', gsub(".", " ", P2, fixed=TRUE)),
         P3 = sub("^\\S+\\s+", '', gsub(".", " ", P3, fixed=TRUE)),
         P4 = sub("^\\S+\\s+", '', gsub(".", " ", P4, fixed=TRUE)),
         P5 = sub("^\\S+\\s+", '', gsub(".", " ", P5, fixed=TRUE)),
         Lineup = paste0(P1, " - ", P2," - ", P3," - ", P4," - ", P5)) %>%
  select(Lineup, Mins, PTS, dPTS, ORTG, DRTG, NETRTG) %>%
  filter(Mins > 0)

home_lineups %>%
  gt() %>%
  cols_label(Lineup = "Lineup", Mins = "Mins", PTS = "Pts", dPTS = "Opp. Pts", ORTG = "ORtg.", DRTG = "DRtg.", NETRTG = "Net Rtg."
  ) %>%
  tab_header(
    title = md(paste0(player_stats$Home[1], " Lineups")),
    #subtitle = table_subtitle
  )  %>%
  data_color(
    columns = c(NETRTG),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "RColorBrewer::RdYlGn",
        direction  = 1
      ) %>% as.character(),
      domain = c(min(lineups$NETRTG),max(lineups$NETRTG)),
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
      domain = c(min(lineups$ORTG),max(lineups$ORTG)),
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
      domain = c(min(lineups$DRTG),max(lineups$DRTG)),
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
             vars(NETRTG, dPTS) ~ px(60),
             vars(ORTG, DRTG) ~ px(45),
             vars(Mins, PTS,) ~ px(38),
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
  gtsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Home Lineups.png", expand = 0)

