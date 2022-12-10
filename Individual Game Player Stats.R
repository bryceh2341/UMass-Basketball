library(hoopR)
library(bigballR)
library(ncaahoopR)
library(gt)
library(cowplot)
library(ggpubr)
library(dplyr)

setwd("C:/Users/Bryce Haase/Desktop/UMass Basketball")


ncaa_game_id <- 5366223

full_player_stats <- read.csv("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\2021_22_player_stats.csv")


player_stats <- get_player_stats(get_play_by_play(ncaa_game_id), multi.games = F, simple = F)


#home_roster <- ncaahoopR::get_roster(player_stats$Home[1], season = "2022-23")
home_roster <- ncaahoopR::get_roster("Charlotte", season = "2022-23")
home_roster <- home_roster %>%
  mutate(Jersey = number) %>%
  select(Jersey, player_image)

home_roster <- merge(home_roster, get_team_roster(season = "2022-23", team.name = player_stats$Home[1]), by="Jersey")

#away_roster <- ncaahoopR::get_roster(player_stats$Away[1], season = "2022-23")
away_roster <- ncaahoopR::get_roster("UMass", season = "2022-23")
away_roster <- away_roster %>%
  mutate(Jersey = number) %>%
  select(Jersey, player_image)

away_roster <- merge(away_roster, get_team_roster(season = "2022-23", team.name = player_stats$Away[1]), by="Jersey")

home_basic <- player_stats %>%
  filter(Home == Team) %>%
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


away_basic <- player_stats %>%
  filter(Away == Team) %>%
  mutate(MINS = round(MINS,1),
         REB = ORB+DRB,
         FG = paste0(FGM,"/",FGA),
         TP = paste0(TPM,"/",TPA),
         FG. = round(FG.*100,1),
         TP. = round(TP.*100,1)
  ) %>%
  select(Player, MINS, PTS, REB, AST, FG, FG., TP, TP., STL, BLK, TOV, PF, ORB, DRB)
away_basic <- merge(away_basic, away_roster, by="Player")
away_basic <- away_basic %>%
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

away_basic %>%
  gt() %>%
  cols_label(player_image = "", CleanName = "Player", MINS = "Mins", PTS = "Pts", REB = "Reb",
             AST = "Ast", FG = "FG", FG. = "FG%", TP = "3pt", TP. = "3pt%", STL = "Stl", BLK = "Blk",
             TOV = "Tov", PF = "PF", ORB = "OReb", DRB = "DReb"
  ) %>%
  tab_header(
    title = md(paste0(player_stats$Away[1]," Basic Stats")),
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
  gtsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Basic Player Away.png")


home_shooting <- player_stats %>%
  filter(Home == Team,
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

away_shooting <- player_stats %>%
  filter(Away == Team,
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
away_shooting <- merge(away_shooting, away_roster, by="Player")
away_shooting <- away_shooting %>%
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


away_shooting %>%
  gt() %>%
  cols_label(player_image = "", CleanName = "Player", PTS = "Pts", TS. = "TS%", eFG. = "eFG%", 
             RIM = "Rim", RIM. = "Rim%", MID = "Mid", MID. = "Mid%", TP = "3pt", TP. = "3pt%", FT = "FT", FT. = "FT%"
  ) %>%
  tab_header(
    title = md(paste0(player_stats$Away[1]," Shooting Stats")),
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
  gtsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Shooting Player Away.png")

home_advanced <- player_stats %>%
  filter(Home == Team,
         FGA > 0) %>%
  mutate(
    Pts_Created = PTS_unast,
    Created_Prop = round(Pts_Created/PTS*100, 1),
    Pts_Added = round((((RIM.-mean(full_player_stats$RIM.[full_player_stats$RIMA>=20]))*RIMA + (MID.-mean(full_player_stats$MID.[full_player_stats$MIDA>=20]))*MIDA + (TP.-mean(full_player_stats$TP.[full_player_stats$TPA>=20]))*TPA) + (FT.-mean(full_player_stats$FT.[full_player_stats$FTA>=20]))*FTA),1),
    SQ = round((mean(full_player_stats$RIM.[full_player_stats$RIMA>=20])*RIMA/FGA + mean(full_player_stats$MID.[full_player_stats$MIDA>=20])*MIDA/FGA + mean(full_player_stats$TP.[full_player_stats$TPA>=20])*TPA/FGA*1.5)*100,1),
    Shot_Making = round((eFG.*100-SQ),1),
    Trans_Prop = round(PTS_trans/PTS*100,1),
    HC_Prop = round(PTS_half/PTS*100,1),
  ) %>%
  select(Player, Pts_Added, Pts_Created, Created_Prop, SQ, Shot_Making, PTS_trans, Trans_Prop, PTS_half, HC_Prop)
home_advanced[is.na(home_advanced)] = 0.0
home_advanced <- merge(home_advanced, home_roster, by="Player")
home_advanced <- home_advanced %>%
  select(player_image, CleanName, Pts_Added, Pts_Created, Created_Prop, SQ, Shot_Making,  PTS_trans, Trans_Prop, PTS_half, HC_Prop) %>%
  arrange(desc(Pts_Added))

away_advanced <- player_stats %>%
  filter(Away == Team,
         FGA > 0) %>%
  mutate(
    Pts_Created = PTS_unast,
    Created_Prop = round(Pts_Created/PTS*100, 1),
    Pts_Added = round((((RIM.-mean(full_player_stats$RIM.[full_player_stats$RIMA>=20]))*RIMA + (MID.-mean(full_player_stats$MID.[full_player_stats$MIDA>=20]))*MIDA + (TP.-mean(full_player_stats$TP.[full_player_stats$TPA>=20]))*TPA) + (FT.-mean(full_player_stats$FT.[full_player_stats$FTA>=20]))*FTA),1),
    SQ = round((mean(full_player_stats$RIM.[full_player_stats$RIMA>=20])*RIMA/FGA + mean(full_player_stats$MID.[full_player_stats$MIDA>=20])*MIDA/FGA + mean(full_player_stats$TP.[full_player_stats$TPA>=20])*TPA/FGA*1.5)*100,1),
    Shot_Making = round((eFG.*100-SQ),1),
    Trans_Prop = round(PTS_trans/PTS*100,1),
    HC_Prop = round(PTS_half/PTS*100,1),
  ) %>%
  select(Player, Pts_Added, Pts_Created, Created_Prop, SQ, Shot_Making, PTS_trans, Trans_Prop, PTS_half, HC_Prop)
away_advanced[is.na(away_advanced)] = 0.0
away_advanced <- merge(away_advanced, away_roster, by="Player")
away_advanced <- away_advanced %>%
  select(player_image, CleanName, Pts_Added, Pts_Created, Created_Prop, SQ, Shot_Making,  PTS_trans, Trans_Prop, PTS_half, HC_Prop) %>%
  arrange(desc(Pts_Added))

home_advanced %>%
  gt() %>%
  cols_label(player_image = "", CleanName = "Player", Pts_Added = "Points Added", Pts_Created = "Created Pts", Created_Prop = "% of Pts Created", SQ = "Shot Quality", 
             Shot_Making = "Shot Making",  PTS_trans = "Transition Pts", Trans_Prop = "% of Pts in Transition", PTS_half = "Halfcourt Pts", HC_Prop = "% of Pts in HC"
  ) %>%
  tab_header(
    title = md(paste0(player_stats$Home[1]," Advanced Stats")),
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
    columns = c(Pts_Created),
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
    columns = c(Shot_Making),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(Pts_Added),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_align(
    align = "left",
    columns = vars(player_image, CleanName, Pts_Added, Pts_Created, Created_Prop, SQ, Shot_Making,  PTS_trans, Trans_Prop, PTS_half, HC_Prop)
  ) %>%
  cols_width(vars(player_image, ) ~ px(35),
             vars(CleanName) ~ px(115),
             vars(Pts_Created, Pts_Added, Created_Prop, SQ, Shot_Making,  PTS_trans, Trans_Prop, PTS_half, HC_Prop) ~ px(75),
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
  gtsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Advanced Player Home.png")

away_advanced %>%
  gt() %>%
  cols_label(player_image = "", CleanName = "Player", Pts_Added = "Pts Added", Pts_Created = "Created Pts", Created_Prop = "% of Pts Created", SQ = "Shot Quality", 
             Shot_Making = "Shot Making",  PTS_trans = "Transition Pts", Trans_Prop = "% of Pts in Transition", PTS_half = "Halfcourt Pts", HC_Prop = "% of Pts in HC"
  ) %>%
  tab_header(
    title = md(paste0(player_stats$Away[1]," Advanced Stats")),
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
    columns = c(Pts_Created),
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
    columns = c(Shot_Making),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(Pts_Added),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_align(
    align = "left",
    columns = vars(player_image, CleanName, Pts_Added, Pts_Created, Created_Prop, SQ, Shot_Making,  PTS_trans, Trans_Prop, PTS_half, HC_Prop)
  ) %>%
  cols_width(vars(player_image, ) ~ px(35),
             vars(CleanName) ~ px(115),
             vars(Pts_Added, Pts_Created, Created_Prop, SQ, Shot_Making,  PTS_trans, Trans_Prop, PTS_half, HC_Prop) ~ px(75),
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
  gtsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Advanced Player Away.png")

lineups <- get_lineups(play_by_play_data = get_play_by_play(ncaa_game_id), include_transition = T)


home_lineups <- lineups %>%
  filter(Team == player_stats$Home[1]) %>%
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

away_lineups <- lineups %>%
  filter(Team == player_stats$Away[1]) %>%
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

away_lineups %>%
  gt() %>%
  cols_label(Lineup = "Lineup", Mins = "Mins",PTS = "Pts", dPTS = "Opp. Pts", ORTG = "ORtg.", DRTG = "DRtg.", NETRTG = "Net Rtg."
  ) %>%
  tab_header(
    title = md(paste0(player_stats$Away[1], " Lineups")),
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
  gtsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Away Lineups.png", expand = 0)