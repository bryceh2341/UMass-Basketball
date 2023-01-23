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
library(gt)
library(cowplot)
library(ggpubr)
library(dplyr)
library(gtExtras)

setwd("C:/Users/Bryce Haase/Desktop/UMass Basketball")

team_name <- "Massachusetts"

schedule <- get_team_schedule(team.name = team_name, season = "2022-23")

games <- schedule$Game_ID[c(2, 3, 5, 6, 7, 9, 11, 13)]

stats <- get_player_stats(play_by_play_data = get_play_by_play(games), multi.games = T, simple = F)

games <- schedule$Game_ID[c(1, 4, 8, 10, 12)]

other_stats <- get_player_stats(play_by_play_data = get_play_by_play(games), multi.games = T, simple = F)

stats <- stats %>%
  filter(Player == "TJ.WEEKS") %>%
  mutate(MPG = round(MINS/GP,1),
         PPG = round(PTS/GP, 1),
         FG = paste0(FGM, "-", FGA),
         FG_Per = round(FG.*100, 0),
         THREE = paste0(TPM,"-",TPA),
         THREE_Per = round(TP.*100, 0),
         FT = paste0(FTM, "-", FTA),
         FT_Per = round(FT.*100, 0),
         OReb = round(ORB/GP, 1),
         DReb = round(DRB/GP, 1),
         Reb = round((ORB+DRB)/GP, 1),
         Ast = round(AST/GP, 1),
         Fouls = round(PF/GP, 1),
         Tov = round(TOV/GP,1),
         Stl = round(STL/GP,1),
         Blk = round(BLK/GP,1)
         ) %>%
  select(Player, GP, MPG, PPG, FG, FG_Per, THREE, THREE_Per, FT, FT_Per, OReb, DReb, Reb, Ast, Fouls, Tov, Stl, Blk)

other_stats <- other_stats %>%
  filter(Player == "TJ.WEEKS") %>%
  mutate(MPG = round(MINS/GP,1),
         PPG = round(PTS/GP, 1),
         FG = paste0(FGM, "-", FGA),
         FG_Per = round(FG.*100, 0),
         THREE = paste0(TPM,"-",TPA),
         THREE_Per = round(TP.*100, 0),
         FT = paste0(FTM, "-", FTA),
         FT_Per = round(FT.*100, 0),
         OReb = round(ORB/GP, 1),
         DReb = round(DRB/GP, 1),
         Reb = round((ORB+DRB)/GP, 1),
         Ast = round(AST/GP, 1),
         Fouls = round(PF/GP, 1),
         Tov = round(TOV/GP,1),
         Stl = round(STL/GP,1),
         Blk = round(BLK/GP,1)
  ) %>%
  select(Player, GP, MPG, PPG, FG, FG_Per, THREE, THREE_Per, FT, FT_Per, OReb, DReb, Reb, Ast, Fouls, Tov, Stl, Blk)

total_stats <- rbind(stats, other_stats)
total_stats <- total_stats %>%
  mutate(Games = c("St. Bona, N. Texas, Lowell, Harvard, USF, Charlotte, Colorado, Towson", "CCSU, Murray St, Albany, Hofstra, Dartmouth")) %>%
  select(Games, GP, MPG, PPG, FG, FG_Per, THREE, THREE_Per, FT, FT_Per, OReb, DReb, Reb, Ast, Fouls, Tov, Stl, Blk)

total_stats %>%
  gt() %>%
  cols_label(Games = "Games", 
             GP = "GP", 
             MPG = "Mins", 
             PPG = "PPG",
             FG = "FGM-FGA", 
             FG_Per = "FG%", 
             THREE = "3PM-3PA", 
             THREE_Per = "3%", 
             FT = "FTM-FTA", 
             FT_Per = "FT%", 
             OReb = "OReb", 
             DReb = "Dreb", 
             Reb = "Reb", 
             Ast = "Ast",
             Fouls = "PF", 
             Tov = "Tov", 
             Stl = "Stl", 
             Blk = "Blk"
  ) %>%
  tab_header(
    title = md("TJ Weeks Stats"),
    #subtitle = table_subtitle
  )  %>%
  cols_align(
    align = "left",
    columns = vars(Games, GP, MPG, PPG, FG, FG_Per, THREE, THREE_Per, FT, FT_Per, OReb, DReb, Reb, Ast, Fouls, Tov, Stl, Blk)
  ) %>%
  cols_width(vars(GP, MPG, PPG, FG_Per, THREE_Per, FT_Per, OReb, DReb, Reb, Ast, Fouls, Tov, Stl, Blk) ~ px(38),
             vars(Games) ~ px(215),
             vars(FG, THREE, FT, ) ~ px(55),
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
      rows = GP == "League Average"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "white"),
    locations = cells_body(
      rows = GP == "League Average")
  ) %>%
  tab_options(
    table.background.color = "white",
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
  gtsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\TJ Weeks Stats.png")