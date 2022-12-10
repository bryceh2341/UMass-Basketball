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

setwd("C:/Users/Bryce Haase/Desktop/UMass Basketball")

team <- "UMass Lowell"
team_schedule <- get_team_schedule(season = "2021-22", team.name = team)
team_schedule <- team_schedule[-c(13:14),]

player_names <- c("ANTHONY.BLUNT", "ALLIN.BLUNT", "AYINDE.HIKIM", "EVERETTE.HAMMOND")

for (i in 1:4) {
  for (j in player_names) {
    new <- rep(0, nrow(team_schedule))
    team_schedule[ , ncol(team_schedule) + 1] <- new
    colnames(team_schedule)[ncol(team_schedule)] <- paste0(j, i+1)
  }
}
for (i in 1:4){
  for (k in player_names) {
    new <- rep(0, nrow(team_schedule))
    team_schedule[, ncol(team_schedule) + 1] <- new
    colnames(team_schedule)[ncol(team_schedule)] <- paste0(k, i+1, "_mins_out")
  }
}
for (i in 1:4){
  for (k in player_names) {
    new <- rep(0, nrow(team_schedule))
    team_schedule[, ncol(team_schedule) + 1] <- new
    colnames(team_schedule)[ncol(team_schedule)] <- paste0(k, i+1, "_plus_minus_out")
  }
}



for (j in 1:length(team_schedule$Date)){
  play_by_play <- get_play_by_play(team_schedule$Game_ID[j])
  for (m in player_names) {
    new <- rep(0, nrow(play_by_play))
    play_by_play[ , ncol(play_by_play) + 1] <- new
    colnames(play_by_play)[ncol(play_by_play)] <- paste0(m,"_Fouls") }
  row_num<-0
  for (n in player_names) {
    count<-0
    two_out <- 0
    three_out <- 0
    four_out <- 0
    five_out <- 0
    two_points <- 0
    three_points <- 0
    four_points <- 0
    five_points <- 0
    for (i in 1:length(play_by_play$ID)) {
      play_by_play[i,row_num+36] <- count
      if (play_by_play$Event_Type[i] == "Commits Foul" && play_by_play$Player_1[i] == n) {
        count=count+1
        play_by_play[i,row_num+36] <- count
        if (count == 2 && play_by_play$Game_Seconds[i] < 1200) {
          team_schedule[j,10+row_num] <- 1
        }
        else if (count == 3 && play_by_play$Game_Seconds[i] < 1200) {
          team_schedule[j,10+row_num+length(player_names)] <- 1
          team_schedule[j,10+row_num] <- 0
        }
        else if (count == 4) {
          team_schedule[j,10+row_num+length(player_names)*2] <- 1
        }
        else if (count == 5) {
          team_schedule[j,10+row_num+length(player_names)*3] <- 1
          team_schedule[j,10+row_num+length(player_names)*2] <- 0
        }
        
      }
      play_by_play$Shot_Value[is.na(play_by_play$Shot_Value)] <- 0
      if (count == 2 && play_by_play$Game_Seconds[i] < 1200 && !(n %in% c(play_by_play$Home.1[i],play_by_play$Home.2[i],play_by_play$Home.3[i],play_by_play$Home.4[i],play_by_play$Home.5[i],play_by_play$Away.1[i],play_by_play$Away.2[i],play_by_play$Away.3[i],play_by_play$Away.4[i],play_by_play$Away.5[i]))) {
        two_out <- play_by_play$Event_Length[i]+two_out
        team_schedule[j,10+length(player_names)*4+row_num] <- two_out
        if (play_by_play$Event_Team[i] == team && play_by_play$Shot_Value[i] > 0) {
          two_points <- two_points + play_by_play$Shot_Value[i]
          team_schedule[j,10+length(player_names)*8+row_num] <- two_points
        }
        else if (play_by_play$Event_Team[i] != team && play_by_play$Shot_Value[i] > 0) {
          two_points <- two_points - play_by_play$Shot_Value[i]
          team_schedule[j,10+length(player_names)*8+row_num] <- two_points
        }
      }
      else if (count == 3 && play_by_play$Game_Seconds[i] < 1200 && !(n %in% c(play_by_play$Home.1[i],play_by_play$Home.2[i],play_by_play$Home.3[i],play_by_play$Home.4[i],play_by_play$Home.5[i],play_by_play$Away.1[i],play_by_play$Away.2[i],play_by_play$Away.3[i],play_by_play$Away.4[i],play_by_play$Away.5[i]))) {
        three_out <- play_by_play$Event_Length[i]+three_out
        team_schedule[j,10+length(player_names)*5+row_num] <- three_out
        if (play_by_play$Event_Team[i] == team && play_by_play$Shot_Value[i] > 0) {
          three_points <- three_points + play_by_play$Shot_Value[i]
          team_schedule[j,10+length(player_names)*9+row_num] <- three_points
        }
        else if (play_by_play$Event_Team[i] != team && play_by_play$Shot_Value[i] > 0) {
          three_points <- three_points - play_by_play$Shot_Value[i]
          team_schedule[j,10+length(player_names)*9+row_num] <- three_points
        }
      }
      else if (count == 4 && !(n %in% c(play_by_play$Home.1[i],play_by_play$Home.2[i],play_by_play$Home.3[i],play_by_play$Home.4[i],play_by_play$Home.5[i],play_by_play$Away.1[i],play_by_play$Away.2[i],play_by_play$Away.3[i],play_by_play$Away.4[i],play_by_play$Away.5[i]))) {
        four_out <- play_by_play$Event_Length[i]+four_out
        team_schedule[j,10+length(player_names)*6+row_num] <- four_out
        if (play_by_play$Event_Team[i] == team && play_by_play$Shot_Value[i] > 0) {
          four_points <- four_points + play_by_play$Shot_Value[i]
          team_schedule[j,10+length(player_names)*10+row_num] <- four_points
        }
        else if (play_by_play$Event_Team[i] != team && play_by_play$Shot_Value[i] > 0) {
          four_points <- four_points - play_by_play$Shot_Value[i]
          team_schedule[j,10+length(player_names)*10+row_num] <- four_points
        }
      }
      else if (count == 5 && !(n %in% c(play_by_play$Home.1[i],play_by_play$Home.2[i],play_by_play$Home.3[i],play_by_play$Home.4[i],play_by_play$Home.5[i],play_by_play$Away.1[i],play_by_play$Away.2[i],play_by_play$Away.3[i],play_by_play$Away.4[i],play_by_play$Away.5[i]))) {
        five_out <- play_by_play$Event_Length[i]+five_out
        team_schedule[j,10+length(player_names)*7+row_num] <- five_out
        if (play_by_play$Event_Team[i] == team && play_by_play$Shot_Value[i] > 0) {
          five_points <- five_points + play_by_play$Shot_Value[i]
          team_schedule[j,10+length(player_names)*11+row_num] <- five_points
        }
        else if (play_by_play$Event_Team[i] != team && play_by_play$Shot_Value[i] > 0) {
          five_points <- five_points - play_by_play$Shot_Value[i]
          team_schedule[j,10+length(player_names)*11+row_num] <- five_points
        }
      }
      
    }
    row_num = row_num+1
  }
}

two_foul_wins <- c()
two_foul_losses <- c()
three_foul_wins <- c()
three_foul_losses <- c()
four_foul_wins <- c()
four_foul_losses <- c()
five_foul_wins <- c()
five_foul_losses <- c()


two_foul_mins <- c()
two_foul_points <- c()
three_foul_mins <- c()
three_foul_points <- c()
four_foul_mins <- c()
four_foul_points <- c()
five_foul_mins <- c()
five_foul_points <- c()

for (i in 0:(length(player_names)-1)) {
  two_foul_mins <- c(two_foul_mins,sum(team_schedule[,10+length(player_names)*4+i]))
  two_foul_points <- c(two_foul_points,sum(team_schedule[,10+length(player_names)*8+i]))
  three_foul_mins <- c(three_foul_mins,sum(team_schedule[,10+length(player_names)*5+i]))
  three_foul_points <- c(three_foul_points,sum(team_schedule[,10+length(player_names)*9+i]))
  four_foul_mins <- c(four_foul_mins,sum(team_schedule[,10+length(player_names)*6+i]))
  four_foul_points <- c(four_foul_points,sum(team_schedule[,10+length(player_names)*10+i]))
  five_foul_mins <- c(five_foul_mins,sum(team_schedule[,10+length(player_names)*7+i]))
  five_foul_points <- c(five_foul_points,sum(team_schedule[,10+length(player_names)*11+i]))
}


col_num <-0


for (j in player_names){
  two_foul_w <- 0
  two_foul_l <- 0
  three_foul_w <- 0
  three_foul_l <- 0
  four_foul_w <- 0
  four_foul_l <- 0
  five_foul_w <- 0
  five_foul_l <- 0
  
  for (i in 1:length(team_schedule$Date)) {
    if ((team_schedule$Home[i]==team && team_schedule$Home_Score[i]> team_schedule$Away_Score[i] && team_schedule[i,10+col_num]==1) || (team_schedule$Away[i]==team && team_schedule$Away_Score[i]> team_schedule$Home_Score[i] && team_schedule[i,10+col_num]==1)) {
      two_foul_w <- two_foul_w+1
    }
    else if ((team_schedule$Home[i]==team && team_schedule$Home_Score[i]< team_schedule$Away_Score[i] && team_schedule[i,10+col_num]==1) || (team_schedule$Away[i]==team && team_schedule$Away_Score[i]< team_schedule$Home_Score[i] && team_schedule[i,10+col_num]==1)) {
      two_foul_l <- two_foul_l+1
    }
    if ((team_schedule$Home[i]==team && team_schedule$Home_Score[i]> team_schedule$Away_Score[i] && team_schedule[i,10+col_num+length(player_names)]==1) || (team_schedule$Away[i]==team && team_schedule$Away_Score[i]> team_schedule$Home_Score[i] && team_schedule[i,10+col_num+length(player_names)]==1)) {
      three_foul_w <- three_foul_w+1
    }
    else if ((team_schedule$Home[i]==team && team_schedule$Home_Score[i]< team_schedule$Away_Score[i] && team_schedule[i,10+col_num+length(player_names)]==1) || (team_schedule$Away[i]==team && team_schedule$Away_Score[i]< team_schedule$Home_Score[i] && team_schedule[i,10+col_num+length(player_names)]==1)) {
      three_foul_l <- three_foul_l+1
    }
    if ((team_schedule$Home[i]==team && team_schedule$Home_Score[i]> team_schedule$Away_Score[i] && team_schedule[i,10+col_num+length(player_names)*2]==1) || (team_schedule$Away[i]==team && team_schedule$Away_Score[i]> team_schedule$Home_Score[i] && team_schedule[i,10+col_num+length(player_names)*2]==1)) {
      four_foul_w <- four_foul_w+1
    }
    else if ((team_schedule$Home[i]==team && team_schedule$Home_Score[i]< team_schedule$Away_Score[i] && team_schedule[i,10+col_num+length(player_names)*2]==1) || (team_schedule$Away[i]==team && team_schedule$Away_Score[i]< team_schedule$Home_Score[i] && team_schedule[i,10+col_num+length(player_names)*2]==1)) {
      four_foul_l <- four_foul_l+1
    }
    if ((team_schedule$Home[i]==team && team_schedule$Home_Score[i]> team_schedule$Away_Score[i] && team_schedule[i,10+col_num+length(player_names)*3]==1) || (team_schedule$Away[i]==team && team_schedule$Away_Score[i]> team_schedule$Home_Score[i] && team_schedule[i,10+col_num+length(player_names)*3]==1)) {
      five_foul_w <- five_foul_w+1
    }
    else if ((team_schedule$Home[i]==team && team_schedule$Home_Score[i]< team_schedule$Away_Score[i] && team_schedule[i,10+col_num+length(player_names)*3]==1) || (team_schedule$Away[i]==team && team_schedule$Away_Score[i]< team_schedule$Home_Score[i] && team_schedule[i,10+col_num+length(player_names)*3]==1)) {
      five_foul_l <- five_foul_l+1
    }
    
  }
  
  two_foul_wins <- c(two_foul_wins,two_foul_w)
  two_foul_losses <- c(two_foul_losses,two_foul_l)
  three_foul_wins <- c(three_foul_wins,three_foul_w)
  three_foul_losses <- c(three_foul_losses,three_foul_l)
  four_foul_wins <- c(four_foul_wins,four_foul_w)
  four_foul_losses <- c(four_foul_losses,four_foul_l)
  five_foul_wins <- c(five_foul_wins,five_foul_w)
  five_foul_losses <- c(five_foul_losses,five_foul_l)
  
  col_num = col_num+1
  
  
}


df <- data.frame(player_names, two_foul_wins, two_foul_losses, three_foul_wins, three_foul_losses, four_foul_wins, four_foul_losses, five_foul_wins, five_foul_losses, two_foul_mins, two_foul_points, three_foul_mins, three_foul_points, four_foul_mins, four_foul_points, five_foul_mins, five_foul_points)

team_roster <- get_team_roster(season = "2021-22", team.name = team)
team_roster <- team_roster %>%
  mutate(player_names = Player,
         number = Jersey) %>%
  select(player_names, number)

df <- merge(df, team_roster, by="player_names")

team_name <- team

# bart_player_stats <- bart_player_season(year=2022, stat='all')
# bart_player_stats <- bart_player_stats %>%
#   filter(team == team_name) %>%
#   mutate(pfr = round(pfr/40*mpg,1),
#          number = num) %>%
#   select(player, pfr, number)

bart_player_stats <- bart_player_season(year=2022, stat='all')
bart_player_stats <- bart_player_stats %>%
  filter(team == team_name) %>%
  mutate(Net_Per_Min = .671*(ortg-drtg)/mpg) %>%
  mutate(pfr = round(pfr/40*mpg,1),
         number = num) %>%
  select(player, Net_Per_Min, pfr, number)


team_roster <- ncaahoopR::get_roster(team, season = "2021-22")
team_roster <- team_roster %>%
  mutate(player = name) %>%
  select(number, player_image)

foul_player_stats <- merge(bart_player_stats, team_roster, by="number")
foul_player_stats <- merge(df, foul_player_stats, by="number")
foul_player_stats <- foul_player_stats %>%
  mutate(two_win_per = round(two_foul_wins/(two_foul_wins+two_foul_losses)*100,1),
         three_win_per = round(three_foul_wins/(three_foul_wins+three_foul_losses)*100,1),
         four_win_per = round(four_foul_wins/(four_foul_wins+four_foul_losses)*100,1),
         five_win_per = round(five_foul_wins/(five_foul_wins+five_foul_losses)*100,1),
         two_record = paste0(two_foul_wins,"-",two_foul_losses),
         three_record = paste0(three_foul_wins,"-",three_foul_losses),
         four_record = paste0(four_foul_wins,"-",four_foul_losses),
         five_record = paste0(five_foul_wins,"-",five_foul_losses),
         two_foul_points = round((two_foul_points/two_foul_mins/60 - Net_Per_Min) * two_foul_mins/60 / (two_foul_wins+two_foul_losses),1),
         three_foul_points = round((three_foul_points/three_foul_mins/60 - Net_Per_Min) * three_foul_mins/60 / (three_foul_wins+three_foul_losses),1),
         four_foul_points = round((four_foul_points/four_foul_mins/60 - Net_Per_Min) * four_foul_mins/60 / (four_foul_wins+four_foul_losses),1),
         five_foul_points = round((five_foul_points/five_foul_mins/60 - Net_Per_Min) * five_foul_mins/60 / (five_foul_wins+five_foul_losses),1),
         two_foul_mins = round(two_foul_mins/60/(two_foul_wins+two_foul_losses),1),
         three_foul_mins = round(three_foul_mins/60/(three_foul_wins+three_foul_losses),1),
         four_foul_mins = round(four_foul_mins/60/(four_foul_wins+four_foul_losses),1),
         five_foul_mins = round(five_foul_mins/60/(five_foul_wins+five_foul_losses),1),
  ) %>%
  select(player_image, player, pfr, two_record, two_win_per, two_foul_points, two_foul_mins, three_record, three_win_per, three_foul_points, three_foul_mins, four_record, four_win_per, four_foul_points, four_foul_mins, five_record, five_win_per, five_foul_points, five_foul_mins,) %>%
  arrange(desc(pfr))
foul_player_stats[is.na(foul_player_stats)] = 0.0

first_half_fouls <- foul_player_stats %>%
  select(player_image, player, pfr, two_record, two_win_per, two_foul_points, two_foul_mins, three_record, three_win_per, three_foul_points, three_foul_mins)

second_half_fouls <- foul_player_stats %>%
  select(player_image, player, pfr, four_record, four_win_per, four_foul_points, four_foul_mins, five_record, five_win_per, five_foul_points, five_foul_mins)

# foul_player_stats %>%
#   gt() %>%
#   cols_label(player_image = "",
#              player = "Player",
#              pfr = "Fouls Per Game", 
#              two_record = "Record", 
#              two_win_per = "Win %", 
#              three_record = "Record", 
#              three_win_per  = "Win %", 
#              four_record = "Record", 
#              four_win_per  = "Win %", 
#              five_record = "Record", 
#              five_win_per  = "Win %"
#   ) %>%
#   tab_header(
#     title = md(paste0(team," Foul Analysis")),
#     subtitle = "*2 and 3 fouls are for the 1st half"
#   )  %>%
#   tab_spanner(label = "2 Fouls", columns = vars(two_record, two_win_per)) %>%
#   tab_spanner(label = "3 Fouls", columns = vars(three_record, three_win_per)) %>%
#   tab_spanner(label = "4 Fouls", columns = vars(four_record, four_win_per)) %>%
#   tab_spanner(label = "5 Fouls", columns = vars(five_record, five_win_per)) %>%
#   text_transform(
#     locations = cells_body(vars(player_image)),
#     fn = function(x) {
#       web_image(url = x,
#                 height = px(22.5))
#     }
#   ) %>%
#   # data_color(
#   #   columns = c(two_win_per, three_win_per, four_win_per, five_win_per),
#   #   colors = scales::col_numeric(
#   #     palette = paletteer::paletteer_d(
#   #       palette = "ggsci::green_material"
#   #     ) %>% as.character(),
#   #     domain = NULL
#   #   )
#   # ) %>%
#   data_color(
#     columns = vars(two_win_per, three_win_per, four_win_per, five_win_per),
#     colors = scales::col_numeric(
#       palette = paletteer::paletteer_d(
#         palette = "RColorBrewer::RdYlGn",
#         direction  = 1
#       ) %>% as.character(),
#       domain = c(100,0),
#       na.color = "#00441BFF"
#     )
#   ) %>%
#   cols_align(
#     align = "left",
#     columns = vars(player_image, player, pfr, two_record, two_win_per, three_record, three_win_per, four_record, four_win_per, five_record, five_win_per)
#   ) %>%
#   cols_width(vars(player_image, pfr, two_record, two_win_per, three_record, three_win_per, four_record, four_win_per, five_record, five_win_per) ~ px(45),
#              vars(player) ~ px(115),
#              #vars(Charges) ~ px(45),
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
#       rows = player_image == "League Average"
#     )
#   ) %>%
#   tab_style(
#     style = cell_fill(color = "floralwhite"),
#     locations = cells_body(
#       rows = player_image == "League Average")
#   ) %>%
#   tab_options(
#     table.background.color = "floralwhite",
#     column_labels.font.size = 10.5,
#     table.font.size = 10,
#     heading.title.font.size  = 24,
#     heading.title.font.weight = 'bold',
#     heading.subtitle.font.size = 11,
#     table.font.names = "Consolas",
#     table.font.color = 'black',
#     #table.border.top.color = "transparent",
#     data_row.padding = px(2),
#     footnotes.font.size = 8,
#     source_notes.font.size = 9,
#     footnotes.padding = px(1),
#   ) %>%
#   gtsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Foul Analysis.png", expand = 0)

first_half_fouls %>%
  gt() %>%
  cols_label(player_image = "",
             player = "Player",
             pfr = "Fouls per Game",
             two_record = "Record",
             two_win_per = "Win %",
             two_foul_points = "Pts Lost",
             two_foul_mins = "Mins Out",
             three_record = "Record",
             three_win_per = "Win %",
             three_foul_points = "Pts Lost",
             three_foul_mins = "Mins Out"
             
  ) %>%
  tab_header(
    title = md(paste0(team," 1st Half Foul Analysis"))#,
    #subtitle = "*2 and 3 fouls are for the 1st half"
  )  %>%
  tab_spanner(label = "2 Fouls", columns = vars(two_record, two_win_per, two_foul_points, two_foul_mins)) %>%
  tab_spanner(label = "3 Fouls", columns = vars(three_record, three_win_per, three_foul_points, three_foul_mins)) %>%
  # tab_spanner(label = "4 Fouls", columns = vars(four_record, four_win_per)) %>%
  # tab_spanner(label = "5 Fouls", columns = vars(five_record, five_win_per)) %>%
  text_transform(
    locations = cells_body(vars(player_image)),
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
  data_color(
    columns = vars(two_win_per, three_win_per),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "RColorBrewer::RdYlGn",
        direction  = 1
      ) %>% as.character(),
      domain = c(100,0),
      na.color = "#00441BFF"
    )
  ) %>%
  cols_align(
    align = "left",
    columns = vars(player_image, player, pfr, two_record, two_win_per, two_foul_points, two_foul_mins, three_record, three_win_per, three_foul_points, three_foul_mins)
  ) %>%
  cols_width(vars(player_image, pfr, two_record, two_win_per, three_record, three_win_per, ) ~ px(45),
             vars(player) ~ px(115),
             vars(two_foul_points, two_foul_mins, three_foul_points, three_foul_mins) ~ px(60),
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
      rows = player_image == "League Average"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "floralwhite"),
    locations = cells_body(
      rows = player_image == "League Average")
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
  gtsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\First  Half Foul Analysis.png", expand = 0)


second_half_fouls %>%
  gt() %>%
  cols_label(player_image = "",
             player = "Player",
             pfr = "Fouls Per Game",
             four_record = "Record",
             four_win_per = "Win %",
             four_foul_points = "Pts Lost",
             four_foul_mins = "Mins Out",
             five_record = "Record",
             five_win_per = "Win %",
             five_foul_points = "Pts Lost",
             five_foul_mins = "Mins Out"
             
  ) %>%
  tab_header(
    title = md(paste0(team," Foul Analysis"))#,
    #subtitle = "*2 and 3 fouls are for the 1st half"
  )  %>%
  tab_spanner(label = "4 Fouls", columns = vars(four_record, four_win_per, four_foul_points, four_foul_mins)) %>%
  tab_spanner(label = "5 Fouls", columns = vars(five_record, five_win_per, five_foul_points, five_foul_mins)) %>%
  # tab_spanner(label = "4 Fouls", columns = vars(four_record, four_win_per)) %>%
  # tab_spanner(label = "5 Fouls", columns = vars(five_record, five_win_per)) %>%
  text_transform(
    locations = cells_body(vars(player_image)),
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
  data_color(
    columns = vars(four_win_per, five_win_per),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "RColorBrewer::RdYlGn",
        direction  = 1
      ) %>% as.character(),
      domain = c(100,0),
      na.color = "#00441BFF"
    )
  ) %>%
  cols_align(
    align = "left",
    columns = vars(player_image, player, pfr, four_record, four_win_per, four_foul_points, four_foul_mins, five_record, five_win_per, five_foul_points, five_foul_mins)
  ) %>%
  cols_width(vars(player_image, pfr, four_record, four_win_per,  five_record, five_win_per, ) ~ px(45),
             vars(player) ~ px(115),
             vars(four_foul_points, four_foul_mins, five_foul_points, five_foul_mins) ~ px(60),
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
      rows = player_image == "League Average"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "floralwhite"),
    locations = cells_body(
      rows = player_image == "League Average")
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
  gtsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Second Half Foul Analysis.png", expand = 0)