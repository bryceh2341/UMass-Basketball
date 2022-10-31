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


team <- "Towson"
team_schedule <- get_team_schedule(season = "2021-22", team.name = team)

player_names <- c("CAMERON.HOLDEN", "NICOLAS.TIMBERLAKE", "CHARLES.THOMPSON")

for (i in 1:4) {
  for (j in player_names) {
    new <- rep(0, nrow(team_schedule))
    team_schedule[ , ncol(team_schedule) + 1] <- new
    colnames(team_schedule)[ncol(team_schedule)] <- paste0(j, i+1)
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

df <- data.frame(player_names, two_foul_wins, two_foul_losses, three_foul_wins, three_foul_losses, four_foul_wins, four_foul_losses, five_foul_wins, five_foul_losses)

team_roster <- get_team_roster(season = "2021-22", team.name = team)
team_roster <- team_roster %>%
  mutate(player_names = Player,
         number = Jersey) %>%
  select(player_names, number)

df <- merge(df, team_roster, by="player_names")

team_name <- team

bart_player_stats <- bart_player_season(year=2022, stat='all')
bart_player_stats <- bart_player_stats %>%
  filter(team == team_name) %>%
  mutate(pfr = round(pfr/40*mpg,1),
         number = num) %>%
  select(player, pfr, number)
  


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
         five_record = paste0(five_foul_wins,"-",five_foul_losses)) %>%
  select(player_image, player, pfr, two_record, two_win_per, three_record, three_win_per, four_record, four_win_per, five_record, five_win_per) %>%
  arrange(desc(pfr))
foul_player_stats[is.na(foul_player_stats)] = 0.0

foul_player_stats %>%
  gt() %>%
  cols_label(player_image = "",
             player = "Player",
             pfr = "Fouls Per Game", 
             two_record = "Record", 
             two_win_per = "Win %", 
             three_record = "Record", 
             three_win_per  = "Win %", 
             four_record = "Record", 
             four_win_per  = "Win %", 
             five_record = "Record", 
             five_win_per  = "Win %"
  ) %>%
  tab_header(
    title = md(paste0(team," Foul Analysis")),
    subtitle = "*2 and 3 fouls are for the 1st half"
  )  %>%
  tab_spanner(label = "2 Fouls", columns = vars(two_record, two_win_per)) %>%
  tab_spanner(label = "3 Fouls", columns = vars(three_record, three_win_per)) %>%
  tab_spanner(label = "4 Fouls", columns = vars(four_record, four_win_per)) %>%
  tab_spanner(label = "5 Fouls", columns = vars(five_record, five_win_per)) %>%
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
    columns = vars(two_win_per, three_win_per, four_win_per, five_win_per),
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
    columns = vars(player_image, player, pfr, two_record, two_win_per, three_record, three_win_per, four_record, four_win_per, five_record, five_win_per)
  ) %>%
  cols_width(vars(player_image, pfr, two_record, two_win_per, three_record, three_win_per, four_record, four_win_per, five_record, five_win_per) ~ px(45),
             vars(player) ~ px(115),
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
  gtsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Foul Analysis.png", expand = 0)
