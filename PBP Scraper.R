library(hoopR)
library(bigballR)
library(ncaahoopR)

# games <- c()
# 
# for (i in dict$ESPN) {
#   if (!(i %in% c("Birmingham-Southern", "Centenary", "Morris Brown", "Savannah St", "Texas A&M-CC", "Alabama A&M"))) {
#     new <- get_game_ids(i, season = "2021-22")
#     games <- append(games, new)
#   }
# 
# }
# 
# games <- unique(games)
# 
remove <- c("401371687", "401364713", "401364727", "401365073", "401365083", "401373879", "401371609", "401384911", "401384911", "401365101", "401371568", "401371398", "401369899", "401373116", "401370906", "401373137", "401381762", "401381765", "401378827", "401370727", "401364715", "401365027", "401373028", "401373057", "401365803", "401368255", "401373410", "401373631", "401376650", "401372918", "401373172", "401373788", "401364749", "401370957", "401372678", "401410514", "401378813")
games = games[! games %in% remove]
# 
# off_rtg <- c()
# efg <- c()
# tov <- c()
# OReb <- c()
# FTR <- c()
# layup <- c()
# jumper <- c()
# three_pointer <- c()
# layup_per <- c()
# jumper_per <- c()
# three_pointer_per <- c()
# team <- c()
# opponent <- c()
# game <- c()

for (i in games[3732:6349]) {

  game_id <- as.numeric(i)
  
  data <- 0
  #scrape play by play data using ESPN game id
  data <- get_pbp_game(game_id, extra_parse = T)
  if (is.null(data)) {
    print("Done")
    next
  }
  boxscore <- get_boxscore(game_id)
  #replace na with 0
  data[is.na(data)] = 0
  
  #create empty counter variables
  possessions=0
  home_poss=0
  away_poss=0
  
  
  home_layup_fga=0
  home_layup_fgm=0
  away_layup_fga=0
  away_layup_fgm=0
  
  home_team=data$home[1]
  away_team=data$away[1]
  
  home_fga=max(boxscore[[2]]$FGA)
  away_fga=max(boxscore[[1]]$FGA)
  home_fgm=max(boxscore[[2]]$FGM)
  away_fgm=max(boxscore[[1]]$FGM)
  home_fta=max(boxscore[[2]]$FTA)
  away_fta=max(boxscore[[1]]$FTA)
  home_ftm=max(boxscore[[2]]$FTM)
  away_ftm=max(boxscore[[1]]$FTM)
  home_3PA=max(boxscore[[2]]$`3PTA`)
  away_3PA=max(boxscore[[1]]$'3PTA')
  home_3PM=max(boxscore[[2]]$`3PTM`)
  away_3PM=max(boxscore[[1]]$'3PTM')
  home_oreb=max(boxscore[[2]]$OREB)
  away_oreb=max(boxscore[[1]]$OREB)
  home_dreb=max(boxscore[[2]]$DREB)
  away_dreb=max(boxscore[[1]]$DREB)
  home_tov=max(boxscore[[2]]$TO)
  away_tov=max(boxscore[[1]]$TO)
  
  home_off_steal=0
  away_off_steal=0
  home_live_rebound=0
  away_live_rebound=0
  home_off_steal_pts=0
  away_off_steal_pts=0
  home_live_rebound_pts=0
  away_live_rebound_pts=0
  
  for (i in 2:length(data$game_id)) {
    #count the number of possessions for the game
    if ((data$possession_after[i] != data$possession_after[i-1]) && (data$possession_before[i] != 0)) {
      possessions = possessions+1
      #home possession counter
      if (data$possession_before[i] == data$home[i]) {
        home_poss = home_poss+1
      }
      #away possession counter
      else if (data$possession_before[i] == data$away[i]) {
        away_poss = away_poss+1
      }
    }
    
    if (grepl("Steal", data$description[i-1], fixed = TRUE) && (data$secs_remaining[i-1]-data$secs_remaining[i]<=8) && (data$shot_team[i]!=0) ) {
      if (data$home[i] == data$shot_team[i]) {
        home_off_steal=home_off_steal+1
        home_off_steal_pts=home_off_steal_pts+(data$home_score[i]-data$home_score[i-1])
      }
      else if (data$away[i] == data$shot_team[i]) {
        away_off_steal=away_off_steal+1
        away_off_steal_pts=away_off_steal_pts+(data$away_score[i]-data$away_score[i-1])
      }
    }
    
    if (grepl("Defensive Rebound", data$description[i-1], fixed = TRUE) && (data$secs_remaining[i-1]-data$secs_remaining[i]<=8) && (data$shot_team[i]!=0) ) {
      if (data$home[i] == data$shot_team[i]) {
        home_live_rebound=home_live_rebound+1
        home_live_rebound_pts=home_live_rebound_pts+(data$home_score[i]-data$home_score[i-1])
      }
      else if (data$away[i] == data$shot_team[i]) {
        away_live_rebound=away_live_rebound+1
        away_live_rebound_pts=away_live_rebound_pts+(data$away_score[i]-data$away_score[i-1])
      }
    }
    
    if (data$shot_team[i] == data$home[i]) {
      if (grepl("Layup",data$description[i])) {
        home_layup_fga = home_layup_fga+1
        if (data$shot_outcome[i] == 'made') {
          home_layup_fgm = home_layup_fgm+1
        }
      }
    }
    else if (data$shot_team[i] == data$away[i]) {
      if (grepl("Layup",data$description[i])) {
        away_layup_fga = away_layup_fga+1
        if (data$shot_outcome[i] == 'made') {
          away_layup_fgm = away_layup_fgm+1
        }
      }
    }
    
  }
  
  home_off_rtg=data$home_score[i]/home_poss*100
  away_off_rtg=data$away_score[i]/away_poss*100
  home_ftr=home_fta/home_fga
  away_ftr=away_fta/away_fga
  home_efg=(home_fgm+0.5*home_3PM)/home_fga
  away_efg=(away_fgm+0.5*away_3PM)/away_fga
  home_off_reb=home_oreb/(home_oreb + away_dreb)
  away_off_reb=away_oreb/(away_oreb + home_dreb)
  home_tov=home_tov/home_poss
  away_tov=away_tov/away_poss
  
  home_jumper_fga=home_fga-home_3PA-home_layup_fga
  home_jumper_fgm=home_fgm-home_3PM-home_layup_fgm
  away_jumper_fga=away_fga-away_3PA-away_layup_fga
  away_jumper_fgm=away_fgm-away_3PM-away_layup_fgm
  
  off_rtg <- c(off_rtg, home_off_rtg, away_off_rtg)
  efg <- c(efg, home_efg, away_efg)
  tov <- c(tov, home_tov, away_tov)
  OReb <- c(OReb, home_off_reb, away_off_reb)
  FTR <- c(FTR, home_ftr, away_ftr)
  
  layup <- c(layup, home_layup_fga/home_fga, away_layup_fga/away_fga)
  jumper <- c(jumper, home_jumper_fga/home_fga, away_jumper_fga/away_fga)
  three_pointer <- c(three_pointer, home_3PA/home_fga, away_3PA/away_fga)
  
  layup_per <- c(layup_per, home_layup_fgm/home_layup_fga, away_layup_fgm/away_layup_fga)
  jumper_per <- c(jumper_per, home_jumper_fgm/home_jumper_fga, away_jumper_fgm/away_jumper_fga)
  three_pointer_per <- c(three_pointer_per, home_3PM/home_3PA, away_3PM/away_3PA)
  
  team <- c(team, home_team, away_team)
  opponent <- c(opponent, away_team, home_team)
  game <- c(game, game_id, game_id)
  
  # general <- data.frame(team, off_rtg, efg, tov, OReb, FTR)
  # shot_loc <- data.frame(team, layup, jumper, three_pointer)
  # shot_per <- data.frame(team, layup_tot, layup_per, jumper_tot, jumper_per, three_pointer_tot, three_pointer_per)
}
