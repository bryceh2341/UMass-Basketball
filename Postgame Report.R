library(hoopR)
library(bigballR)
library(ncaahoopR)
library(gt)
library(cowplot)
library(ggpubr)

setwd("C:/Users/Bryce Haase/Desktop/UMass Basketball")

game_id <- 401487699
ncaa_game_id <- 5370969

full_player_stats <- read.csv("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\2021_22_player_stats.csv")

#team_name = "Massachusetts"
ESPN_Home <- "South Florida"
ESPN_Away <- "UMass"

#opp_off <- 105.0
#opp_def <- 92.1
home_off <- 98.0
home_def <- 100.0
away_off <- 102.2
away_def <- 99.3
avg <- 101.4

home_color <- 'green'
away_color <- 'red'

# game_shot_chart(game_id, heatmap = F)
# ggsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Shot Chart.png", width = 7, height = 5, dpi = 300, limitsize = F)
# game_shot_chart(game_id, heatmap = T)
# ggsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Heat Map.png")


player_stats <- get_player_stats(get_play_by_play(ncaa_game_id), multi.games = F, simple = F)


#home_roster <- ncaahoopR::get_roster(player_stats$Home[1], season = "2022-23")
home_roster <- ncaahoopR::get_roster(ESPN_Home, season = "2022-23")
home_roster <- home_roster %>%
  mutate(Jersey = number) %>%
  select(Jersey, player_image)

home_roster <- merge(home_roster, get_team_roster(season = "2022-23", team.name = player_stats$Home[1]), by="Jersey")

#away_roster <- ncaahoopR::get_roster(player_stats$Away[1], season = "2022-23")
away_roster <- ncaahoopR::get_roster(ESPN_Away, season = "2022-23")
away_roster <- away_roster %>%
  mutate(Jersey = number) %>%
  select(Jersey, player_image)

away_roster <- merge(away_roster, get_team_roster(season = "2022-23", team.name = player_stats$Away[1]), by="Jersey")

#scrape play by play data using ESPN game id
data <- get_pbp_game(game_id, extra_parse = T)
boxscore <- get_boxscore(game_id)
#replace na with 0
data[is.na(data)] = 0

#create empty counter variables
# possessions=0
# home_poss=0
# away_poss=0


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

id_table <- ids

home_team_url <- "https://upload.wikimedia.org/wikipedia/commons/thumb/d/dd/NCAA_logo.svg/1042px-NCAA_logo.svg.png"
away_team_url <- "https://upload.wikimedia.org/wikipedia/commons/thumb/d/dd/NCAA_logo.svg/1042px-NCAA_logo.svg.png"

for (i in 1:length(id_table$team)) {
  if (id_table$team[i] == home_team) {
    home_team_url <- paste0("https://a.espncdn.com/combiner/i?img=/i/teamlogos/ncaa/500/",id_table$id[i],".png&h=200&w=200")
  }
  else if (id_table$team[i] == away_team) {
    away_team_url <- paste0("https://a.espncdn.com/combiner/i?img=/i/teamlogos/ncaa/500/",id_table$id[i],".png&h=200&w=200")
  }
}

url <- c(home_team_url, away_team_url)

pbp_data <- get_play_by_play(ncaa_game_id)
lineup_data <- get_lineups(play_by_play_data = pbp_data, include_transition = T)

home=pbp_data$Home[1]
away=pbp_data$Away[1]

home_poss=c()
home_trans=c()
home_trans_pts=0
away_poss=c()
away_trans=c()
away_trans_pts=0

for (i in 1:length(pbp_data$ID)) {
  if (pbp_data$Poss_Team[i]==home){
    home_poss=c(home_poss, pbp_data$Poss_Num[i])
    if (pbp_data$isTransition[i]==TRUE) {
      home_trans=c(home_trans, pbp_data$Poss_Num[i])
      if (grepl('made', pbp_data$Event_Result[i], fixed = TRUE)) {
        home_trans_pts=home_trans_pts+pbp_data$Shot_Value[i]
      }
    }
  }
  else if (pbp_data$Poss_Team[i]==away){
    away_poss=c(away_poss, pbp_data$Poss_Num[i])
    if (pbp_data$isTransition[i]==TRUE) {
      away_trans=c(away_trans, pbp_data$Poss_Num[i])
      if (grepl('made', pbp_data$Event_Result[i], fixed = TRUE)) {
        away_trans_pts=away_trans_pts+pbp_data$Shot_Value[i]
      }
    }
  }
}

for (i in 2:length(data$game_id)) {
  #count the number of possessions for the game
  # if ((data$possession_after[i] != data$possession_after[i-1]) && (data$possession_before[i] != 0)) {
  #   possessions = possessions+1
  #   #home possession counter
  #   if (data$possession_before[i] == data$home[i]) {
  #     home_poss = home_poss+1
  #   }
  #   #away possession counter
  #   else if (data$possession_before[i] == data$away[i]) {
  #     away_poss = away_poss+1
  #   }
  # }
  
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

home_poss=length(unique(home_poss))
home_trans=length(unique(home_trans))
home_trans_off_rtg=home_trans_pts/home_trans*100
away_poss=length(unique(away_poss))
away_trans=length(unique(away_trans))
away_trans_off_rtg=away_trans_pts/away_trans*100

home_half=home_poss-home_trans
home_half_off_rtg=(data$home_score[i]-home_trans_pts)/(home_poss-home_trans)*100
away_half=away_poss-away_trans
away_half_off_rtg=(data$away_score[i]-away_trans_pts)/(away_poss-away_trans)*100

home_fga <- as.numeric(home_fga)
home_fgm <- as.numeric(home_fgm)
away_fga <- as.numeric(away_fga)
away_fgm <- as.numeric(away_fgm)

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



total_data <- read.csv("total_data.csv")
total_data[nrow(total_data)+1,] <- c(nrow(total_data)+1, game_id, home_team, away_team, home_off_rtg, home_efg, home_tov, home_off_reb, home_ftr, home_layup_fga/home_fga, home_jumper_fga/home_fga, home_3PA/home_fga, home_layup_fgm/home_layup_fga, home_jumper_fgm/home_jumper_fga, home_3PM/home_3PA)
total_data[nrow(total_data)+1,] <- c(nrow(total_data)+1, game_id, away_team, home_team, away_off_rtg, away_efg, away_tov, away_off_reb, away_ftr, away_layup_fga/away_fga, away_jumper_fga/away_fga, away_3PA/away_fga, away_layup_fgm/away_layup_fga, away_jumper_fgm/away_jumper_fga, away_3PM/away_3PA)
total_data[c("off_rtg", "efg", "tov", "OReb", "FTR", "layup", "jumper", "three_pointer", "layup_per",  "jumper_per", "three_pointer_per")] <- sapply(total_data[c("off_rtg", "efg", "tov", "OReb", "FTR", "layup", "jumper", "three_pointer", "layup_per",  "jumper_per", "three_pointer_per")],as.numeric)

total_data <- total_data %>%
  mutate(off_rtg_perc = round(pnorm((off_rtg-mean(total_data$off_rtg))/sd(total_data$off_rtg)), 2),
         efg_perc = round(pnorm((efg-mean(total_data$efg))/sd(total_data$efg)), 2),
         tov_perc = round(pnorm(-1*((tov-mean(total_data$tov))/sd(total_data$tov))), 2),
         OReb_perc = round(pnorm((OReb-mean(total_data$OReb))/sd(total_data$OReb)), 2),
         FTR_perc = round(pnorm((FTR-mean(total_data$FTR))/sd(total_data$FTR)), 2),
         layup_perc = round(pnorm((layup-mean(total_data$layup))/sd(total_data$layup)), 2),
         jumper_perc = round(pnorm((jumper-mean(total_data$jumper))/sd(total_data$jumper)), 2),
         three_pointer_perc = round(pnorm((three_pointer-mean(total_data$three_pointer))/sd(total_data$three_pointer)), 2),
         layup_per_perc = round(pnorm((layup_per-mean(total_data$layup_per))/sd(total_data$layup_per)), 2),
         jumper_per_perc = round(pnorm((jumper_per-mean(total_data$jumper_per))/sd(total_data$jumper_per)), 2),
         three_pointer_per_perc = round(pnorm((three_pointer_per-mean(total_data$three_pointer_per))/sd(total_data$three_pointer_per)), 2))

team <- c(home_team, away_team)
off_rtg <- c(round(home_off_rtg, 1), round(away_off_rtg, 1))
off_rtg_perc <- c(total_data$off_rtg_perc[nrow(total_data)-1], total_data$off_rtg_perc[nrow(total_data)])
efg <- c(home_efg, away_efg)
efg_perc <- c(total_data$efg_perc[nrow(total_data)-1], total_data$efg_perc[nrow(total_data)])
tov <- c(home_tov, away_tov)
tov_perc <- c(total_data$tov_perc[nrow(total_data)-1], total_data$tov_perc[nrow(total_data)])
OReb <- c(home_off_reb, away_off_reb)
OReb_perc <- c(total_data$OReb_perc[nrow(total_data)-1], total_data$OReb_perc[nrow(total_data)])
FTR <- c(home_ftr, away_ftr)
FTR_perc <- c(total_data$FTR_perc[nrow(total_data)-1], total_data$FTR_perc[nrow(total_data)])

layup <- c(home_layup_fga/home_fga, away_layup_fga/away_fga)
layup_perc <- c(total_data$layup_perc[nrow(total_data)-1], total_data$layup_perc[nrow(total_data)])
jumper <- c(home_jumper_fga/home_fga, away_jumper_fga/away_fga)
jumper_perc <- c(total_data$jumper_perc[nrow(total_data)-1], total_data$jumper_perc[nrow(total_data)])
three_pointer <- c(home_3PA/home_fga, away_3PA/away_fga)
three_pointer_perc <- c(total_data$three_pointer_perc[nrow(total_data)-1], total_data$three_pointer_perc[nrow(total_data)])


layup_tot <- c(paste(home_layup_fgm, home_layup_fga, sep="/"), paste(away_layup_fgm, away_layup_fga, sep="/"))
layup_per <- c(home_layup_fgm/home_layup_fga, away_layup_fgm/away_layup_fga)
layup_per_perc <- c(total_data$layup_per_perc[nrow(total_data)-1], total_data$layup_per_perc[nrow(total_data)])
jumper_tot <- c(paste(home_jumper_fgm, home_jumper_fga, sep="/"), paste(away_jumper_fgm, away_jumper_fga, sep="/"))
jumper_per <- c(home_jumper_fgm/home_jumper_fga, away_jumper_fgm/away_jumper_fga)
jumper_per_perc <- c(total_data$jumper_per_perc[nrow(total_data)-1], total_data$jumper_per_perc[nrow(total_data)])
three_pointer_tot <- c(paste(home_3PM,home_3PA, sep="/"), paste(away_3PM,away_3PA, sep="/"))
three_pointer_per <- c(home_3PM/home_3PA, away_3PM/away_3PA)
three_pointer_per_perc <- c(total_data$three_pointer_per_perc[nrow(total_data)-1], total_data$three_pointer_per_perc[nrow(total_data)])

score <- c(data$home_score[i], data$away_score[i])
poss <- c(home_poss, away_poss)
halfcourt <- c(home_half/home_poss, away_half/away_poss)
halfcourt_rtg <- c(round(home_half_off_rtg,1), round(away_half_off_rtg,1))
trans <- c(home_trans/home_poss, away_trans/away_poss)
trans_rtg <- c(round(home_trans_off_rtg,1), round(away_trans_off_rtg,1))

general <- data.frame(url, team, efg, efg_perc, tov, tov_perc, OReb, OReb_perc, FTR, FTR_perc)
shot_loc <- data.frame(url, team, layup, layup_perc, jumper, jumper_perc, three_pointer, three_pointer_perc)
shot_per <- data.frame(url, team, layup_tot, layup_per, layup_per_perc, jumper_tot, jumper_per, jumper_per_perc, three_pointer_tot, three_pointer_per, three_pointer_per_perc)
overview <- data.frame(url, team, score, poss, off_rtg, off_rtg_perc, halfcourt, halfcourt_rtg, trans, trans_rtg)

overview %>%
  gt()  %>%
  cols_label(url = "",
             team = "",
             score="Score",
             poss="Possessions",
             off_rtg = "Rtg.",
             off_rtg_perc = "%ile",
             halfcourt = "%",
             halfcourt_rtg = "Rtg.",
             trans="%",
             trans_rtg="Rtg.") %>%
  tab_header(
    title = "General",
    #subtitle = ""
  )  %>%
  text_transform(
    locations = cells_body(vars(url)),
    fn = function(x) {
      web_image(url = x,
                height = px(22.5))
    }
  ) %>%
  tab_spanner(
    label = "Total",
    columns = vars(poss, off_rtg, off_rtg_perc)
  ) %>%
  tab_spanner(
    label = "Halfcourt",
    columns = vars(halfcourt, halfcourt_rtg)
  ) %>%
  tab_spanner(
    label = "Transition",
    columns = vars(trans, trans_rtg)
  ) %>%
  fmt_percent(
    columns = vars(off_rtg_perc, halfcourt, trans),
    decimals = 0
  )  %>%
  # fmt_percent(
  #   columns = vars(efg, tov, OReb, FTR),
  #   decimals = 1
  # )  %>%
  data_color(
    columns = vars(off_rtg_perc),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "RColorBrewer::RdYlGn",
        direction  = 1
      ) %>% as.character(),
      domain = c(0, 1),
      na.color = "#00441BFF"
    )
  ) %>%
  cols_align(
    align = "left",
    columns = vars(off_rtg_perc, halfcourt_rtg, trans_rtg)
  ) %>%
  cols_width(vars(score, poss, off_rtg, off_rtg_perc, halfcourt, halfcourt_rtg, trans, trans_rtg) ~ px(50),
             #vars(off_rtg_perc, efg_perc, tov_perc, OReb_perc, FTR_perc) ~ px(30),
             vars(team) ~ px(95)) %>%
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
  gtsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\General.png", expand = 0)


general %>%
  gt()  %>%
  cols_label(url = "",
             team = "",
             efg = "",
             efg_perc = "",
             tov = "",
             tov_perc = "",
             OReb = "",
             OReb_perc = "",
             FTR = "",
             FTR_perc = "") %>%
  tab_header(
    title = "Overview & Four Factors",
    #subtitle = ""
  )  %>%
  text_transform(
    locations = cells_body(vars(url)),
    fn = function(x) {
      web_image(url = x,
                height = px(22.5))
    }
  ) %>%
  tab_spanner(
    label = "eFG%",
    columns = vars(efg, efg_perc)
  ) %>%
  tab_spanner(
    label = "TOV%",
    columns = vars(tov, tov_perc)
  ) %>%
  tab_spanner(
    label = "OReb%",
    columns = vars(OReb, OReb_perc)
  ) %>%
  tab_spanner(
    label = "FTr",
    columns = vars(FTR, FTR_perc)
  ) %>%
  fmt_percent(
    columns = vars(efg_perc, tov_perc, OReb_perc, FTR_perc),
    decimals = 0
  )  %>%
  fmt_percent(
    columns = vars(efg, tov, OReb, FTR),
    decimals = 1
  )  %>%
  data_color(
    columns = vars(efg_perc, tov_perc, OReb_perc, FTR_perc),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "RColorBrewer::RdYlGn",
        direction  = 1
      ) %>% as.character(),
      domain = c(0, 1),
      na.color = "#00441BFF"
    )
  ) %>%
  cols_align(
    align = "left",
    columns = vars(efg_perc, tov_perc, OReb_perc, FTR_perc)
  ) %>%
  cols_width(vars(efg, tov, OReb, FTR) ~ px(45),
             vars(efg_perc, tov_perc, OReb_perc, FTR_perc) ~ px(30),
             vars(team) ~ px(95)) %>%
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
  gtsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Overview and Four Factors.png", expand = 0)

shot_loc %>%
  gt()  %>%
  cols_label(url = "",
             team = "",
             layup = "",
             layup_perc = "",
             jumper = "",
             jumper_perc = "",
             three_pointer = "",
             three_pointer_perc = "") %>%
  tab_header(
    title = "Shot Locations",
    #subtitle = "2021-22 Regular Season"
  )  %>%
  text_transform(
    locations = cells_body(vars(url)),
    fn = function(x) {
      web_image(url = x,
                height = px(22.5))
    }
  ) %>%
  tab_spanner(
    label = "Layups",
    columns = vars(layup, layup_perc)
  ) %>%
  tab_spanner(
    label = "Midrange",
    columns = vars(jumper, jumper_perc)
  ) %>%
  tab_spanner(
    label = "3-Pointers",
    columns = vars(three_pointer, three_pointer_perc)
  ) %>%
  fmt_percent(
    columns = vars(layup_perc, jumper_perc, three_pointer_perc),
    decimals = 0
  )  %>%
  fmt_percent(
    columns = vars(layup, jumper, three_pointer),
    decimals = 1
  )  %>%
  data_color(
    columns = vars(layup_perc, jumper_perc, three_pointer_perc),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "RColorBrewer::RdYlGn",
        direction  = 1
      ) %>% as.character(),
      domain = c(0, 1),
      na.color = "#00441BFF"
    )
  ) %>%
  cols_align(
    align = "left",
    columns = vars(layup_perc, jumper_perc, three_pointer_perc)
  ) %>%
  cols_width(vars(layup, jumper, three_pointer) ~ px(45),
             vars(layup_perc, jumper_perc, three_pointer_perc) ~ px(30),
             vars(team) ~ px(95)) %>%
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
  gtsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Shot Locations.png", expand = 0)

shot_per %>%
  gt()  %>%
  cols_label(url = "",
             team = "",
             layup_tot = "",
             layup_per = "",
             layup_per_perc = "",
             jumper_tot = "",
             jumper_per = "",
             jumper_per_perc = "",
             three_pointer_tot = "",
             three_pointer_per = "",
             three_pointer_per_perc = "") %>%
  tab_header(
    title = "Shooting Percentages",
    #subtitle = "2021-22 Regular Season"
  )  %>%
  text_transform(
    locations = cells_body(vars(url)),
    fn = function(x) {
      web_image(url = x,
                height = px(22.5))
    }
  ) %>%
  tab_spanner(
    label = "Layups",
    columns = vars(layup_tot, layup_per, layup_per_perc)
  ) %>%
  tab_spanner(
    label = "Midrange",
    columns = vars(jumper_tot, jumper_per, jumper_per_perc)
  ) %>%
  tab_spanner(
    label = "3-Pointers",
    columns = vars(three_pointer_tot, three_pointer_per, three_pointer_per_perc)
  ) %>%
  fmt_percent(
    columns = vars(layup_per_perc, jumper_per_perc, three_pointer_per_perc),
    decimals = 0
  )  %>%
  fmt_percent(
    columns = vars(layup_per, jumper_per, three_pointer_per),
    decimals = 1
  )  %>%
  data_color(
    columns = vars(layup_per_perc, jumper_per_perc, three_pointer_per_perc),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "RColorBrewer::RdYlGn",
        direction  = 1
      ) %>% as.character(),
      domain = c(0, 1),
      na.color = "#00441BFF"
    )
  ) %>%
  cols_align(
    align = "left",
    columns = vars(layup_per_perc, jumper_per_perc, jumper_per_perc)
  ) %>%
  cols_width(vars(layup_per, jumper_per, three_pointer_per, layup_tot, jumper_tot, three_pointer_tot) ~ px(35),
             vars(layup_per_perc, jumper_per_perc, three_pointer_per_perc) ~ px(30),
             vars(team) ~ px(95)) %>%
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
  gtsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Shot Percentages.png", expand = 0)


setwd("C:/Users/Bryce Haase/Desktop/UMass Basketball")
plot0 <- ggdraw() + draw_image("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\General.png", scale = 1)
plot1 <- ggdraw() + draw_image("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Overview and Four Factors.png", scale = 1)
plot2 <- ggdraw() + draw_image("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Shot Percentages.png", scale = 1)
plot3 <- ggdraw() + draw_image("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Shot Locations.png", scale = 1)
total_plot<-ggarrange(plot0, plot1, plot2, plot3, ncol=1, nrow=4) + bgcolor("transparent")
#annotate_figure(total_plot, top = text_grob(paste0(home_team, ": ", data$home_score[i], " | ", away_team, ": ", data$away_score[i]), color = "black", face = "bold", family = "Consolas", size = 25))
ggsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\All Data.png")


game_flow(game_id, home_color, away_color)
ggsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Game Flow.png", width = 7, height = 5, dpi = 300, limitsize = F)
print(wp_chart_new(game_id, home_color, away_color, include_spread = T, show_labels = T))
ggsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Win Prob.png", width = 7, height = 5, dpi = 300, limitsize = F)

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

game_data <- get_team_stats(play_by_play_data = get_play_by_play(ncaa_game_id), include_transition =F)
home_team <- game_data$Home[1]
away_team <- game_data$Away[1]

data <- game_data %>%
  filter(Team == home_team) %>%
  mutate(AdjO = round(ORTG-away_def+avg,1),
         AdjO_R = 0,
         AdjD = round(DRTG-away_off+avg,1), 
         AdjD_R = 0,
         AdjN = AdjO-AdjD, 
         Twopt = round((FGM-TPM)/(FGA-TPA)*100,1),
         Twopt_R = 0,
         Threept = round(TPM/TPA*100,1), 
         Threept_R = 0,
         ThreeptVolume = round(TPA/FGA*100,1), 
         ThreeptVolume_R = 0, 
         eFG = round(eFG.*100,1), 
         eFG_R = 0, 
         oeFG = round(deFG.*100,1), 
         oeFG_R = 0,
         Oreb = round(ORB.*100,1),
         Oreb_R = 0, 
         Dreb = round((1-DRB.)*100,1),
         Dreb_R = 0, 
         Tov = round(TOrate*100,1), 
         Tov_R = 0, 
         oTov = round(dTOrate*100,1), 
         oTov_R = 0, 
         FTA = round(FTrate*100,1), 
         FTA_R = 0, 
         oFTA = round(dFTrate*100,1), 
         oFTA_R = 0, 
         AFGM = round(AST/FGM*100,1), 
         AFGM_R = 0) %>%
  select(Team, ORTG, DRTG, NETRTG, AdjO, AdjO_R, AdjD, AdjD_R, AdjN, Twopt, Twopt_R, Threept, Threept_R, ThreeptVolume, ThreeptVolume_R, eFG, eFG_R, oeFG, oeFG_R, Oreb, Oreb_R, Dreb, Dreb_R, Tov, Tov_R, oTov, oTov_R, FTA, FTA_R, oFTA, oFTA_R, AFGM, AFGM_R)


advanced_numbers <- read.csv("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Advanced Game Stats.csv")


advanced_numbers[nrow(advanced_numbers) + 1,] <- c(data$Team, data$ORTG, data$DRTG, data$NETRTG, data$AdjO, data$AdjO_R, data$AdjD, data$AdjD_R, data$AdjN, data$Twopt, data$Twopt_R, data$Threept, data$Threept_R, data$ThreeptVolume, data$ThreeptVolume_R, data$eFG, data$eFG_R, data$oeFG, data$oeFG_R, data$Oreb, data$Oreb_R, data$Dreb, data$Dreb_R, data$Tov, data$Tov_R, data$oTov, data$oTov_R, data$FTA, data$FTA_R, data$oFTA, data$oFTA_R, data$AFGM, data$AFGM_R)
advanced_numbers[,2:33]<- as.numeric(unlist(advanced_numbers[,2:33]))

advanced_numbers <- advanced_numbers %>%
  mutate(Ortg_R = round(rank(desc(Ortg)),0),
         Drtg_R = round(rank((Drtg)),0),
         Net_R = round(rank(desc(Net)),0),
         AdjO_R = round(rank(desc(AdjO)),0),
         AdjD_R = round(rank((AdjD)),0),
         AdjN_R = round(rank(desc(AdjN)),0),
         Twopt_R = round(rank(desc(Twopt)),0),
         Threept_R = round(rank(desc(Treept)),0),
         ThreeptVolume_R = round(rank(desc(ThreeptVolume)),0),
         eFG_R = round(rank(desc(eFG)),0),
         oeFG_R = round(rank((oeFG)),0),
         Oreb_R = round(rank(desc(Oreb)),0),
         Dreb_R = round(rank((Dreb)),0),
         Tov_R = round(rank((Tov)),0),
         oTov_R = round(rank(desc(oTov)),0),
         FTA_R = round(rank(desc(FTA)),0),
         oFTA_R = round(rank((oFTA)),0),
         AFGM_R = round(rank(desc(AFGM)),0)
  ) %>%
  filter(Team == home_team) %>%
  select(Ortg_R, Drtg_R, Net_R, AdjO_R, AdjD_R, AdjN_R, Twopt_R, Threept_R, ThreeptVolume_R, eFG_R, oeFG_R, Oreb_R, Dreb_R, Tov_R, oTov_R, FTA_R, oFTA_R, AFGM_R)

total_data <- read.csv("Advanced Practice Data.csv")

df <- data.frame(Stat = total_data$Stat,
                 Number = c(round(data$ORTG,1), round(data$DRTG,1), round(data$NETRTG,1), round(data$AdjO,1), round(data$AdjD,1), round(data$AdjN,1), round(data$Twopt,1), round(data$Threept,1), round(data$ThreeptVolume,1), round(data$eFG,1), round(data$oeFG,1), round(data$Oreb,1), round(data$Dreb,1), round(data$Tov,1), round(data$oTov,1), round(data$FTA,1), round(data$oFTA,1), round(data$AFGM,1)),
                 Rank = c(advanced_numbers$Ortg_R, advanced_numbers$Drtg_R, advanced_numbers$Net_R, advanced_numbers$AdjO_R, advanced_numbers$AdjD_R, advanced_numbers$AdjN_R, advanced_numbers$Twopt_R, advanced_numbers$Threept_R, advanced_numbers$ThreeptVolume_R, advanced_numbers$eFG_R, advanced_numbers$oeFG_R, advanced_numbers$Oreb_R, advanced_numbers$Dreb_R, advanced_numbers$Tov_R, advanced_numbers$oTov_R, advanced_numbers$FTA_R, advanced_numbers$oFTA_R, advanced_numbers$AFGM_R),
                 Explanation = total_data$Explanation)

df %>%
  gt() %>%
  cols_label(Stat = "Stat",
             Number = "#",
             Rank = "NCAA Rank",
             Explanation = "Description"
  ) %>%
  tab_header(
    title = md(paste0(home_team," Advanced Numbers")),
    #subtitle = paste0(home_team, " vs. ", away_team)
  )  %>%
  # text_transform(
  #   locations = cells_body(vars(url)),
  #   fn = function(x) {
  #     web_image(url = x,
  #               height = px(22.5))
  #   }
  # ) %>%
  data_color(
    columns = vars(Rank),
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
    columns = vars(Stat, Number, Rank, Explanation)
  ) %>%
  cols_align(
    align = "center",
    columns = vars(Rank)
  ) %>%
  cols_width(vars(Rank) ~ px(65),
             vars(Explanation) ~ px(300),
             vars(Stat) ~ px(100),
             vars(Number) ~ px(45)
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
      rows = Stat == "League Average"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "floralwhite"),
    locations = cells_body(
      rows = Stat == "League Average")
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
  gtsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Home Advanced Game Data.png", expand = 0)

game_data <- get_team_stats(play_by_play_data = get_play_by_play(ncaa_game_id), include_transition =F)
home_team <- game_data$Home[1]
away_team <- game_data$Away[1]

data <- game_data %>%
  filter(Team == away_team) %>%
  mutate(AdjO = round(ORTG-home_def+avg,1),
         AdjO_R = 0,
         AdjD = round(DRTG-home_off+avg,1), 
         AdjD_R = 0,
         AdjN = AdjO-AdjD, 
         Twopt = round((FGM-TPM)/(FGA-TPA)*100,1),
         Twopt_R = 0,
         Threept = round(TPM/TPA*100,1), 
         Threept_R = 0,
         ThreeptVolume = round(TPA/FGA*100,1), 
         ThreeptVolume_R = 0, 
         eFG = round(eFG.*100,1), 
         eFG_R = 0, 
         oeFG = round(deFG.*100,1), 
         oeFG_R = 0,
         Oreb = round(ORB.*100,1),
         Oreb_R = 0, 
         Dreb = round((1-DRB.)*100,1),
         Dreb_R = 0, 
         Tov = round(TOrate*100,1), 
         Tov_R = 0, 
         oTov = round(dTOrate*100,1), 
         oTov_R = 0, 
         FTA = round(FTrate*100,1), 
         FTA_R = 0, 
         oFTA = round(dFTrate*100,1), 
         oFTA_R = 0, 
         AFGM = round(AST/FGM*100,1), 
         AFGM_R = 0) %>%
  select(Team, ORTG, DRTG, NETRTG, AdjO, AdjO_R, AdjD, AdjD_R, AdjN, Twopt, Twopt_R, Threept, Threept_R, ThreeptVolume, ThreeptVolume_R, eFG, eFG_R, oeFG, oeFG_R, Oreb, Oreb_R, Dreb, Dreb_R, Tov, Tov_R, oTov, oTov_R, FTA, FTA_R, oFTA, oFTA_R, AFGM, AFGM_R)


advanced_numbers <- read.csv("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Advanced Game Stats.csv")


advanced_numbers[nrow(advanced_numbers) + 1,] <- c(data$Team, data$ORTG, data$DRTG, data$NETRTG, data$AdjO, data$AdjO_R, data$AdjD, data$AdjD_R, data$AdjN, data$Twopt, data$Twopt_R, data$Threept, data$Threept_R, data$ThreeptVolume, data$ThreeptVolume_R, data$eFG, data$eFG_R, data$oeFG, data$oeFG_R, data$Oreb, data$Oreb_R, data$Dreb, data$Dreb_R, data$Tov, data$Tov_R, data$oTov, data$oTov_R, data$FTA, data$FTA_R, data$oFTA, data$oFTA_R, data$AFGM, data$AFGM_R)
advanced_numbers[,2:33]<- as.numeric(unlist(advanced_numbers[,2:33]))

advanced_numbers <- advanced_numbers %>%
  mutate(Ortg_R = round(rank(desc(Ortg)),0),
         Drtg_R = round(rank((Drtg)),0),
         Net_R = round(rank(desc(Net)),0),
         AdjO_R = round(rank(desc(AdjO)),0),
         AdjD_R = round(rank((AdjD)),0),
         AdjN_R = round(rank(desc(AdjN)),0),
         Twopt_R = round(rank(desc(Twopt)),0),
         Threept_R = round(rank(desc(Treept)),0),
         ThreeptVolume_R = round(rank(desc(ThreeptVolume)),0),
         eFG_R = round(rank(desc(eFG)),0),
         oeFG_R = round(rank((oeFG)),0),
         Oreb_R = round(rank(desc(Oreb)),0),
         Dreb_R = round(rank((Dreb)),0),
         Tov_R = round(rank((Tov)),0),
         oTov_R = round(rank(desc(oTov)),0),
         FTA_R = round(rank(desc(FTA)),0),
         oFTA_R = round(rank((oFTA)),0),
         AFGM_R = round(rank(desc(AFGM)),0)
  ) %>%
  filter(Team == away_team) %>%
  select(Ortg_R, Drtg_R, Net_R, AdjO_R, AdjD_R, AdjN_R, Twopt_R, Threept_R, ThreeptVolume_R, eFG_R, oeFG_R, Oreb_R, Dreb_R, Tov_R, oTov_R, FTA_R, oFTA_R, AFGM_R)

total_data <- read.csv("Advanced Practice Data.csv")

df <- data.frame(Stat = total_data$Stat,
                 Number = c(round(data$ORTG,1), round(data$DRTG,1), round(data$NETRTG,1), round(data$AdjO,1), round(data$AdjD,1), round(data$AdjN,1), round(data$Twopt,1), round(data$Threept,1), round(data$ThreeptVolume,1), round(data$eFG,1), round(data$oeFG,1), round(data$Oreb,1), round(data$Dreb,1), round(data$Tov,1), round(data$oTov,1), round(data$FTA,1), round(data$oFTA,1), round(data$AFGM,1)),
                 Rank = c(advanced_numbers$Ortg_R, advanced_numbers$Drtg_R, advanced_numbers$Net_R, advanced_numbers$AdjO_R, advanced_numbers$AdjD_R, advanced_numbers$AdjN_R, advanced_numbers$Twopt_R, advanced_numbers$Threept_R, advanced_numbers$ThreeptVolume_R, advanced_numbers$eFG_R, advanced_numbers$oeFG_R, advanced_numbers$Oreb_R, advanced_numbers$Dreb_R, advanced_numbers$Tov_R, advanced_numbers$oTov_R, advanced_numbers$FTA_R, advanced_numbers$oFTA_R, advanced_numbers$AFGM_R),
                 Explanation = total_data$Explanation)

df %>%
  gt() %>%
  cols_label(Stat = "Stat",
             Number = "#",
             Rank = "NCAA Rank",
             Explanation = "Description"
  ) %>%
  tab_header(
    title = md(paste0(away_team," Advanced Numbers")),
    #subtitle = paste0(home_team, " vs. ", away_team)
  )  %>%
  # text_transform(
  #   locations = cells_body(vars(url)),
  #   fn = function(x) {
  #     web_image(url = x,
  #               height = px(22.5))
  #   }
  # ) %>%
  data_color(
    columns = vars(Rank),
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
    columns = vars(Stat, Number, Rank, Explanation)
  ) %>%
  cols_align(
    align = "center",
    columns = vars(Rank)
  ) %>%
  cols_width(vars(Rank) ~ px(65),
             vars(Explanation) ~ px(300),
             vars(Stat) ~ px(100),
             vars(Number) ~ px(45)
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
      rows = Stat == "League Average"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "floralwhite"),
    locations = cells_body(
      rows = Stat == "League Average")
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
  gtsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Away Advanced Game Data.png", expand = 0)

# data <- get_team_stats(play_by_play_data = get_play_by_play(ncaa_game_id), include_transition =F)
# home_team <- data$Home[1]
# away_team <- data$Away[1]
# 
# 
# 
# data <- data %>%
#   filter(Team == team_name) %>%
#   mutate(AdjO = round(ORTG-opp_def+avg,1),
#          AdjO_R = 0,
#          AdjD = round(DRTG-opp_off+avg,1), 
#          AdjD_R = 0,
#          AdjN = AdjO-AdjD, 
#          Twopt = round((FGM-TPM)/(FGA-TPA)*100,1),
#          Twopt_R = 0,
#          Threept = round(TPM/TPA*100,1), 
#          Threept_R = 0,
#          ThreeptVolume = round(TPA/FGA*100,1), 
#          ThreeptVolume_R = 0, 
#          eFG = round(eFG.*100,1), 
#          eFG_R = 0, 
#          oeFG = round(deFG.*100,1), 
#          oeFG_R = 0,
#          Oreb = round(ORB.*100,1),
#          Oreb_R = 0, 
#          Dreb = round((1-DRB.)*100,1),
#          Dreb_R = 0, 
#          Tov = round(TOrate*100,1), 
#          Tov_R = 0, 
#          oTov = round(dTOrate*100,1), 
#          oTov_R = 0, 
#          FTA = round(FTrate*100,1), 
#          FTA_R = 0, 
#          oFTA = round(dFTrate*100,1), 
#          oFTA_R = 0, 
#          AFGM = round(AST/FGM*100,1), 
#          AFGM_R = 0) %>%
#   select(Team, ORTG, DRTG, NETRTG, AdjO, AdjO_R, AdjD, AdjD_R, AdjN, Twopt, Twopt_R, Threept, Threept_R, ThreeptVolume, ThreeptVolume_R, eFG, eFG_R, oeFG, oeFG_R, Oreb, Oreb_R, Dreb, Dreb_R, Tov, Tov_R, oTov, oTov_R, FTA, FTA_R, oFTA, oFTA_R, AFGM, AFGM_R)
# 
# 
# advanced_numbers <- read.csv("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Advanced Game Stats.csv")
# 
# 
# advanced_numbers[nrow(advanced_numbers) + 1,] <- c(data$Team, data$ORTG, data$DRTG, data$NETRTG, data$AdjO, data$AdjO_R, data$AdjD, data$AdjD_R, data$AdjN, data$Twopt, data$Twopt_R, data$Threept, data$Threept_R, data$ThreeptVolume, data$ThreeptVolume_R, data$eFG, data$eFG_R, data$oeFG, data$oeFG_R, data$Oreb, data$Oreb_R, data$Dreb, data$Dreb_R, data$Tov, data$Tov_R, data$oTov, data$oTov_R, data$FTA, data$FTA_R, data$oFTA, data$oFTA_R, data$AFGM, data$AFGM_R)
# advanced_numbers[,2:33]<- as.numeric(unlist(advanced_numbers[,2:33]))
# 
# advanced_numbers <- advanced_numbers %>%
#   mutate(Ortg_R = round(rank(desc(Ortg)),0),
#          Drtg_R = round(rank((Drtg)),0),
#          Net_R = round(rank(desc(Net)),0),
#          AdjO_R = round(rank(desc(AdjO)),0),
#          AdjD_R = round(rank((AdjD)),0),
#          AdjN_R = round(rank(desc(AdjN)),0),
#          Twopt_R = round(rank(desc(Twopt)),0),
#          Threept_R = round(rank(desc(Treept)),0),
#          ThreeptVolume_R = round(rank(desc(ThreeptVolume)),0),
#          eFG_R = round(rank(desc(eFG)),0),
#          oeFG_R = round(rank((oeFG)),0),
#          Oreb_R = round(rank(desc(Oreb)),0),
#          Dreb_R = round(rank((Dreb)),0),
#          Tov_R = round(rank((Tov)),0),
#          oTov_R = round(rank(desc(oTov)),0),
#          FTA_R = round(rank(desc(FTA)),0),
#          oFTA_R = round(rank((oFTA)),0),
#          AFGM_R = round(rank(desc(AFGM)),0)
#   ) %>%
#   filter(Team == team_name) %>%
#   select(Ortg_R, Drtg_R, Net_R, AdjO_R, AdjD_R, AdjN_R, Twopt_R, Threept_R, ThreeptVolume_R, eFG_R, oeFG_R, Oreb_R, Dreb_R, Tov_R, oTov_R, FTA_R, oFTA_R, AFGM_R)
# 
# total_data <- read.csv("Advanced Practice Data.csv")
# 
# df <- data.frame(Stat = total_data$Stat,
#                  Number = c(round(data$ORTG,1), round(data$DRTG,1), round(data$NETRTG,1), round(data$AdjO,1), round(data$AdjD,1), round(data$AdjN,1), round(data$Twopt,1), round(data$Threept,1), round(data$ThreeptVolume,1), round(data$eFG,1), round(data$oeFG,1), round(data$Oreb,1), round(data$Dreb,1), round(data$Tov,1), round(data$oTov,1), round(data$FTA,1), round(data$oFTA,1), round(data$AFGM,1)),
#                  Rank = c(advanced_numbers$Ortg_R, advanced_numbers$Drtg_R, advanced_numbers$Net_R, advanced_numbers$AdjO_R, advanced_numbers$AdjD_R, advanced_numbers$AdjN_R, advanced_numbers$Twopt_R, advanced_numbers$Threept_R, advanced_numbers$ThreeptVolume_R, advanced_numbers$eFG_R, advanced_numbers$oeFG_R, advanced_numbers$Oreb_R, advanced_numbers$Dreb_R, advanced_numbers$Tov_R, advanced_numbers$oTov_R, advanced_numbers$FTA_R, advanced_numbers$oFTA_R, advanced_numbers$AFGM_R),
#                  Explanation = total_data$Explanation)
# 
# df %>%
#   gt() %>%
#   cols_label(Stat = "Stat",
#              Number = "#",
#              Rank = "NCAA Rank",
#              Explanation = "Description"
#   ) %>%
#   tab_header(
#     title = md("Advanced Numbers"),
#     subtitle = paste0(home_team, " vs. ", away_team)
#   )  %>%
#   # text_transform(
#   #   locations = cells_body(vars(url)),
#   #   fn = function(x) {
#   #     web_image(url = x,
#   #               height = px(22.5))
#   #   }
#   # ) %>%
#   data_color(
#     columns = vars(Rank),
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
#     columns = vars(Stat, Number, Rank, Explanation)
#   ) %>%
#   cols_align(
#     align = "center",
#     columns = vars(Rank)
#   ) %>%
#   cols_width(vars(Rank) ~ px(65),
#              vars(Explanation) ~ px(300),
#              vars(Stat) ~ px(100),
#              vars(Number) ~ px(45)
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
#       rows = Stat == "League Average"
#     )
#   ) %>%
#   tab_style(
#     style = cell_fill(color = "floralwhite"),
#     locations = cells_body(
#       rows = Stat == "League Average")
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
#   gtsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Advanced Game Data.png", expand = 0)
