library(hoopR)
library(bigballR)
library(ncaahoopR)
library(gt)
library(cowplot)
library(ggpubr)

setwd("C:/Users/Bryce Haase/Desktop/UMass Basketball")

game_id <- 401408636
ncaa_game_id <- 5255827

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
  gtsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\General.png")


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
  gtsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Overview and Four Factors.png")

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
  gtsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Shot Locations.png")

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
  gtsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Shot Percentages.png")


setwd("C:/Users/Bryce Haase/Desktop/UMass Basketball")
plot0 <- ggdraw() + draw_image("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\General.png", scale = 1)
plot1 <- ggdraw() + draw_image("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Overview and Four Factors.png", scale = 1)
plot2 <- ggdraw() + draw_image("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Shot Percentages.png", scale = 1)
plot3 <- ggdraw() + draw_image("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Shot Locations.png", scale = 1)
total_plot<-ggarrange(plot0, plot1, plot2, plot3, ncol=1, nrow=4) + bgcolor("white")
#annotate_figure(total_plot, top = text_grob(paste0(home_team, ": ", data$home_score[i], " | ", away_team, ": ", data$away_score[i]), color = "black", face = "bold", family = "Consolas", size = 25))
ggsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\All Data.png")