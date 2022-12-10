library(hoopR)
library(bigballR)
library(ncaahoopR)
library(gt)
library(cowplot)
library(ggpubr)

ncaa_game_id <- 5366223

setwd("C:/Users/Bryce Haase/Desktop/UMass Basketball")

team_stats <- read.csv("C:/Users/Bryce Haase/Desktop/UMass Basketball/Manual Multi Game Team Stats.csv")   #get_team_stats(play_by_play_data = get_play_by_play(ncaa_game_id), include_transition = T)
#player_stats <- get_player_stats(get_play_by_play(ncaa_game_id), multi.games = F, simple = F)

Team = c("Massachusetts", "Opponents")
home_team<-"Opponent"
away_team<-"UMass"

total_data <- read.csv("total_data.csv")
#total_data[nrow(total_data)+1,] <- c(nrow(total_data)+1, game_id, home_team, away_team, home_off_rtg, home_efg, home_tov, home_off_reb, home_ftr, home_layup_fga/home_fga, home_jumper_fga/home_fga, home_3PA/home_fga, home_layup_fgm/home_layup_fga, home_jumper_fgm/home_jumper_fga, home_3PM/home_3PA)
#total_data[nrow(total_data)+1,] <- c(nrow(total_data)+1, game_id, away_team, home_team, away_off_rtg, away_efg, away_tov, away_off_reb, away_ftr, away_layup_fga/away_fga, away_jumper_fga/away_fga, away_3PA/away_fga, away_layup_fgm/away_layup_fga, away_jumper_fgm/away_jumper_fga, away_3PM/away_3PA)
total_data[c("off_rtg", "efg", "tov", "OReb", "FTR", "layup", "jumper", "three_pointer", "layup_per",  "jumper_per", "three_pointer_per")] <- sapply(total_data[c("off_rtg", "efg", "tov", "OReb", "FTR", "layup", "jumper", "three_pointer", "layup_per",  "jumper_per", "three_pointer_per")],as.numeric)

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

URL <- c(away_team_url, home_team_url)

team_stats <- team_stats %>%
  mutate(off_rtg_perc = round(pnorm((ORTG-mean(total_data$off_rtg))/sd(total_data$off_rtg)), 2),
         efg_perc = round(pnorm((eFG.-mean(total_data$efg))/sd(total_data$efg)), 2),
         tov_perc = round(pnorm(-1*((TO/100-mean(total_data$tov))/sd(total_data$tov))), 2),
         OReb_perc = round(pnorm((ORB.-mean(total_data$OReb))/sd(total_data$OReb)), 2),
         FTR_perc = round(pnorm((FTrate-mean(total_data$FTR))/sd(total_data$FTR)), 2),
         layup_perc = round(pnorm((RIMrate-mean(total_data$layup))/sd(total_data$layup)), 2),
         jumper_perc = round(pnorm((MIDrate-mean(total_data$jumper))/sd(total_data$jumper)), 2),
         three_pointer_perc = round(pnorm((TPrate-mean(total_data$three_pointer))/sd(total_data$three_pointer)), 2),
         layup_per_perc = round(pnorm((RIM.-mean(total_data$layup_per))/sd(total_data$layup_per)), 2),
         jumper_per_perc = round(pnorm((MID.-mean(total_data$jumper_per))/sd(total_data$jumper_per)), 2),
         three_pointer_per_perc = round(pnorm((TPM/TPA-mean(total_data$three_pointer_per))/sd(total_data$three_pointer_per)), 2),
         score = PTS,
         poss = oPOSS,
         off_rtg = round(ORTG,1),
         halfcourt = (1-oTransPCT),
         halfcourt_rtg = round(ORTG_half,1),
         trans = oTransPCT,
         trans_rtg = round(ORTG_trans,1),
         team = Team,
         efg = round(eFG.*100,1),
         tov = round(TO,1),
         OReb = round(ORB.*100,1),
         FTR = round(FTrate*100,1),
         layup_tot = paste0(RIMM, "/", RIMA),
         layup_per = round(RIM.*100,1),
         jumper_tot = paste0(FGM-RIMM-TPM, "/", FGA-RIMA-TPA),
         jumper_per = (round((FGM-RIMM-TPM)/(FGA-RIMA-TPA)*100,1)),
         three_pointer_tot = paste0(TPM,"/",TPA),
         three_pointer_per = round(TPM/TPA*100,1),
         layup = round(RIMrate*100,1),
         jumper = round(MIDrate*100,1),
         three_pointer = round(TPrate*100,1),
         team = Team,
         url=URL
  )


general <- team_stats %>%
  select(url, team, efg, efg_perc, tov, tov_perc, OReb, OReb_perc, FTR, FTR_perc)
#general <- general[,4:13]
shot_loc  <- team_stats %>%
  select(url, team, layup, layup_perc, jumper, jumper_perc, three_pointer, three_pointer_perc)
#shot_loc <- shot_loc[,4:11]
shot_per  <- team_stats %>%
  select(url, team, layup_tot, layup_per, layup_per_perc, jumper_tot, jumper_per, jumper_per_perc, three_pointer_tot, three_pointer_per, three_pointer_per_perc)
#shot_per <- shot_per[,4:14]
overview  <- team_stats %>%
  select(url, team, score, poss, off_rtg, off_rtg_perc, halfcourt, halfcourt_rtg, trans, trans_rtg)
#overview <- overview[,4:13]

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
  # fmt_percent(
  #   columns = vars(efg, tov, OReb, FTR),
  #   decimals = 1
  # )  %>%
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
  # fmt_percent(
  #   columns = vars(layup, jumper, three_pointer),
  #   decimals = 1
  # )  %>%
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
  # fmt_percent(
  #   columns = vars(layup_per, jumper_per, three_pointer_per),
  #   decimals = 1
  # )  %>%
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