library(bigballR)
library(hoopR)
library(ncaahoopR)
library(gt)
library(cowplot)
library(ggpubr)
library(dplyr)

team <- "Richmond"

schedule <- get_team_schedule(team.name = team, season = "2022-23")

wins <- c(schedule$Game_ID[c(1,2,6,9,10,12,13,15,16,17,19)])
losses <- c(schedule$Game_ID[c(3,4,5,7,8,11,14,18,20)])

setwd("C:/Users/Bryce Haase/Desktop/UMass Basketball")

total_data <- read.csv("total_data.csv")
total_data[c("off_rtg", "efg", "tov", "OReb", "FTR", "layup", "jumper", "three_pointer", "layup_per",  "jumper_per", "three_pointer_per")] <- sapply(total_data[c("off_rtg", "efg", "tov", "OReb", "FTR", "layup", "jumper", "three_pointer", "layup_per",  "jumper_per", "three_pointer_per")],as.numeric)


# for (i in 1:14) {
#   if ((schedule$Home == "Massachusetts" && schedule$Home_Score[i] > schedule$Away_Score[i]) || (schedule$Away == "Massachusetts" && schedule$Away_Score[i] > schedule$Home_Score[i])) {
#     wins <- c(wins, schedule$Game_ID[i])
#   }
#   else {
#     losses <- c(losses, schedule$Game_ID[i])
#   }
# }

losses <- get_team_stats(play_by_play_data = get_play_by_play(game_ids = losses), include_transition = T)
wins <- get_team_stats(play_by_play_data = get_play_by_play(game_ids = wins), include_transition = T)

for (i in 1:length(losses$Team)) {
  if (losses$Team[i] != team) {
    losses$Team[i] = "Opponent"
  }
}

losses <- losses %>% 
  group_by(Team)  %>%
  summarise(Points = mean(PTS),
            Allowed = mean(dPTS),
            Pace = mean(oPOSS),
            Poss = sum(oPOSS),
            OffRtg = mean(ORTG),
            DefRtg = mean(DRTG),
            FGA = sum(FGA),
            FGM = sum(FGM),
            TPA = sum(TPA),
            TPM = sum(TPM),
            FTA = sum(FTA),
            FTM = sum(FTM),
            RIMM = sum(RIMM),
            RIMA = sum(RIMA),
            ORB = sum(ORB),
            TO = sum(TO),
            Trans = mean(oTransPCT),
            TransPTS = mean(PTS_trans),
            TransOrtg = mean(ORTG_trans),
            HalfOrtg = mean(ORTG_half)
            )

losses <- losses %>%
  mutate(Points = round(Points,1),
         Pace = round(Pace,1),
    
         MIDM = FGM - RIMM - TPM,
         MIDA = FGA - RIMA - TPA,
         layup = round(RIMA/FGA*100,1),
         jumper = round(MIDA/FGA*100,1),
         three_pointer = round(TPA/FGA*100,1),
         layup_perc = round(pnorm((layup/100-mean(total_data$layup))/sd(total_data$layup)), 2),
         jumper_perc = round(pnorm((jumper/100-mean(total_data$jumper))/sd(total_data$jumper)), 2),
         three_pointer_perc = round(pnorm((three_pointer/100-mean(total_data$three_pointer))/sd(total_data$three_pointer)), 2),
         
         efg = round(((MIDM+RIMM+(1.5*TPM))/FGA)*100,1),
         tov = round(TO/Poss*100,1),
         OReb = round(ORB/(FGA-FGM)*100,1),
         FTR = round(FTA/FGA*100,1),
         efg_perc = round(pnorm((efg/100-mean(total_data$efg))/sd(total_data$efg)), 2),
         tov_perc = round(pnorm(-1*((tov/100-mean(total_data$tov))/sd(total_data$tov))), 2),
         OReb_perc = round(pnorm((OReb/100-mean(total_data$OReb))/sd(total_data$OReb)), 2),
         FTR_perc = round(pnorm((FTR/100-mean(total_data$FTR))/sd(total_data$FTR)), 2),
         
         layup_tot = paste0(RIMM, "/", RIMA),
         layup_per = round(RIMM/RIMA*100,1),
         jumper_tot = paste0(FGM-RIMM-TPM, "/", FGA-RIMA-TPA),
         jumper_per = (round((FGM-RIMM-TPM)/(FGA-RIMA-TPA)*100,1)),
         three_pointer_tot = paste0(TPM,"/",TPA),
         three_pointer_per = round(TPM/TPA*100,1),
         layup_per_perc = round(pnorm((layup_per/100-mean(total_data$layup_per))/sd(total_data$layup_per)), 2),
         jumper_per_perc = round(pnorm((jumper_per/100-mean(total_data$jumper_per))/sd(total_data$jumper_per)), 2),
         three_pointer_per_perc = round(pnorm((TPM/TPA-mean(total_data$three_pointer_per))/sd(total_data$three_pointer_per)), 2),
         
         off_rtg_perc = round(pnorm((OffRtg-mean(total_data$off_rtg))/sd(total_data$off_rtg)), 2),
         score = Points,
         poss = Pace,
         off_rtg = round(OffRtg,1),
         halfcourt = (1-Trans),
         halfcourt_rtg = round(HalfOrtg,1),
         trans = Trans,
         trans_rtg = round(TransOrtg,1),
         
         url = c("https://media.istockphoto.com/id/1237873935/vector/l-loser-hand-sign.jpg?s=612x612&w=0&k=20&c=Os-DBpNsar0D9KoIhEwrBpNtZoEJ3Y_fdKETPEWwVno=", "https://media.istockphoto.com/id/1237873935/vector/l-loser-hand-sign.jpg?s=612x612&w=0&k=20&c=Os-DBpNsar0D9KoIhEwrBpNtZoEJ3Y_fdKETPEWwVno="),
         team = c("Offense in Losses", "Defense in Losses"),
         )

for (i in 1:length(wins$Team)) {
  if (wins$Team[i] != team) {
    wins$Team[i] = "Opponent"
  }
}

wins <- wins %>% 
  group_by(Team)  %>%
  summarise(Points = mean(PTS),
            Allowed = mean(dPTS),
            Pace = mean(oPOSS),
            Poss = sum(oPOSS),
            OffRtg = mean(ORTG),
            DefRtg = mean(DRTG),
            FGA = sum(FGA),
            FGM = sum(FGM),
            TPA = sum(TPA),
            TPM = sum(TPM),
            FTA = sum(FTA),
            FTM = sum(FTM),
            RIMM = sum(RIMM),
            RIMA = sum(RIMA),
            ORB = sum(ORB),
            TO = sum(TO),
            Trans = mean(oTransPCT),
            TransPTS = mean(PTS_trans),
            TransOrtg = mean(ORTG_trans),
            HalfOrtg = mean(ORTG_half)
  )

wins <- wins %>%
  mutate(Points = round(Points,1),
         Pace = round(Pace,1),
         
         MIDM = FGM - RIMM - TPM,
         MIDA = FGA - RIMA - TPA,
         layup = round(RIMA/FGA*100,1),
         jumper = round(MIDA/FGA*100,1),
         three_pointer = round(TPA/FGA*100,1),
         layup_perc = round(pnorm((layup/100-mean(total_data$layup))/sd(total_data$layup)), 2),
         jumper_perc = round(pnorm((jumper/100-mean(total_data$jumper))/sd(total_data$jumper)), 2),
         three_pointer_perc = round(pnorm((three_pointer/100-mean(total_data$three_pointer))/sd(total_data$three_pointer)), 2),
         
         efg = round(((MIDM+RIMM+(1.5*TPM))/FGA)*100,1),
         tov = round(TO/Poss*100,1),
         OReb = round(ORB/(FGA-FGM)*100,1),
         FTR = round(FTA/FGA*100,1),
         efg_perc = round(pnorm((efg/100-mean(total_data$efg))/sd(total_data$efg)), 2),
         tov_perc = round(pnorm(-1*((tov/100-mean(total_data$tov))/sd(total_data$tov))), 2),
         OReb_perc = round(pnorm((OReb/100-mean(total_data$OReb))/sd(total_data$OReb)), 2),
         FTR_perc = round(pnorm((FTR/100-mean(total_data$FTR))/sd(total_data$FTR)), 2),
         
         layup_tot = paste0(RIMM, "/", RIMA),
         layup_per = round(RIMM/RIMA*100,1),
         jumper_tot = paste0(FGM-RIMM-TPM, "/", FGA-RIMA-TPA),
         jumper_per = (round((FGM-RIMM-TPM)/(FGA-RIMA-TPA)*100,1)),
         three_pointer_tot = paste0(TPM,"/",TPA),
         three_pointer_per = round(TPM/TPA*100,1),
         layup_per_perc = round(pnorm((layup_per/100-mean(total_data$layup_per))/sd(total_data$layup_per)), 2),
         jumper_per_perc = round(pnorm((jumper_per/100-mean(total_data$jumper_per))/sd(total_data$jumper_per)), 2),
         three_pointer_per_perc = round(pnorm((TPM/TPA-mean(total_data$three_pointer_per))/sd(total_data$three_pointer_per)), 2),
         
         off_rtg_perc = round(pnorm((OffRtg-mean(total_data$off_rtg))/sd(total_data$off_rtg)), 2),
         score = Points,
         poss = Pace,
         off_rtg = round(OffRtg,1),
         halfcourt = (1-Trans),
         halfcourt_rtg = round(HalfOrtg,1),
         trans = Trans,
         trans_rtg = round(TransOrtg,1),
         
         url = c("https://content.presentermedia.com/files/clipart/00017000/17029/trophy_800_wht.jpg", "https://content.presentermedia.com/files/clipart/00017000/17029/trophy_800_wht.jpg"),
         team = c("Offense in Wins", "Defense in Wins"),
  )

offense <- rbind(wins, losses)
offense <- offense[c(1,3),]

general <- offense %>%
  select(url, team, efg, efg_perc, tov, tov_perc, OReb, OReb_perc, FTR, FTR_perc)
shot_loc  <- offense %>%
  select(url, team, layup, layup_perc, jumper, jumper_perc, three_pointer, three_pointer_perc)
shot_per  <- offense %>%
  select(url, team, layup_tot, layup_per, layup_per_perc, jumper_tot, jumper_per, jumper_per_perc, three_pointer_tot, three_pointer_per, three_pointer_per_perc)
overview  <- offense %>%
  select(url, team, score, poss, off_rtg, off_rtg_perc, halfcourt, halfcourt_rtg, trans, trans_rtg)

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
ggsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\All Data Offense.png")

defense <- rbind(wins, losses)
defense <- defense[c(2,4),]

general <- defense %>%
  select(url, team, efg, efg_perc, tov, tov_perc, OReb, OReb_perc, FTR, FTR_perc)
shot_loc  <- defense %>%
  select(url, team, layup, layup_perc, jumper, jumper_perc, three_pointer, three_pointer_perc)
shot_per  <- defense %>%
  select(url, team, layup_tot, layup_per, layup_per_perc, jumper_tot, jumper_per, jumper_per_perc, three_pointer_tot, three_pointer_per, three_pointer_per_perc)
overview  <- defense %>%
  select(url, team, score, poss, off_rtg, off_rtg_perc, halfcourt, halfcourt_rtg, trans, trans_rtg)

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
        direction  = -1
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
        direction  = -1
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
        direction  = -1
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
        direction  = -1
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
ggsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\All Data Defense.png")
