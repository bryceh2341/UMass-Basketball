library(bigballR)
library(dplyr)
library(gt)
library(cowplot)
library(ggpubr)
library(gtExtras)

ncaa_game_id <- 5366223

team_name = "Massachusetts"

opp_off <- 101.3
opp_def <- 98.5
avg <- 100.3

data <- get_team_stats(play_by_play_data = get_play_by_play(ncaa_game_id), include_transition =F)
home_team <- data$Home[1]
away_team <- data$Away[1]



data <- data %>%
  filter(Team == team_name) %>%
  mutate(AdjO = round(ORTG-opp_def+avg,1),
         AdjO_R = 0,
         AdjD = round(DRTG-opp_off+avg,1), 
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
  filter(Team == team_name) %>%
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
    title = md("Advanced Numbers"),
    subtitle = paste0(home_team, " vs. ", away_team)
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
  gtsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Advanced Game Data.png", expand = 0)
