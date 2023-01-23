library(hoopR)
library(bigballR)
library(ncaahoopR)
library(gt)
library(cowplot)
library(ggpubr)

setwd("C:/Users/Bryce Haase/Desktop/UMass Basketball")

game_id <- 401484441
ncaa_game_id <- 5391931

full_player_stats <- read.csv("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\2021_22_player_stats.csv")

#team_name = "Massachusetts"
ESPN_Home <- "Saint Joseph's"
ESPN_Away <- "UMass"

#opp_off <- 105.0
#opp_def <- 92.1
home_off <- 102.5
home_def <- 104.8
away_off <- 102.0
away_def <- 101.9
avg <- 104.2

home_color <- 'black'
away_color <- 'red'

# game_shot_chart(game_id, heatmap = F)
# ggsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Shot Chart.png", width = 7, height = 5, dpi = 300, limitsize = F)
# game_shot_chart(game_id, heatmap = T)
# ggsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Heat Map.png")


player_stats <- get_player_stats(get_play_by_play(ncaa_game_id), multi.games = F, simple = F)


home_roster <- ncaahoopR::get_roster("Saint Joe's", season = "2022-23")
#home_roster <- ncaahoopR::get_roster(ESPN_Home, season = "2022-23")
home_roster <- home_roster %>%
  mutate(Jersey = number) %>%
  select(Jersey, player_image)

home_roster <- merge(home_roster, get_team_roster(season = "2022-23", team.name = player_stats$Home[1]), by="Jersey")

#away_roster <- ncaahoopR::get_roster("URI", season = "2022-23")
away_roster <- ncaahoopR::get_roster(ESPN_Away, season = "2022-23")
away_roster <- away_roster %>%
  mutate(Jersey = number) %>%
  select(Jersey, player_image)

away_roster <- merge(away_roster, get_team_roster(season = "2022-23", team.name = player_stats$Away[1]), by="Jersey")

team_stats <- get_team_stats(play_by_play_data = get_play_by_play(ncaa_game_id), include_transition = T)

total_data <- read.csv("total_data.csv")
total_data[c("off_rtg", "efg", "tov", "OReb", "FTR", "layup", "jumper", "three_pointer", "layup_per",  "jumper_per", "three_pointer_per")] <- sapply(total_data[c("off_rtg", "efg", "tov", "OReb", "FTR", "layup", "jumper", "three_pointer", "layup_per",  "jumper_per", "three_pointer_per")],as.numeric)

id_table <- ids

home_team_url <- "https://upload.wikimedia.org/wikipedia/commons/thumb/d/dd/NCAA_logo.svg/1042px-NCAA_logo.svg.png"
away_team_url <- "https://upload.wikimedia.org/wikipedia/commons/thumb/d/dd/NCAA_logo.svg/1042px-NCAA_logo.svg.png"

for (i in 1:length(id_table$team)) {
  if (id_table$team[i] == ESPN_Home) {
    home_team_url <- paste0("https://a.espncdn.com/combiner/i?img=/i/teamlogos/ncaa/500/",id_table$id[i],".png&h=200&w=200")
  }
  else if (id_table$team[i] == ESPN_Away) {
    away_team_url <- paste0("https://a.espncdn.com/combiner/i?img=/i/teamlogos/ncaa/500/",id_table$id[i],".png&h=200&w=200")
  }
}

url <- c(away_team_url, home_team_url)

team_stats <- team_stats %>%
  mutate(off_rtg_perc = round(pnorm((ORTG-mean(total_data$off_rtg))/sd(total_data$off_rtg)), 2),
         efg_perc = round(pnorm((eFG.-mean(total_data$efg))/sd(total_data$efg)), 2),
         tov_perc = round(pnorm(-1*((TO/oPOSS-mean(total_data$tov))/sd(total_data$tov))), 2),
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
         tov = round(TO/oPOSS*100,1),
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
         url=url
  )


general <- team_stats %>%
  select(url, team, efg, efg_perc, tov, tov_perc, OReb, OReb_perc, FTR, FTR_perc)
general <- general[,4:13]
shot_loc  <- team_stats %>%
  select(url, team, layup, layup_perc, jumper, jumper_perc, three_pointer, three_pointer_perc)
shot_loc <- shot_loc[,4:11]
shot_per  <- team_stats %>%
  select(url, team, layup_tot, layup_per, layup_per_perc, jumper_tot, jumper_per, jumper_per_perc, three_pointer_tot, three_pointer_per, three_pointer_per_perc)
shot_per <- shot_per[,4:14]
overview  <- team_stats %>%
  select(url, team, score, poss, off_rtg, off_rtg_perc, halfcourt, halfcourt_rtg, trans, trans_rtg)
overview <- overview[,4:13]

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
