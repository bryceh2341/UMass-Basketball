# library(DT)
# library(ncaahoopR)
# library(shiny)
# library(rvest)
# library(ggpubr)
# library(DT)
# library(gt)
# library(cowplot)
# library(bigballR)
# library(shinydashboard)
# 
# teams <- teamids %>%
#   filter(Season == "2021-22")
# 
# player_stats <- read.csv("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\2021_22_player_stats.csv")
# 
# get_roster <- function(team_name, season) {
#   roster <- get_team_roster(season = season, team.name = team_name)
#   roster <- roster %>%
#     select(CleanName, Yr, Jersey, Pos, Ht)
# }
# 
# get_basic_stats <- function(team_name) {
#   #ind_schedule <- get_team_schedule(season = season, team.name = team_name)
#   #player_stats <- get_player_stats(play_by_play_data = get_play_by_play(ind_schedule$Game_ID), multi.games = T, simple = F)
#   basic_data <- player_stats %>%
#     filter(Team == team_name) %>%
#     mutate(PPG = round(PTS/GP,1),
#            APG = round(AST/GP,1),
#            ORPG = round(ORB/GP,1),
#            DRPG = round(DRB/GP,1),
#            RPG = round((ORB+DRB)/GP,1),
#            TOVPG = round(TOV/GP,1),
#            STLPG = round(STL/GP,1),
#            MPG = round(MINS/GP,1),
#            TWOA = round((RIMA+MIDA)/GP,1),
#            TWOM = round((RIMM+MIDM)/GP,1),
#            THREEA = round(TPA/GP,1),
#            THREEM = round(TPM/GP,1),
#            TWOPCT = round(TWOM/TWOA*100,1),
#            THREEPCT = round(THREEM/THREEA*100,1),
#            FTAPG = round(FTA/GP,1),
#            FTMPG = round(FTM/GP,1),
#            FTPCT = round(FT.*100,1),
#            Player = gsub(".", " ", Player, fixed=TRUE)) %>%
#     filter(Team == team_name) %>%
#     select(Player, GP, GS, MPG, PPG, RPG, APG, STLPG, TOVPG, TWOA, TWOM, TWOPCT, THREEM, THREEA, THREEPCT, FTAPG, FTMPG, FTPCT, ORPG, DRPG) %>%
#     arrange(desc(MPG))
# }
# 
# ui <- fluidPage(
#   titlePanel(title=div(img(src="https://upload.wikimedia.org/wikipedia/commons/thumb/0/06/UMass_Amherst_Athletics_logo.svg/2505px-UMass_Amherst_Athletics_logo.svg.png", height = 80, width =70, align = "right"))),
#   
#   h1(p(strong("UMass Basketball Dashboard"), align = "center", style = "color:darkred")),
#   
#   # sidebarPanel(selectInput("team_name", h1("select box"),
#   #                          label = "Team",
#   #                          choices = c(teams$Team
#   #                          ))
#   #              
#   # ),
#   # sidebarPanel(selectInput("season", h1("select box"),
#   #                          label = "Season",
#   #                          choices = c("2014-15",
#   #                                      "2015-16",
#   #                                      "2016-17",
#   #                                      "2017-18",
#   #                                      "2018-19",
#   #                                      "2019-20", 
#   #                                      "2020-21", 
#   #                                      "2021-22"
#   #                          ))
#   #              
#   # ),
#   sidebar <- dashboardSidebar(
#     sidebarMenu(
#       menuItem("Player Stats", tabName = "player_stats_dash", icon = icon("dashboard")),
#       menuItem("Team Stats", tabName = "team_stats_dash", icon = icon("th"))
#     )
#   ),
#   
#   body <- dashboardBody(
#     tabItems(
#       tabItem(tabName = "player_stats_dash",
#               h2("Player Stats"),
#               tabsetPanel(type = "tabs",
#                           tabPanel("Basic Stats", DT::dataTableOutput("basic_player_stats")),
#                           tabPanel("Shooting", DT::dataTableOutput("shotting")),
#                           tabPanel("Advanced", DT::dataTableOutput("advanced")),),
#               sidebarPanel(selectInput("team_name", h1("select box"),
#                                        label = "Team",
#                                        choices = c(teams$Team
#                                        ))
# 
#               ),
#               sidebarPanel(selectInput("season", h1("select box"),
#                                        label = "Season",
#                                        choices = c("2014-15",
#                                                    "2015-16",
#                                                    "2016-17",
#                                                    "2017-18",
#                                                    "2018-19",
#                                                    "2019-20", 
#                                                    "2020-21", 
#                                                    "2021-22"
#                                        ))
#                            
#               ),
#               sidebarPanel(selectInput("type", h1("select box"),
#                                        label = "Type",
#                                        choices = c("Per Game",
#                                                    "Totals",
#                                                    "Per 40 Minutes",
#                                                    "Per 70 Possessions"
#                                        ))
#                            
#               )
#       ),
#       
#       tabItem(tabName = "team_stats_dash",
#               h2("Team Stats"),
#               tabsetPanel(type = "tabs",
#                           tabPanel("Roster", DT::dataTableOutput("roster"))),
#               sidebarPanel(selectInput("team_name", h1("select box"),
#                                        label = "Team",
#                                        choices = c(teams$Team
#                                        ))
#                            
#               ),
#               sidebarPanel(selectInput("season", h1("select box"),
#                                        label = "Season",
#                                        choices = c("2014-15",
#                                                    "2015-16",
#                                                    "2016-17",
#                                                    "2017-18",
#                                                    "2018-19",
#                                                    "2019-20", 
#                                                    "2020-21", 
#                                                    "2021-22"
#                                        ))
#                            
#               )
#       )
#     )
#   ),
#   
#   # Put them together into a dashboardPage
#   dashboardPage(
#     dashboardHeader(title = ""),
#     sidebar,
#     body
#   ),
#   
#   mainPanel(
#     # Output: Tabset w/ plot, summary, and table ----
#     tabsetPanel(type = "tabs",
#                 tabPanel("Roster", DT::dataTableOutput("roster")),
#                 tabPanel("Basic Stats", DT::dataTableOutput("basic_player_stats")),
#     #setBackgroundColor("lightgrey")
#     )
#   )
#   
# )
# 
# server <- function(input, output) {
#   output$basic_player_stats = DT::renderDataTable({
#     #basic_data
#     #get_basic_stats(team_name = input$team_name, season = input$season)
#     DT::datatable(get_basic_stats(team_name = input$team_name), options = list(pageLength = 25))
#   })
#   output$roster = DT::renderDataTable({
#     printable_roster #%>%
#     #mutate(print_image = paste('<img src="',player_image,'">'))
#     #escape=FALSE
#   })
# }
# 
# shinyApp(ui, server)

#--------------------------------------------------------------------------------------------------------------------------------------------
library(DT)
library(ncaahoopR)
library(shiny)
library(rvest)
library(ggpubr)
library(DT)
library(gt)
library(cowplot)
library(bigballR)
library(shinythemes)

teams <- teamids %>%
  filter(Season == "2021-22")

player_stats <- read.csv("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\2021_22_player_stats.csv")
full_player_stats <- read.csv("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\2021_22_full_player_stats.csv")


get_roster <- function(team_name, season) {
  roster <- get_team_roster(season = season, team.name = team_name)
  roster <- roster %>%
    select(CleanName, Yr, Jersey, Pos, Ht)
}

get_basic_stats <- function(team_name, season, type = "Per Game") {
  #ind_schedule <- get_team_schedule(season = season, team.name = team_name)
  #player_stats <- get_player_stats(play_by_play_data = get_play_by_play(ind_schedule$Game_ID), multi.games = T, simple = F)
  if (type == "Per Game") {
    basic_data <- full_player_stats %>%
      filter(Team %in% teams$Team) %>%
      mutate(Player = gsub(".", " ", Player, fixed=TRUE),
             #per game basic
             'Pts %ile' = round(pnorm((PPG-mean(full_player_stats$PPG[full_player_stats$MPG>=20]))/sd(full_player_stats$PPG[full_player_stats$MPG>=20]))*100,0),
             'Ast %ile' = round(pnorm((APG-mean(full_player_stats$APG[full_player_stats$MPG>=20]))/sd(full_player_stats$APG[full_player_stats$MPG>=20]))*100,0),
             'Reb %ile' = round(pnorm((RPG-mean(full_player_stats$RPG[full_player_stats$MPG>=20]))/sd(full_player_stats$RPG[full_player_stats$MPG>=20]))*100,0),
             'TOV %ile' = round(pnorm((TOVPG-mean(full_player_stats$TOVPG[full_player_stats$MPG>=20]))/sd(full_player_stats$TOVPG[full_player_stats$MPG>=20]))*100,0),
             'STL %ile' = round(pnorm((STLPG-mean(full_player_stats$STLPG[full_player_stats$MPG>=20]))/sd(full_player_stats$STLPG[full_player_stats$MPG>=20]))*100,0),
             'BLK %ile' = round(pnorm((BLKPG-mean(full_player_stats$BLKPG[full_player_stats$MPG>=20]))/sd(full_player_stats$BLKPG[full_player_stats$MPG>=20]))*100,0),
             '2PA %ile' = round(pnorm((TWOAPG-mean(full_player_stats$TWOAPG[full_player_stats$MPG>=20]))/sd(full_player_stats$TWOAPG[full_player_stats$MPG>=20]))*100,0),
             '3PA %ile' = round(pnorm((THREEA-mean(full_player_stats$THREEA[full_player_stats$MPG>=20]))/sd(full_player_stats$THREEA[full_player_stats$TPA>=25]))*100,0),
             '2% %ile' = round(pnorm((TWOPCT-mean(full_player_stats$TWOPCT[full_player_stats$FGA>=25]))/sd(full_player_stats$TWOPCT[full_player_stats$MPG>=20]))*100,0),
             '3% %ile' = round(pnorm((THREEPCT-mean(full_player_stats$THREEPCT[full_player_stats$TPA>=25]))/sd(full_player_stats$THREEPCT[full_player_stats$FGA>=25]))*100,0),
             'FTA %ile' = round(pnorm((FTAPG-mean(full_player_stats$FTAPG[full_player_stats$MPG>=20]))/sd(full_player_stats$FTAPG[full_player_stats$MPG>=20]))*100,0),
             'FT% %ile' = round(pnorm((FTPCT-mean(full_player_stats$FTPCT[full_player_stats$FTA>=25]))/sd(full_player_stats$FTPCT[full_player_stats$FTA>=25]))*100,0),
             'OReb %ile' = round(pnorm((ORPG-mean(full_player_stats$ORPG[full_player_stats$MPG>=20]))/sd(full_player_stats$ORPG[full_player_stats$MPG>=20]))*100,0),
             'DReb %ile' = round(pnorm((DRPG-mean(full_player_stats$DRPG[full_player_stats$MPG>=20]))/sd(full_player_stats$DRPG[full_player_stats$MPG>=20]))*100,0)) %>%
      filter(Team == team_name) %>%
      select(Player, GP, GS, MPG, PPG, 'Pts %ile', RPG, 'Reb %ile', APG, 'Ast %ile', STLPG, 'STL %ile', BLKPG, 'BLK %ile', TOVPG, 'TOV %ile', TWOMPG, TWOAPG, '2PA %ile', TWOPCT, '2% %ile', THREEM, THREEA, '3PA %ile', THREEPCT, '3% %ile', FTMPG, FTAPG, 'FTA %ile', FTPCT, 'FT% %ile', ORPG, 'OReb %ile', DRPG, 'DReb %ile') %>%
      arrange(desc(MPG))
    colnames(basic_data) <- c("Player", 'GP', 'GS', 'MINS', 'PTS', 'Pts %ile', 'REB', 'Reb %ile', 'AST', 'Ast %ile', 'STL', 'STL %ile', 'BLK', 'BLK %ile', 'TOV', 'TOV %ile', '2PM', '2PA', '2PA %ile', '2%', '2% %ile', '3PM', '3PA', '3PA %ile', '3%', '3% %ile', 'FTM', 'FTA', 'FTA %ile', 'FT%', 'FT% %ile', 'OReb', 'OReb %ile', 'DReb', 'DReb %ile')
    return(basic_data)
  }
  else if (type == "Totals") {
    basic_data <- full_player_stats %>%
      filter(Team %in% teams$Team) %>%
      mutate(Player = gsub(".", " ", Player, fixed=TRUE),
             #per game basic
             'Pts %ile' = round(pnorm((PTS-mean(full_player_stats$PTS[full_player_stats$MPG>=20]))/sd(full_player_stats$PTS[full_player_stats$MPG>=20]))*100,0),
             'Ast %ile' = round(pnorm((AST-mean(full_player_stats$AST[full_player_stats$MPG>=20]))/sd(full_player_stats$AST[full_player_stats$MPG>=20]))*100,0),
             'Reb %ile' = round(pnorm((REB-mean(full_player_stats$REB[full_player_stats$MPG>=20]))/sd(full_player_stats$REB[full_player_stats$MPG>=20]))*100,0),
             'TOV %ile' = round(pnorm((TOV-mean(full_player_stats$TOV[full_player_stats$MPG>=20]))/sd(full_player_stats$TOV[full_player_stats$MPG>=20]))*100,0),
             'STL %ile' = round(pnorm((STL-mean(full_player_stats$STL[full_player_stats$MPG>=20]))/sd(full_player_stats$STL[full_player_stats$MPG>=20]))*100,0),
             'BLK %ile' = round(pnorm((BLK-mean(full_player_stats$BLK[full_player_stats$MPG>=20]))/sd(full_player_stats$BLK[full_player_stats$MPG>=20]))*100,0),
             '2PA %ile' = round(pnorm((TWOA-mean(full_player_stats$TWOA[full_player_stats$MPG>=20]))/sd(full_player_stats$TWOA[full_player_stats$MPG>=20]))*100,0),
             '3PA %ile' = round(pnorm((TPA-mean(full_player_stats$TPA[full_player_stats$MPG>=20]))/sd(full_player_stats$TPA[full_player_stats$MPG>=20]))*100,0),
             '2% %ile' = round(pnorm((TWOPCT-mean(full_player_stats$TWOPCT[full_player_stats$FGA>=25]))/sd(full_player_stats$TWOPCT[full_player_stats$FGA>=25]))*100,0),
             '3% %ile' = round(pnorm((THREEPCT-mean(full_player_stats$THREEPCT[full_player_stats$TPA>=25]))/sd(full_player_stats$THREEPCT[full_player_stats$TPA>=25]))*100,0),
             'FTA %ile' = round(pnorm((FTA-mean(full_player_stats$FTA[full_player_stats$MPG>=20]))/sd(full_player_stats$FTA[full_player_stats$MPG>=20]))*100,0),
             'FT% %ile' = round(pnorm((FTPCT-mean(full_player_stats$FTPCT[full_player_stats$FTA>=25]))/sd(full_player_stats$FTPCT[full_player_stats$FTA>=25]))*100,0),
             'OReb %ile' = round(pnorm((ORB-mean(full_player_stats$ORB[full_player_stats$MPG>=20]))/sd(full_player_stats$ORB[full_player_stats$MPG>=20]))*100,0),
             'DReb %ile' = round(pnorm((DRB-mean(full_player_stats$DRB[full_player_stats$MPG>=20]))/sd(full_player_stats$DRB[full_player_stats$MPG>=20]))*100,0)) %>%
      filter(Team == team_name) %>%
      select(Player, GP, GS, MINS, PTS, 'Pts %ile', REB, 'Reb %ile', AST, 'Ast %ile', STL, 'STL %ile', BLK, 'BLK %ile', TOV, 'TOV %ile', TWOM, TWOA, '2PA %ile', TWOPCT, '2% %ile', TPM, TPA, '3PA %ile', THREEPCT, '3% %ile', FTM, FTA, 'FTA %ile', FTPCT, 'FT% %ile', ORB, 'OReb %ile', DRB, 'DReb %ile') %>%
      arrange(desc(MINS))
    colnames(basic_data) <- c("Player", 'GP', 'GS', 'MINS', 'PTS', 'Pts %ile', 'REB', 'Reb %ile', 'AST', 'Ast %ile', 'STL', 'STL %ile', 'BLK', 'BLK %ile', 'TOV', 'TOV %ile', '2PM', '2PA', '2PA %ile', '2%', '2% %ile', '3PM', '3PA', '3PA %ile', '3%', '3% %ile', 'FTM', 'FTA', 'FTA %ile', 'FT%', 'FT% %ile', 'OReb', 'OReb %ile', 'DReb', 'DReb %ile')
    return(basic_data)
  }
  else if (type == "Per 40 Minutes") {
    basic_data <- full_player_stats %>%
      filter(Team %in% teams$Team) %>%
      mutate(Player = gsub(".", " ", Player, fixed=TRUE),
             #per game basic
             'Pts %ile' = round(pnorm((PPMin-mean(full_player_stats$PPMin[full_player_stats$MPG>=20]))/sd(full_player_stats$PPMin[full_player_stats$MPG>=20]))*100,0),
             'Ast %ile' = round(pnorm((APMin-mean(full_player_stats$APMin[full_player_stats$MPG>=20]))/sd(full_player_stats$APMin[full_player_stats$MPG>=20]))*100,0),
             'Reb %ile' = round(pnorm((RPMin-mean(full_player_stats$RPMin[full_player_stats$MPG>=20]))/sd(full_player_stats$RPMin[full_player_stats$MPG>=20]))*100,0),
             'TOV %ile' = round(pnorm((TOVPMin-mean(full_player_stats$TOVPMin[full_player_stats$MPG>=20]))/sd(full_player_stats$TOVPMin[full_player_stats$MPG>=20]))*100,0),
             'STL %ile' = round(pnorm((STLPMin-mean(full_player_stats$STLPMin[full_player_stats$MPG>=20]))/sd(full_player_stats$STLPMin[full_player_stats$MPG>=20]))*100,0),
             'BLK %ile' = round(pnorm((BLKPMin-mean(full_player_stats$BLKPMin[full_player_stats$MPG>=20]))/sd(full_player_stats$BLKPMin[full_player_stats$MPG>=20]))*100,0),
             '2PA %ile' = round(pnorm((TWOAPM-mean(full_player_stats$TWOAPM[full_player_stats$MPG>=20]))/sd(full_player_stats$TWOAPM[full_player_stats$MPG>=20]))*100,0),
             '3PA %ile' = round(pnorm((THREEAPM-mean(full_player_stats$THREEAPM[full_player_stats$MPG>=20]))/sd(full_player_stats$THREEAPM[full_player_stats$MPG>=20]))*100,0),
             '2% %ile' = round(pnorm((TWOPCT-mean(full_player_stats$TWOPCT[full_player_stats$FGA>=25]))/sd(full_player_stats$TWOPCT[full_player_stats$FGA>=25]))*100,0),
             '3% %ile' = round(pnorm((THREEPCT-mean(full_player_stats$THREEPCT[full_player_stats$TPA>=25]))/sd(full_player_stats$THREEPCT[full_player_stats$TPA>=25]))*100,0),
             'FTA %ile' = round(pnorm((FTAPMin-mean(full_player_stats$FTAPMin[full_player_stats$MPG>=20]))/sd(full_player_stats$FTAPMin[full_player_stats$MPG>=20]))*100,0),
             'FT% %ile' = round(pnorm((FTPCT-mean(full_player_stats$FTPCT[full_player_stats$FTA>=25]))/sd(full_player_stats$FTPCT[full_player_stats$FTA>=25]))*100,0),
             'OReb %ile' = round(pnorm((ORPMin-mean(full_player_stats$ORPMin[full_player_stats$MPG>=20]))/sd(full_player_stats$ORPMin[full_player_stats$MPG>=20]))*100,0),
             'DReb %ile' = round(pnorm((DRPMin-mean(full_player_stats$DRPMin[full_player_stats$MPG>=20]))/sd(full_player_stats$DRPMin[full_player_stats$MPG>=20]))*100,0)) %>%
      filter(Team == team_name) %>%
      select(Player, GP, GS, MPG, PPMin, 'Pts %ile', RPMin, 'Reb %ile', APMin, 'Ast %ile', STLPMin, 'STL %ile', BLKPMin, 'BLK %ile', TOVPMin, 'TOV %ile', TWOMPM, TWOAPM, '2PA %ile', TWOPCT, '2% %ile', THREEAPM, THREEMPM, '3PA %ile', THREEPCT, '3% %ile', FTMPMin, FTAPMin, 'FTA %ile', FTPCT, 'FT% %ile', ORPMin, 'OReb %ile', DRPMin, 'DReb %ile') %>%
      arrange(desc(MPG))
    colnames(basic_data) <- c("Player", 'GP', 'GS', 'MINS', 'PTS', 'Pts %ile', 'REB', 'Reb %ile', 'AST', 'Ast %ile', 'STL', 'STL %ile', 'BLK', 'BLK %ile', 'TOV', 'TOV %ile', '2PM', '2PA', '2PA %ile', '2%', '2% %ile', '3PM', '3PA', '3PA %ile', '3%', '3% %ile', 'FTM', 'FTA', 'FTA %ile', 'FT%', 'FT% %ile', 'OReb', 'OReb %ile', 'DReb', 'DReb %ile')
    return(basic_data)
  }
  else if (type == "Per 70 Possessions") {
    basic_data <- full_player_stats %>%
      filter(Team %in% teams$Team) %>%
      mutate(Player = gsub(".", " ", Player, fixed=TRUE),
             'Pts %ile' = round(pnorm((PPPoss-mean(full_player_stats$PPPoss[full_player_stats$MPG>=20]))/sd(full_player_stats$PPPoss[full_player_stats$MPG>=20]))*100,0),
             'Ast %ile' = round(pnorm((APPoss-mean(full_player_stats$APPoss[full_player_stats$MPG>=20]))/sd(full_player_stats$APPoss[full_player_stats$MPG>=20]))*100,0),
             'Reb %ile' = round(pnorm((RPPoss-mean(full_player_stats$RPPoss[full_player_stats$MPG>=20]))/sd(full_player_stats$RPPoss[full_player_stats$MPG>=20]))*100,0),
             'TOV %ile' = round(pnorm((TOVPPoss-mean(full_player_stats$TOVPPoss[full_player_stats$MPG>=20]))/sd(full_player_stats$TOVPPoss[full_player_stats$MPG>=20]))*100,0),
             'STL %ile' = round(pnorm((STLPPoss-mean(full_player_stats$STLPPoss[full_player_stats$MPG>=20]))/sd(full_player_stats$STLPPoss[full_player_stats$MPG>=20]))*100,0),
             'BLK %ile' = round(pnorm((BLKPPoss-mean(full_player_stats$BLKPPoss[full_player_stats$MPG>=20]))/sd(full_player_stats$BLKPPoss[full_player_stats$MPG>=20]))*100,0),
             '2PA %ile' = round(pnorm((TWOAPPoss-mean(full_player_stats$TWOAPPoss[full_player_stats$MPG>=20]))/sd(full_player_stats$TWOAPPoss[full_player_stats$MPG>=20]))*100,0),
             '3PA %ile' = round(pnorm((THREEAPPoss-mean(full_player_stats$THREEAPPoss[full_player_stats$MPG>=20]))/sd(full_player_stats$THREEAPPoss[full_player_stats$MPG>=20]))*100,0),
             '2% %ile' = round(pnorm((TWOPCT-mean(full_player_stats$TWOPCT[full_player_stats$FGA>=25]))/sd(full_player_stats$TWOPCT[full_player_stats$FGA>=25]))*100,0),
             '3% %ile' = round(pnorm((THREEPCT-mean(full_player_stats$THREEPCT[full_player_stats$TPA>=25]))/sd(full_player_stats$THREEPCT[full_player_stats$TPA>=25]))*100,0),
             'FTA %ile' = round(pnorm((FTAPPoss-mean(full_player_stats$FTAPPoss[full_player_stats$MPG>=20]))/sd(full_player_stats$FTAPPoss[full_player_stats$MPG>=20]))*100,0),
             'FT% %ile' = round(pnorm((FTPCT-mean(full_player_stats$FTPCT[full_player_stats$FTA>=25]))/sd(full_player_stats$FTPCT[full_player_stats$FTA>=25]))*100,0),
             'OReb %ile' = round(pnorm((ORPPoss-mean(full_player_stats$ORPPoss[full_player_stats$MPG>=20]))/sd(full_player_stats$ORPPoss[full_player_stats$MPG>=20]))*100,0),
             'DReb %ile' = round(pnorm((DRPPoss-mean(full_player_stats$DRPPoss[full_player_stats$MPG>=20]))/sd(full_player_stats$DRPPoss[full_player_stats$MPG>=20]))*100,0)) %>%
      filter(Team == team_name) %>%
      select(Player, GP, GS, MPG, PPPoss, 'Pts %ile', RPPoss, 'Reb %ile', APPoss, 'Ast %ile', STLPPoss, 'STL %ile', BLKPPoss, 'BLK %ile', TOVPPoss, 'TOV %ile', TWOMPPoss, TWOAPPoss, '2PA %ile', TWOPCT, '2% %ile', THREEMPPoss, THREEAPPoss, '3PA %ile', THREEPCT, '3% %ile', FTMPPoss, FTAPPoss, 'FTA %ile', FTPCT, 'FT% %ile', ORPPoss, 'OReb %ile', DRPPoss, 'DReb %ile') %>%
      arrange(desc(MPG))
    colnames(basic_data) <- c("Player", 'GP', 'GS', 'MINS', 'PTS', 'Pts %ile', 'REB', 'Reb %ile', 'AST', 'Ast %ile', 'STL', 'STL %ile', 'BLK', 'BLK %ile', 'TOV', 'TOV %ile', '2PM', '2PA', '2PA %ile', '2%', '2% %ile', '3PM', '3PA', '3PA %ile', '3%', '3% %ile', 'FTM', 'FTA', 'FTA %ile', 'FT%', 'FT% %ile', 'OReb', 'OReb %ile', 'DReb', 'DReb %ile')
    return(basic_data)
  }
  
}

get_shooting_stats <- function(team_name, season, type = "Per Game") {
  if (type == "Per Game") {
    basic_data <- full_player_stats %>%
      filter(Team %in% teams$Team) %>%
      mutate(Player = gsub(".", " ", Player, fixed=TRUE),
             'FGA %ile' = round(pnorm((FGAPG-mean(full_player_stats$FGAPG[full_player_stats$MPG>=20]))/sd(full_player_stats$FGAPG[full_player_stats$MPG>=20]))*100,0),
             'eFG% %ile' = round(pnorm((eFG.-mean(full_player_stats$eFG.[full_player_stats$FGA>=20]))/sd(full_player_stats$eFG.[full_player_stats$FGA>=20]))*100,0),
             'TS% %ile' = round(pnorm((TS.-mean(full_player_stats$TS.[full_player_stats$FGA>=20]))/sd(full_player_stats$TS.[full_player_stats$FGA>=20]))*100,0),
             'RimA %ile' = round(pnorm((RIMAPG-mean(full_player_stats$RIMAPG[full_player_stats$MPG>=20]))/sd(full_player_stats$RIMAPG[full_player_stats$MPG>=20]))*100,0),
             'Rim% %ile' = round(pnorm((RIM.-mean(full_player_stats$RIM.[full_player_stats$RIMA>=20]))/sd(full_player_stats$RIM.[full_player_stats$RIMA>=20]))*100,0),
             'MidA %ile' = round(pnorm((MIDAPG-mean(full_player_stats$MIDAPG[full_player_stats$MPG>=20]))/sd(full_player_stats$MIDAPG[full_player_stats$MPG>=20]))*100,0),
             'Mid% %ile' = round(pnorm((MID.-mean(full_player_stats$MID.[full_player_stats$MIDA>=20]))/sd(full_player_stats$MID.[full_player_stats$MIDA>=20]))*100,0),
             '3PA %ile' = round(pnorm((THREEA-mean(full_player_stats$THREEA[full_player_stats$MPG>=20]))/sd(full_player_stats$THREEA[full_player_stats$MPG>=20]))*100,0),
             '3% %ile' = round(pnorm((THREEPCT-mean(full_player_stats$THREEPCT[full_player_stats$TPA>=25]))/sd(full_player_stats$THREEPCT[full_player_stats$TPA>=25]))*100,0),
             'FTA %ile' = round(pnorm((FTAPG-mean(full_player_stats$FTAPG[full_player_stats$MPG>=20]))/sd(full_player_stats$FTAPG[full_player_stats$MPG>=20]))*100,0),
             'FT% %ile' = round(pnorm((FTPCT-mean(full_player_stats$FTPCT[full_player_stats$FTA>=25]))/sd(full_player_stats$FTPCT[full_player_stats$FTA>=25]))*100,0)) %>%
      filter(Team == team_name) %>%
      select(Player, PPG, FGAPG, 'FGA %ile', eFG., 'eFG% %ile', TS., 'TS% %ile', RIMMPG, RIMAPG, 'RimA %ile', RIM., 'Rim% %ile', MIDMPG, MIDAPG, 'MidA %ile', MID., 'Mid% %ile', THREEM, THREEA, '3PA %ile', THREEPCT, '3% %ile', FTMPG, FTAPG, 'FTA %ile', FTPCT, 'FT% %ile') %>%
      arrange(desc(FGAPG))
    colnames(basic_data) <- c('Player', 'Points', 'FGA', 'FGA %ile', 'eFG%', 'eFG% %ile', 'TS%', 'TS% %ile', 'Rim Makes', 'Rim Attemtps', 'Rim Attempt %ile', 'Rim%', 'Rim% %ile', 'Mid Makes', 'Mid Attempts', 'Mid Attempt %ile', 'Mid%', 'Mid% %ile', '3PM', '3PA', '3PA %ile', '3%', '3% %ile', 'FTM', 'FTA', 'FTA %ile', 'FT%', 'FT% %ile')
    return(basic_data)
  }
  else if (type == "Totals") {
    basic_data <- full_player_stats %>%
      filter(Team %in% teams$Team) %>%
      mutate(Player = gsub(".", " ", Player, fixed=TRUE),
             'FGA %ile' = round(pnorm((FGA-mean(full_player_stats$FGA[full_player_stats$MPG>=20]))/sd(full_player_stats$FGA[full_player_stats$MPG>=20]))*100,0),
             'eFG% %ile' = round(pnorm((eFG.-mean(full_player_stats$eFG.[full_player_stats$FGA>=20]))/sd(full_player_stats$eFG.[full_player_stats$FGA>=20]))*100,0),
             'TS% %ile' = round(pnorm((TS.-mean(full_player_stats$TS.[full_player_stats$FGA>=20]))/sd(full_player_stats$TS.[full_player_stats$FGA>=20]))*100,0),
             'RimA %ile' = round(pnorm((RIMA-mean(full_player_stats$RIMA[full_player_stats$MPG>=20]))/sd(full_player_stats$RIMA[full_player_stats$MPG>=20]))*100,0),
             'Rim% %ile' = round(pnorm((RIM.-mean(full_player_stats$RIM.[full_player_stats$RIMA>=20]))/sd(full_player_stats$RIM.[full_player_stats$RIMA>=20]))*100,0),
             'MidA %ile' = round(pnorm((MIDA-mean(full_player_stats$MIDA[full_player_stats$MPG>=20]))/sd(full_player_stats$MIDA[full_player_stats$MPG>=20]))*100,0),
             'Mid% %ile' = round(pnorm((MID.-mean(full_player_stats$MID.[full_player_stats$MIDA>=20]))/sd(full_player_stats$MID.[full_player_stats$MIDA>=20]))*100,0),
             '3PA %ile' = round(pnorm((TPA-mean(full_player_stats$TPA[full_player_stats$MPG>=20]))/sd(full_player_stats$TPA[full_player_stats$MPG>=20]))*100,0),
             '3% %ile' = round(pnorm((THREEPCT-mean(full_player_stats$THREEPCT[full_player_stats$TPA>=25]))/sd(full_player_stats$THREEPCT[full_player_stats$TPA>=25]))*100,0),
             'FTA %ile' = round(pnorm((FTA-mean(full_player_stats$FTA[full_player_stats$MPG>=20]))/sd(full_player_stats$FTA[full_player_stats$MPG>=20]))*100,0),
             'FT% %ile' = round(pnorm((FTPCT-mean(full_player_stats$FTPCT[full_player_stats$FTA>=25]))/sd(full_player_stats$FTPCT[full_player_stats$FTA>=25]))*100,0)) %>%
      filter(Team == team_name) %>%
      select(Player, PTS, FGA, 'FGA %ile', eFG., 'eFG% %ile', TS., 'TS% %ile', RIMM, RIMA, 'RimA %ile', RIM., 'Rim% %ile', MIDM, MIDA, 'MidA %ile', MID., 'Mid% %ile', TPM, TPA, '3PA %ile', THREEPCT, '3% %ile', FTM, FTA, 'FTA %ile', FTPCT, 'FT% %ile') %>%
      arrange(desc(FGA))
    colnames(basic_data) <- c('Player', 'Points', 'FGA', 'FGA %ile', 'eFG%', 'eFG% %ile', 'TS%', 'TS% %ile', 'Rim Makes', 'Rim Attemtps', 'Rim Attempt %ile', 'Rim%', 'Rim% %ile', 'Mid Makes', 'Mid Attempts', 'Mid Attempt %ile', 'Mid%', 'Mid% %ile', '3PM', '3PA', '3PA %ile', '3%', '3% %ile', 'FTM', 'FTA', 'FTA %ile', 'FT%', 'FT% %ile')
    return(basic_data)
  }
  else if (type == "Per 40 Minutes") {
    basic_data <- full_player_stats %>%
      filter(Team %in% teams$Team) %>%
      mutate(Player = gsub(".", " ", Player, fixed=TRUE),
             'FGA %ile' = round(pnorm((FGAPM-mean(full_player_stats$FGAPM[full_player_stats$MPG>=20]))/sd(full_player_stats$FGAPM[full_player_stats$MPG>=20]))*100,0),
             'eFG% %ile' = round(pnorm((eFG.-mean(full_player_stats$eFG.[full_player_stats$FGA>=20]))/sd(full_player_stats$eFG.[full_player_stats$FGA>=20]))*100,0),
             'TS% %ile' = round(pnorm((TS.-mean(full_player_stats$TS.[full_player_stats$FGA>=20]))/sd(full_player_stats$TS.[full_player_stats$FGA>=20]))*100,0),
             'RimA %ile' = round(pnorm((RIMAPM-mean(full_player_stats$RIMAPM[full_player_stats$MPG>=20]))/sd(full_player_stats$RIMAPM[full_player_stats$MPG>=20]))*100,0),
             'Rim% %ile' = round(pnorm((RIM.-mean(full_player_stats$RIM.[full_player_stats$RIMA>=20]))/sd(full_player_stats$RIM.[full_player_stats$RIMA>=20]))*100,0),
             'MidA %ile' = round(pnorm((MIDAPM-mean(full_player_stats$MIDAPM[full_player_stats$MPG>=20]))/sd(full_player_stats$MIDAPM[full_player_stats$MPG>=20]))*100,0),
             'Mid% %ile' = round(pnorm((MID.-mean(full_player_stats$MID.[full_player_stats$MIDA>=20]))/sd(full_player_stats$MID.[full_player_stats$MIDA>=20]))*100,0),
             '3PA %ile' = round(pnorm((THREEAPM-mean(full_player_stats$THREEAPM[full_player_stats$MPG>=20]))/sd(full_player_stats$THREEAPM[full_player_stats$MPG>=20]))*100,0),
             '3% %ile' = round(pnorm((THREEPCT-mean(full_player_stats$THREEPCT[full_player_stats$TPA>=25]))/sd(full_player_stats$THREEPCT[full_player_stats$TPA>=25]))*100,0),
             'FTA %ile' = round(pnorm((FTAPMin-mean(full_player_stats$FTAPMin[full_player_stats$MPG>=20]))/sd(full_player_stats$FTAPMin[full_player_stats$MPG>=20]))*100,0),
             'FT% %ile' = round(pnorm((FTPCT-mean(full_player_stats$FTPCT[full_player_stats$FTA>=25]))/sd(full_player_stats$FTPCT[full_player_stats$FTA>=25]))*100,0)) %>%
      filter(Team == team_name) %>%
      select(Player, PPMin, FGAPM, 'FGA %ile', eFG., 'eFG% %ile', TS., 'TS% %ile', RIMMPM, RIMAPM, 'RimA %ile', RIM., 'Rim% %ile', MIDMPM, MIDAPM, 'MidA %ile', MID., 'Mid% %ile', THREEMPM, THREEAPM, '3PA %ile', THREEPCT, '3% %ile', FTMPMin, FTAPMin, 'FTA %ile', FTPCT, 'FT% %ile') %>%
      arrange(desc(FGAPM))
    colnames(basic_data) <- c('Player', 'Points', 'FGA', 'FGA %ile', 'eFG%', 'eFG% %ile', 'TS%', 'TS% %ile', 'Rim Makes', 'Rim Attemtps', 'Rim Attempt %ile', 'Rim%', 'Rim% %ile', 'Mid Makes', 'Mid Attempts', 'Mid Attempt %ile', 'Mid%', 'Mid% %ile', '3PM', '3PA', '3PA %ile', '3%', '3% %ile', 'FTM', 'FTA', 'FTA %ile', 'FT%', 'FT% %ile')
    return(basic_data)
  }
  else if (type == "Per 70 Possessions") {
    basic_data <- full_player_stats %>%
      filter(Team %in% teams$Team) %>%
      mutate(Player = gsub(".", " ", Player, fixed=TRUE),
             'FGA %ile' = round(pnorm((FGAPP-mean(full_player_stats$FGAPP[full_player_stats$MPG>=20]))/sd(full_player_stats$FGAPP[full_player_stats$MPG>=20]))*100,0),
             'eFG% %ile' = round(pnorm((eFG.-mean(full_player_stats$eFG.[full_player_stats$FGA>=20]))/sd(full_player_stats$eFG.[full_player_stats$FGA>=20]))*100,0),
             'TS% %ile' = round(pnorm((TS.-mean(full_player_stats$TS.[full_player_stats$FGA>=20]))/sd(full_player_stats$TS.[full_player_stats$FGA>=20]))*100,0),
             'RimA %ile' = round(pnorm((RIMAPP-mean(full_player_stats$RIMAPP[full_player_stats$MPG>=20]))/sd(full_player_stats$RIMAPP[full_player_stats$MPG>=20]))*100,0),
             'Rim% %ile' = round(pnorm((RIM.-mean(full_player_stats$RIM.[full_player_stats$RIMA>=20]))/sd(full_player_stats$RIM.[full_player_stats$RIMA>=20]))*100,0),
             'MidA %ile' = round(pnorm((MIDAPP-mean(full_player_stats$MIDAPP[full_player_stats$MPG>=20]))/sd(full_player_stats$MIDAPP[full_player_stats$MPG>=20]))*100,0),
             'Mid% %ile' = round(pnorm((MID.-mean(full_player_stats$MID.[full_player_stats$MIDA>=20]))/sd(full_player_stats$MID.[full_player_stats$MIDA>=20]))*100,0),
             '3PA %ile' = round(pnorm((THREEAPPoss-mean(full_player_stats$THREEAPPoss[full_player_stats$MPG>=20]))/sd(full_player_stats$THREEAPPoss[full_player_stats$MPG>=20]))*100,0),
             '3% %ile' = round(pnorm((THREEPCT-mean(full_player_stats$THREEPCT[full_player_stats$TPA>=25]))/sd(full_player_stats$THREEPCT[full_player_stats$TPA>=25]))*100,0),
             'FTA %ile' = round(pnorm((FTAPPoss-mean(full_player_stats$FTAPPoss[full_player_stats$MPG>=20]))/sd(full_player_stats$FTAPPoss[full_player_stats$MPG>=20]))*100,0),
             'FT% %ile' = round(pnorm((FTPCT-mean(full_player_stats$FTPCT[full_player_stats$FTA>=25]))/sd(full_player_stats$FTPCT[full_player_stats$FTA>=25]))*100,0)) %>%
      filter(Team == team_name) %>%
      select(Player, PPPoss, FGAPP, 'FGA %ile', eFG., 'eFG% %ile', TS., 'TS% %ile', RIMMPP, RIMAPP, 'RimA %ile', RIM., 'Rim% %ile', MIDMPP, MIDAPP, 'MidA %ile', MID., 'Mid% %ile', THREEMPPoss, THREEAPPoss, '3PA %ile', THREEPCT, '3% %ile', FTMPPoss, FTAPPoss, 'FTA %ile', FTPCT, 'FT% %ile') %>%
      arrange(desc(FGAPP))
    colnames(basic_data) <- c('Player', 'Points', 'FGA', 'FGA %ile', 'eFG%', 'eFG% %ile', 'TS%', 'TS% %ile', 'Rim Makes', 'Rim Attemtps', 'Rim Attempt %ile', 'Rim%', 'Rim% %ile', 'Mid Makes', 'Mid Attempts', 'Mid Attempt %ile', 'Mid%', 'Mid% %ile', '3PM', '3PA', '3PA %ile', '3%', '3% %ile', 'FTM', 'FTA', 'FTA %ile', 'FT%', 'FT% %ile')
    return(basic_data)
  }
  
}

get_scoring_stats <- function(team_name, season, type = "Per Game") {
  if (type == "Per Game") {
    basic_data <- full_player_stats %>%
      filter(Team %in% teams$Team) %>%
      mutate(Player = gsub(".", " ", Player, fixed=TRUE),
             'Pts %ile' = round(pnorm((PPG-mean(full_player_stats$PPG[full_player_stats$MPG>=20]))/sd(full_player_stats$PPG[full_player_stats$MPG>=20]))*100,0),
             'FGA %ile' = round(pnorm((FGACREATEDPG-mean(full_player_stats$FGACREATEDPG[full_player_stats$MPG>=20]))/sd(full_player_stats$FGACREATEDPG[full_player_stats$MPG>=20]))*100,0),
             'eFG% %ile' = round(pnorm((eFG._unast-mean(full_player_stats$eFG._unast[full_player_stats$FGA>=20]))/sd(full_player_stats$eFG._unast[full_player_stats$FGA>=20]))*100,0),
             'RimA %ile' = round(pnorm((RIMACREATEDPG-mean(full_player_stats$RIMACREATEDPG[full_player_stats$MPG>=20]))/sd(full_player_stats$RIMACREATEDPG[full_player_stats$MPG>=20]))*100,0),
             'Rim% %ile' = round(pnorm((RIM._unast-mean(full_player_stats$RIM._unast[full_player_stats$RIMA>=20]))/sd(full_player_stats$RIM._unast[full_player_stats$RIMA>=20]))*100,0),
             'MidA %ile' = round(pnorm((MIDACREATEDPG-mean(full_player_stats$MIDACREATEDPG[full_player_stats$MPG>=20]))/sd(full_player_stats$MIDACREATEDPG[full_player_stats$MPG>=20]))*100,0),
             'Mid% %ile' = round(pnorm((MID._unast-mean(full_player_stats$MID._unast[full_player_stats$MIDA>=20]))/sd(full_player_stats$MID._unast[full_player_stats$MIDA>=20]))*100,0),
             '3PA %ile' = round(pnorm((THREEACREATEDPG-mean(full_player_stats$THREEACREATEDPG[full_player_stats$MPG>=20]))/sd(full_player_stats$THREEACREATEDPG[full_player_stats$MPG>=20]))*100,0),
             '3% %ile' = round(pnorm((TP._unast-mean(full_player_stats$TP._unast[full_player_stats$TPA>=25]))/sd(full_player_stats$TP._unast[full_player_stats$TPA>=25]))*100,0),
             'PTS Created %ile' = round(pnorm((PTSCREATEDPG-mean(full_player_stats$PTSCREATEDPG[full_player_stats$MPG>=20]))/sd(full_player_stats$PTSCREATEDPG[full_player_stats$MPG>=20]))*100,0),
             'PTS Assisted %ile' = round(pnorm((PTSASSISTEDPG-mean(full_player_stats$PTSASSISTEDPG[full_player_stats$MPG>=20]))/sd(full_player_stats$PTSASSISTEDPG[full_player_stats$MPG>=20]))*100,0),
             'Creation Load %ile' = round(pnorm((CREATIONPROPORTION-mean(full_player_stats$CREATIONPROPORTION[full_player_stats$MPG>=20]))/sd(full_player_stats$CREATIONPROPORTION[full_player_stats$MPG>=20]))*100,0),
      ) %>%
      filter(Team == team_name) %>%
      select(Player, PPG, 'Pts %ile', PTSCREATEDPG, 'PTS Created %ile', PTSASSISTEDPG, 'PTS Assisted %ile', CREATIONPROPORTION, 'Creation Load %ile', FGACREATEDPG, 'FGA %ile', eFG._unast, 'eFG% %ile', RIMMCREATEDPG, RIMACREATEDPG, 'RimA %ile', RIM._unast, 'Rim% %ile', MIDMCREATEDPG, MIDACREATEDPG, 'MidA %ile', MID._unast, 'Mid% %ile', THREEMCREATEDPG, THREEACREATEDPG, '3PA %ile', TP._unast, '3% %ile') %>%
      arrange(desc(FGACREATEDPG))
    colnames(basic_data) <- c('Player', 'Pts', 'Pts %ile', 'Created Pts', 'Created Pts %ile', 'Assisted Pts', 'Assisted Pts %ile', 'Creation Load', 'Creation Load %ile', 'Created FGA', 'FGA %ile', 'Created eFG%', 'Created eFG% %ile', 'Created Rim Makes', 'Created Rim Attemtps', 'Created Rim Attempt %ile', 'Created Rim%', 'Created Rim% %ile', 'Created Mid Makes', 'Created Mid Attempts', 'Created Mid Attempt %ile', 'Created Mid%', 'Created Mid% %ile', 'Created 3PM', 'Created 3PA', 'Created 3PA %ile', 'Created 3%', 'Created 3% %ile')
    return(basic_data)
  }
  else if (type == "Totals") {
    basic_data <- full_player_stats %>%
      filter(Team %in% teams$Team) %>%
      mutate(Player = gsub(".", " ", Player, fixed=TRUE),
             'Pts %ile' = round(pnorm((PTS-mean(full_player_stats$PTS[full_player_stats$MPG>=20]))/sd(full_player_stats$PTS[full_player_stats$MPG>=20]))*100,0),
             'FGA %ile' = round(pnorm((FGA_unast-mean(full_player_stats$FGA_unast[full_player_stats$MPG>=20]))/sd(full_player_stats$FGA_unast[full_player_stats$MPG>=20]))*100,0),
             'eFG% %ile' = round(pnorm((eFG._unast-mean(full_player_stats$eFG._unast[full_player_stats$FGA>=20]))/sd(full_player_stats$eFG._unast[full_player_stats$FGA>=20]))*100,0),
             'RimA %ile' = round(pnorm((RIMA_unast-mean(full_player_stats$RIMA_unast[full_player_stats$MPG>=20]))/sd(full_player_stats$RIMA_unast[full_player_stats$MPG>=20]))*100,0),
             'Rim% %ile' = round(pnorm((RIM._unast-mean(full_player_stats$RIM._unast[full_player_stats$RIMA>=20]))/sd(full_player_stats$RIM._unast[full_player_stats$RIMA>=20]))*100,0),
             'MidA %ile' = round(pnorm((MIDA_unast-mean(full_player_stats$MIDA_unast[full_player_stats$MPG>=20]))/sd(full_player_stats$MIDA_unast[full_player_stats$MPG>=20]))*100,0),
             'Mid% %ile' = round(pnorm((MID._unast-mean(full_player_stats$MID._unast[full_player_stats$MIDA>=20]))/sd(full_player_stats$MID._unast[full_player_stats$MIDA>=20]))*100,0),
             '3PA %ile' = round(pnorm((TPA_unast-mean(full_player_stats$TPA_unast[full_player_stats$MPG>=20]))/sd(full_player_stats$TPA_unast[full_player_stats$MPG>=20]))*100,0),
             '3% %ile' = round(pnorm((TP._unast-mean(full_player_stats$TP._unast[full_player_stats$TPA>=25]))/sd(full_player_stats$TP._unast[full_player_stats$TPA>=25]))*100,0),
             'PTS Created %ile' = round(pnorm((PTS_unast-mean(full_player_stats$PTS_unast[full_player_stats$MPG>=20]))/sd(full_player_stats$PTS_unast[full_player_stats$MPG>=20]))*100,0),
             'PTS Assisted %ile' = round(pnorm((PTS_ast-mean(full_player_stats$PTS_ast[full_player_stats$MPG>=20]))/sd(full_player_stats$PTS_ast[full_player_stats$MPG>=20]))*100,0),
             'Creation Load %ile' = round(pnorm((CREATIONPROPORTION-mean(full_player_stats$CREATIONPROPORTION[full_player_stats$MPG>=20]))/sd(full_player_stats$CREATIONPROPORTION[full_player_stats$MPG>=20]))*100,0),
             
      ) %>%
      filter(Team == team_name) %>%
      select(Player, PTS, 'Pts %ile', PTS_unast, 'PTS Created %ile', PTS_ast, 'PTS Assisted %ile', CREATIONPROPORTION, 'Creation Load %ile', FGA_unast, 'FGA %ile', eFG._unast, 'eFG% %ile', RIMM_ast, RIMA_unast, 'RimA %ile', RIM._unast, 'Rim% %ile', MIDM_unast, MIDA_unast, 'MidA %ile', MID._unast, 'Mid% %ile', TPM_unast, TPA_unast, '3PA %ile', TP._unast, '3% %ile') %>%
      arrange(desc(FGA_unast))
    colnames(basic_data) <- c('Player', 'Pts', 'Pts %ile', 'Created Pts', 'Created Pts %ile', 'Assisted Pts', 'Assisted Pts %ile', 'Creation Load', 'Creation Load %ile', 'Created FGA', 'FGA %ile', 'Created eFG%', 'Created eFG% %ile', 'Created Rim Makes', 'Created Rim Attemtps', 'Created Rim Attempt %ile', 'Created Rim%', 'Created Rim% %ile', 'Created Mid Makes', 'Created Mid Attempts', 'Created Mid Attempt %ile', 'Created Mid%', 'Created Mid% %ile', 'Created 3PM', 'Created 3PA', 'Created 3PA %ile', 'Created 3%', 'Created 3% %ile')
    return(basic_data)
  }
  else if (type == "Per 40 Minutes") {
    basic_data <- full_player_stats %>%
      filter(Team %in% teams$Team) %>%
      mutate(Player = gsub(".", " ", Player, fixed=TRUE),
             'Pts %ile' = round(pnorm((PPMin-mean(full_player_stats$PPMin[full_player_stats$MPG>=20]))/sd(full_player_stats$PPMin[full_player_stats$MPG>=20]))*100,0),
             'FGA %ile' = round(pnorm((FGACREATEDPM-mean(full_player_stats$FGACREATEDPM[full_player_stats$MPG>=20]))/sd(full_player_stats$FGACREATEDPM[full_player_stats$MPG>=20]))*100,0),
             'eFG% %ile' = round(pnorm((eFG._unast-mean(full_player_stats$eFG._unast[full_player_stats$FGA>=20]))/sd(full_player_stats$eFG._unast[full_player_stats$FGA>=20]))*100,0),
             'RimA %ile' = round(pnorm((RIMACREATEDPM-mean(full_player_stats$RIMACREATEDPM[full_player_stats$MPG>=20]))/sd(full_player_stats$RIMACREATEDPM[full_player_stats$MPG>=20]))*100,0),
             'Rim% %ile' = round(pnorm((RIM._unast-mean(full_player_stats$RIM._unast[full_player_stats$RIMA>=20]))/sd(full_player_stats$RIM._unast[full_player_stats$RIMA>=20]))*100,0),
             'MidA %ile' = round(pnorm((MIDACREATEDPM-mean(full_player_stats$MIDACREATEDPM[full_player_stats$MPG>=20]))/sd(full_player_stats$MIDACREATEDPM[full_player_stats$MPG>=20]))*100,0),
             'Mid% %ile' = round(pnorm((MID._unast-mean(full_player_stats$MID._unast[full_player_stats$MIDA>=20]))/sd(full_player_stats$MID._unast[full_player_stats$MIDA>=20]))*100,0),
             '3PA %ile' = round(pnorm((THREEACREATEDPM-mean(full_player_stats$THREEACREATEDPM[full_player_stats$MPG>=20]))/sd(full_player_stats$THREEACREATEDPM[full_player_stats$MPG>=20]))*100,0),
             '3% %ile' = round(pnorm((TP._unast-mean(full_player_stats$TP._unast[full_player_stats$TPA>=25]))/sd(full_player_stats$TP._unast[full_player_stats$TPA>=25]))*100,0),
             'PTS Created %ile' = round(pnorm((PTSCREATEDPM-mean(full_player_stats$PTSCREATEDPM[full_player_stats$MPG>=20]))/sd(full_player_stats$PTSCREATEDPM[full_player_stats$MPG>=20]))*100,0),
             'PTS Assisted %ile' = round(pnorm((PTSASSISTEDPM-mean(full_player_stats$PTSASSISTEDPM[full_player_stats$MPG>=20]))/sd(full_player_stats$PTSASSISTEDPM[full_player_stats$MPG>=20]))*100,0),
             'Creation Load %ile' = round(pnorm((CREATIONPROPORTION-mean(full_player_stats$CREATIONPROPORTION[full_player_stats$MPG>=20]))/sd(full_player_stats$CREATIONPROPORTION[full_player_stats$MPG>=20]))*100,0),
      ) %>%
      filter(Team == team_name) %>%
      select(Player, PPMin, 'Pts %ile', PTSCREATEDPM, 'PTS Created %ile', PTSASSISTEDPM, 'PTS Assisted %ile', CREATIONPROPORTION, 'Creation Load %ile', FGACREATEDPM, 'FGA %ile', eFG._unast, 'eFG% %ile', RIMMCREATEDPM, RIMACREATEDPM, 'RimA %ile', RIM._unast, 'Rim% %ile', MIDMCREATEDPM, MIDACREATEDPM, 'MidA %ile', MID._unast, 'Mid% %ile', THREEMCREATEDPM, THREEACREATEDPM, '3PA %ile', TP._unast, '3% %ile') %>%
      arrange(desc(FGACREATEDPM))
    colnames(basic_data) <- c('Player', 'Pts', 'Pts %ile', 'Created Pts', 'Created Pts %ile', 'Assisted Pts', 'Assisted Pts %ile', 'Creation Load', 'Creation Load %ile', 'Created FGA', 'FGA %ile', 'Created eFG%', 'Created eFG% %ile', 'Created Rim Makes', 'Created Rim Attemtps', 'Created Rim Attempt %ile', 'Created Rim%', 'Created Rim% %ile', 'Created Mid Makes', 'Created Mid Attempts', 'Created Mid Attempt %ile', 'Created Mid%', 'Created Mid% %ile', 'Created 3PM', 'Created 3PA', 'Created 3PA %ile', 'Created 3%', 'Created 3% %ile')
    return(basic_data)
  }
  else if (type == "Per 70 Possessions") {
    basic_data <- full_player_stats %>%
      filter(Team %in% teams$Team) %>%
      mutate(Player = gsub(".", " ", Player, fixed=TRUE),
             'Pts %ile' = round(pnorm((PPPoss-mean(full_player_stats$PPPoss[full_player_stats$MPG>=20]))/sd(full_player_stats$PPPoss[full_player_stats$MPG>=20]))*100,0),
             'FGA %ile' = round(pnorm((FGACREATEDPP-mean(full_player_stats$FGACREATEDPP[full_player_stats$MPG>=20]))/sd(full_player_stats$FGACREATEDPP[full_player_stats$MPG>=20]))*100,0),
             'eFG% %ile' = round(pnorm((eFG._unast-mean(full_player_stats$eFG._unast[full_player_stats$FGA>=20]))/sd(full_player_stats$eFG._unast[full_player_stats$FGA>=20]))*100,0),
             'RimA %ile' = round(pnorm((RIMACREATEDPP-mean(full_player_stats$RIMACREATEDPP[full_player_stats$MPG>=20]))/sd(full_player_stats$RIMACREATEDPP[full_player_stats$MPG>=20]))*100,0),
             'Rim% %ile' = round(pnorm((RIM._unast-mean(full_player_stats$RIM._unast[full_player_stats$RIMA>=20]))/sd(full_player_stats$RIM._unast[full_player_stats$RIMA>=20]))*100,0),
             'MidA %ile' = round(pnorm((MIDACREATEDPP-mean(full_player_stats$MIDACREATEDPP[full_player_stats$MPG>=20]))/sd(full_player_stats$MIDACREATEDPP[full_player_stats$MPG>=20]))*100,0),
             'Mid% %ile' = round(pnorm((MID._unast-mean(full_player_stats$MID._unast[full_player_stats$MIDA>=20]))/sd(full_player_stats$MID._unast[full_player_stats$MIDA>=20]))*100,0),
             '3PA %ile' = round(pnorm((THREEACREATEDPP-mean(full_player_stats$THREEACREATEDPP[full_player_stats$MPG>=20]))/sd(full_player_stats$THREEACREATEDPP[full_player_stats$MPG>=20]))*100,0),
             '3% %ile' = round(pnorm((TP._unast-mean(full_player_stats$TP._unast[full_player_stats$TPA>=25]))/sd(full_player_stats$TP._unast[full_player_stats$TPA>=25]))*100,0),
             'PTS Created %ile' = round(pnorm((PTSCREATEDPP-mean(full_player_stats$PTSCREATEDPP[full_player_stats$MPG>=20]))/sd(full_player_stats$PTSCREATEDPP[full_player_stats$MPG>=20]))*100,0),
             'PTS Assisted %ile' = round(pnorm((PTSASSISTEDPP-mean(full_player_stats$PTSASSISTEDPP[full_player_stats$MPG>=20]))/sd(full_player_stats$PTSASSISTEDPP[full_player_stats$MPG>=20]))*100,0),
             'Creation Load %ile' = round(pnorm((CREATIONPROPORTION-mean(full_player_stats$CREATIONPROPORTION[full_player_stats$MPG>=20]))/sd(full_player_stats$CREATIONPROPORTION[full_player_stats$MPG>=20]))*100,0),
      ) %>%
      filter(Team == team_name) %>%
      select(Player, PPPoss, 'Pts %ile', PTSCREATEDPP, 'PTS Created %ile', PTSASSISTEDPP, 'PTS Assisted %ile', CREATIONPROPORTION, 'Creation Load %ile', FGACREATEDPP, 'FGA %ile', eFG._unast, 'eFG% %ile', RIMMCREATEDPP, RIMACREATEDPP, 'RimA %ile', RIM._unast, 'Rim% %ile', MIDMCREATEDPP, MIDACREATEDPP, 'MidA %ile', MID._unast, 'Mid% %ile', THREEMCREATEDPP, THREEACREATEDPP, '3PA %ile', TP._unast, '3% %ile') %>%
      arrange(desc(FGACREATEDPP))
    colnames(basic_data) <- c('Player', 'Pts', 'Pts %ile', 'Created Pts', 'Created Pts %ile', 'Assisted Pts', 'Assisted Pts %ile', 'Creation Load', 'Creation Load %ile', 'Created FGA', 'FGA %ile', 'Created eFG%', 'Created eFG% %ile', 'Created Rim Makes', 'Created Rim Attemtps', 'Created Rim Attempt %ile', 'Created Rim%', 'Created Rim% %ile', 'Created Mid Makes', 'Created Mid Attempts', 'Created Mid Attempt %ile', 'Created Mid%', 'Created Mid% %ile', 'Created 3PM', 'Created 3PA', 'Created 3PA %ile', 'Created 3%', 'Created 3% %ile')
    return(basic_data)
  }
}

get_advanced_stats <- function(team_name, season, type = "Per Game") {
  if (type == "Per Game") {
    basic_data <- full_player_stats %>%
      filter(Team %in% teams$Team) %>%
      mutate(Player = gsub(".", " ", Player, fixed=TRUE),
             'Pts %ile' = round(pnorm((PTSADDEDPG-mean(full_player_stats$PTSADDEDPG[full_player_stats$MPG>=20]))/sd(full_player_stats$PTSADDEDPG[full_player_stats$MPG>=20]))*100,0),
             'SQ %ile' = round(pnorm((SHOTQUALITY-mean(full_player_stats$SHOTQUALITY[full_player_stats$FGA>=20]))/sd(full_player_stats$SHOTQUALITY[full_player_stats$FGA>=20]))*100,0),
             'Shot-Making %ile' = round(pnorm((SHOTMAKING-mean(full_player_stats$SHOTMAKING[full_player_stats$FGA>=20]))/sd(full_player_stats$SHOTMAKING[full_player_stats$FGA>=20]))*100,0),
             'Creation Volume %ile' = round(pnorm((CREATIONVOLUMEPG-mean(full_player_stats$CREATIONVOLUMEPG[full_player_stats$FGA>=20]))/sd(full_player_stats$CREATIONVOLUMEPG[full_player_stats$FGA>=20]))*100,0),
             '% of Scoring in Transition %ile' = round(pnorm((TRANSITIONPROP-mean(full_player_stats$TRANSITIONPROP[full_player_stats$FGA>=20]))/sd(full_player_stats$TRANSITIONPROP[full_player_stats$FGA>=20]))*100,0),
             '% of Scoring in Halfcourt %ile' = round(pnorm((HALFCOURTPROP-mean(full_player_stats$HALFCOURTPROP[full_player_stats$FGA>=20]))/sd(full_player_stats$HALFCOURTPROP[full_player_stats$FGA>=20]))*100,0),
      ) %>%
      filter(Team == team_name) %>%
      select(Player, PTSADDEDPG, 'Pts %ile', SHOTQUALITY, 'SQ %ile', SHOTMAKING, 'Shot-Making %ile', CREATIONVOLUMEPG, 'Creation Volume %ile', TRANSITIONPROP, '% of Scoring in Transition %ile', HALFCOURTPROP, '% of Scoring in Halfcourt %ile') %>%
      arrange(desc(PTSADDEDPG))
    colnames(basic_data) <- c('Player', 'Points Added', 'Pts %ile', 'Shot Quality', 'SQ %ile', 'Shot-Making', 'Shot-Making %ile', 'Creation Volume', 'Creation Volume %ile', '% of Scoring in Transition', '% of Scoring in Transition %ile', '% of Scoring in Halfcourt', '% of Scoring in Halfcourt %ile')
    return(basic_data)
  }
  else if (type == "Totals") {
    basic_data <- full_player_stats %>%
      filter(Team %in% teams$Team) %>%
      mutate(Player = gsub(".", " ", Player, fixed=TRUE),
             'Pts %ile' = round(pnorm((PTSADDED-mean(full_player_stats$PTSADDED[full_player_stats$MPG>=20]))/sd(full_player_stats$PTSADDED[full_player_stats$MPG>=20]))*100,0),
             'SQ %ile' = round(pnorm((SHOTQUALITY-mean(full_player_stats$SHOTQUALITY[full_player_stats$FGA>=20]))/sd(full_player_stats$SHOTQUALITY[full_player_stats$FGA>=20]))*100,0),
             'Shot-Making %ile' = round(pnorm((SHOTMAKING-mean(full_player_stats$SHOTMAKING[full_player_stats$FGA>=20]))/sd(full_player_stats$SHOTMAKING[full_player_stats$FGA>=20]))*100,0),
             'Creation Volume %ile' = round(pnorm((CREATIONVOLUME-mean(full_player_stats$CREATIONVOLUME[full_player_stats$FGA>=20]))/sd(full_player_stats$CREATIONVOLUME[full_player_stats$FGA>=20]))*100,0),
             '% of Scoring in Transition %ile' = round(pnorm((TRANSITIONPROP-mean(full_player_stats$TRANSITIONPROP[full_player_stats$FGA>=20]))/sd(full_player_stats$TRANSITIONPROP[full_player_stats$FGA>=20]))*100,0),
             '% of Scoring in Halfcourt %ile' = round(pnorm((HALFCOURTPROP-mean(full_player_stats$HALFCOURTPROP[full_player_stats$FGA>=20]))/sd(full_player_stats$HALFCOURTPROP[full_player_stats$FGA>=20]))*100,0),
      ) %>%
      filter(Team == team_name) %>%
      select(Player, PTSADDED, 'Pts %ile', SHOTQUALITY, 'SQ %ile', SHOTMAKING, 'Shot-Making %ile', CREATIONVOLUME, 'Creation Volume %ile', TRANSITIONPROP, '% of Scoring in Transition %ile', HALFCOURTPROP, '% of Scoring in Halfcourt %ile') %>%
      arrange(desc(PTSADDED))
    colnames(basic_data) <- c('Player', 'Points Added', 'Pts %ile', 'Shot Quality', 'SQ %ile', 'Shot-Making', 'Shot-Making %ile', 'Creation Volume', 'Creation Volume %ile', '% of Scoring in Transition', '% of Scoring in Transition %ile', '% of Scoring in Halfcourt', '% of Scoring in Halfcourt %ile')
    return(basic_data)
  }
  else if (type == "Per 40 Minutes") {
    basic_data <- full_player_stats %>%
      filter(Team %in% teams$Team) %>%
      mutate(Player = gsub(".", " ", Player, fixed=TRUE),
             'Pts %ile' = round(pnorm((PTSADDEDPM-mean(full_player_stats$PTSADDEDPM[full_player_stats$MPG>=20]))/sd(full_player_stats$PTSADDEDPM[full_player_stats$MPG>=20]))*100,0),
             'SQ %ile' = round(pnorm((SHOTQUALITY-mean(full_player_stats$SHOTQUALITY[full_player_stats$FGA>=20]))/sd(full_player_stats$SHOTQUALITY[full_player_stats$FGA>=20]))*100,0),
             'Shot-Making %ile' = round(pnorm((SHOTMAKING-mean(full_player_stats$SHOTMAKING[full_player_stats$FGA>=20]))/sd(full_player_stats$SHOTMAKING[full_player_stats$FGA>=20]))*100,0),
             'Creation Volume %ile' = round(pnorm((CREATIONVOLUMEPM-mean(full_player_stats$CREATIONVOLUMEPM[full_player_stats$FGA>=20]))/sd(full_player_stats$CREATIONVOLUMEPM[full_player_stats$FGA>=20]))*100,0),
             '% of Scoring in Transition %ile' = round(pnorm((TRANSITIONPROP-mean(full_player_stats$TRANSITIONPROP[full_player_stats$FGA>=20]))/sd(full_player_stats$TRANSITIONPROP[full_player_stats$FGA>=20]))*100,0),
             '% of Scoring in Halfcourt %ile' = round(pnorm((HALFCOURTPROP-mean(full_player_stats$HALFCOURTPROP[full_player_stats$FGA>=20]))/sd(full_player_stats$HALFCOURTPROP[full_player_stats$FGA>=20]))*100,0),
      ) %>%
      filter(Team == team_name) %>%
      select(Player, PTSADDEDPM, 'Pts %ile', SHOTQUALITY, 'SQ %ile', SHOTMAKING, 'Shot-Making %ile', CREATIONVOLUMEPM, 'Creation Volume %ile', TRANSITIONPROP, '% of Scoring in Transition %ile', HALFCOURTPROP, '% of Scoring in Halfcourt %ile') %>%
      arrange(desc(PTSADDEDPM))
    colnames(basic_data) <- c('Player', 'Points Added', 'Pts %ile', 'Shot Quality', 'SQ %ile', 'Shot-Making', 'Shot-Making %ile', 'Creation Volume', 'Creation Volume %ile', '% of Scoring in Transition', '% of Scoring in Transition %ile', '% of Scoring in Halfcourt', '% of Scoring in Halfcourt %ile')
    return(basic_data)
  }
  else if (type == "Per 70 Possessions") {
    basic_data <- full_player_stats %>%
      filter(Team %in% teams$Team) %>%
      mutate(Player = gsub(".", " ", Player, fixed=TRUE),
             'Pts %ile' = round(pnorm((PTSADDEDPP-mean(full_player_stats$PTSADDEDPP[full_player_stats$MPG>=20]))/sd(full_player_stats$PTSADDEDPP[full_player_stats$MPG>=20]))*100,0),
             'SQ %ile' = round(pnorm((SHOTQUALITY-mean(full_player_stats$SHOTQUALITY[full_player_stats$FGA>=20]))/sd(full_player_stats$SHOTQUALITY[full_player_stats$FGA>=20]))*100,0),
             'Shot-Making %ile' = round(pnorm((SHOTMAKING-mean(full_player_stats$SHOTMAKING[full_player_stats$FGA>=20]))/sd(full_player_stats$SHOTMAKING[full_player_stats$FGA>=20]))*100,0),
             'Creation Volume %ile' = round(pnorm((CREATIONVOLUMEPP-mean(full_player_stats$CREATIONVOLUMEPP[full_player_stats$FGA>=20]))/sd(full_player_stats$CREATIONVOLUMEPP[full_player_stats$FGA>=20]))*100,0),
             '% of Scoring in Transition %ile' = round(pnorm((TRANSITIONPROP-mean(full_player_stats$TRANSITIONPROP[full_player_stats$FGA>=20]))/sd(full_player_stats$TRANSITIONPROP[full_player_stats$FGA>=20]))*100,0),
             '% of Scoring in Halfcourt %ile' = round(pnorm((HALFCOURTPROP-mean(full_player_stats$HALFCOURTPROP[full_player_stats$FGA>=20]))/sd(full_player_stats$HALFCOURTPROP[full_player_stats$FGA>=20]))*100,0),
      ) %>%
      filter(Team == team_name) %>%
      select(Player, PTSADDEDPP, 'Pts %ile', SHOTQUALITY, 'SQ %ile', SHOTMAKING, 'Shot-Making %ile', CREATIONVOLUMEPP, 'Creation Volume %ile', TRANSITIONPROP, '% of Scoring in Transition %ile', HALFCOURTPROP, '% of Scoring in Halfcourt %ile') %>%
      arrange(desc(PTSADDEDPP))
    colnames(basic_data) <- c('Player', 'Points Added', 'Pts %ile', 'Shot Quality', 'SQ %ile', 'Shot-Making', 'Shot-Making %ile', 'Creation Volume', 'Creation Volume %ile', '% of Scoring in Transition', '% of Scoring in Transition %ile', '% of Scoring in Halfcourt', '% of Scoring in Halfcourt %ile')
    return(basic_data)
  }
  
}


ui <- basicPage(
  titlePanel(title=div(img(src="https://upload.wikimedia.org/wikipedia/commons/thumb/0/06/UMass_Amherst_Athletics_logo.svg/2505px-UMass_Amherst_Athletics_logo.svg.png", height = 50, width =40, align = "right"))),
  
  h1(p(strong("UMass Basketball Dashboard"), align = "center", style = "color:darkred")),
  
  sidebarPanel(selectInput("team_name", h1("select box"),
                           label = "Team",
                           choices = c(teams$Team
                           ))
               
  ),
  sidebarPanel(selectInput("season", h1("select box"),
                           label = "Season",
                           choices = c("2021-22"
                           ))
               
  ),
  sidebarPanel(selectInput("type", h1("select box"),
                           label = "Type",
                           choices = c("Per Game",
                                       "Totals",
                                       "Per 40 Minutes",
                                       "Per 70 Possessions"
                           ))
               
  ),
  tags$style(HTML('table.dataTable.hover tbody tr:hover, table.dataTable.display tbody tr:hover {background-color: pink !important;}')),
  
  mainPanel(
    # Output: Tabset w/ plot, summary, and table ----
    tabsetPanel(type = "tabs",
                tabPanel("Roster", DT::dataTableOutput("roster")),
                tabPanel("Basic Player Stats", DT::dataTableOutput("stats")),
                tabPanel("Player Shooting", DT::dataTableOutput("shooting")),
                tabPanel("Player Scoring", DT::dataTableOutput("scoring")),
                tabPanel("Player Advanced", DT::dataTableOutput("advanced")),
                #setBackgroundColor("lightgrey")
    )
  )
  
)

server <- function(input, output) {
  output$stats = DT::renderDataTable({
    DT::datatable(get_basic_stats(team_name = input$team_name, type = input$type), options = list(pageLength = 25), rownames = F) %>%
      DT::formatStyle(columns = c('Pts %ile', 'Reb %ile', 'Ast %ile', 'STL %ile', 'BLK %ile', 'TOV %ile', '2PA %ile', '2% %ile', '3PA %ile', '3% %ile', 'FTA %ile', 'FT% %ile', 'OReb %ile', 'DReb %ile'), 
                      backgroundColor = styleInterval(c(0,10,20, 30, 40, 50, 60, 70, 80, 90), c("#f8696b", "#f98370", "#fa9d75", "#fcb77a", "#fdd17f", "#ffeb84", "#e0e383", "#c1da81", "#a2d07f", "#83c77d", "#63be7b")))
  })
  output$roster = DT::renderDataTable({
    DT::datatable(get_roster(team_name = input$team_name, season = input$season), options = list(pageLength = 25), rownames = F)
  })
  output$shooting = DT::renderDataTable({
    DT::datatable(get_shooting_stats(team_name = input$team_name, type = input$type), options = list(pageLength = 25), rownames = F) %>%
      DT::formatStyle(columns = c('FGA %ile', 'eFG% %ile', 'TS% %ile', 'Rim Attempt %ile', 'Rim% %ile', 'Mid Attempt %ile', 'Mid% %ile', '3PA %ile', '3% %ile', 'FTA %ile', 'FT% %ile'), 
                      backgroundColor = styleInterval(c(0,10,20, 30, 40, 50, 60, 70, 80, 90), c("#f8696b", "#f98370", "#fa9d75", "#fcb77a", "#fdd17f", "#ffeb84", "#e0e383", "#c1da81", "#a2d07f", "#83c77d", "#63be7b")))
    
  })
  output$scoring = DT::renderDataTable({
    DT::datatable(get_scoring_stats(team_name = input$team_name, type = input$type), options = list(pageLength = 25), rownames = F) %>%
      DT::formatStyle(columns = c('Pts %ile', 'Created Pts %ile', 'Assisted Pts %ile', 'Creation Load %ile', 'FGA %ile', 'Created eFG% %ile', 'Created Rim Attempt %ile', 'Created Rim% %ile', 'Created Mid Attempt %ile', 'Created Mid% %ile', 'Created 3PA %ile', 'Created 3% %ile'), 
                      backgroundColor = styleInterval(c(0,10,20, 30, 40, 50, 60, 70, 80, 90), c("#f8696b", "#f98370", "#fa9d75", "#fcb77a", "#fdd17f", "#ffeb84", "#e0e383", "#c1da81", "#a2d07f", "#83c77d", "#63be7b")))
    
  })
  output$advanced = DT::renderDataTable({
    DT::datatable(get_advanced_stats(team_name = input$team_name, type = input$type), options = list(pageLength = 25), rownames = F) %>%
      DT::formatStyle(columns = c('Pts %ile', 'SQ %ile', 'Shot-Making %ile', 'Creation Volume %ile', '% of Scoring in Transition %ile', '% of Scoring in Halfcourt %ile'), 
                      backgroundColor = styleInterval(c(0,10,20, 30, 40, 50, 60, 70, 80, 90), c("#f8696b", "#f98370", "#fa9d75", "#fcb77a", "#fdd17f", "#ffeb84", "#e0e383", "#c1da81", "#a2d07f", "#83c77d", "#63be7b")))
    
  })
}

shinyApp(ui, server)