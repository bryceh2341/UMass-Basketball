library(bigballR)
library(toRvik)
library(gt)
library(cowplot)
library(ggpubr)
library(RGraphics)
library(gridExtra)
library(stringr)
library(dplyr)
library(ncaahoopR)

setwd("C:/Users/Bryce Haase/Desktop/UMass Basketball")

team_name <- "Hofstra"
ncaa_team_name <- "Hofstra"
hoop_r_team_name <- "Hofstra"

bart_player_stats <- bart_player_season(year=2023, stat = 'all')
basic_player_stats <- bart_player_stats %>%
  filter(team == team_name) %>%
  mutate(mpg = round(mpg,1),
         ppg = round(ppg,1),
         apg = round(apg,1),
         rpg = round(rpg,1),
         spg = round(spg,1),
         bpg = round(bpg,1),
         fg_pct = round(fg_pct*100,1),
         three_pct = round(three_pct*100,1)) %>%
  select(player, pos, num, hgt, mpg, ppg, rpg, apg, spg, bpg, fg_pct, three_pct)

# shooting_player_stats <- bart_player_stats %>%
#   filter(team == team_name) %>%
#   arrange(desc(mpg)) %>%
#   mutate(rim = paste0(rim_m, "/", rim_a),
#          rim_pct = round(rim_pct*100,1),
#          mid = paste0(mid_m, "/", mid_a),
#          mid_pct = round(mid_pct*100,1),
#          three = paste0(three_m, "/", three_a),
#          three_pct = round(three_pct*100,1),
#          ft = paste0(ftm, "/", fta),
#          ft_pct = round(ft_pct*100,1)) %>%
#   select(player, num, mpg, efg, ts, rim, rim_pct, mid, mid_pct, three, three_pct, ft, ft_pct)
# shooting_player_stats[is.na(shooting_player_stats)] <- 0

schedule <- get_team_schedule(team.name = ncaa_team_name, season = "2022-23")
stats <- get_player_stats(play_by_play_data = get_play_by_play(schedule$Game_ID[!is.na(schedule$Game_ID)]), multi.games = T, simple = F)
roster <- get_team_roster(team.name = ncaa_team_name, season = "2022-23")
roster <- roster %>%
  mutate(num = Jersey) %>%
  select(Player, num)
stats <- stats %>%
  filter(Team == ncaa_team_name) %>%
  mutate(rim = paste0(RIMM, "/", RIMA),
         rim_pct = round(RIM.*100,1),
         mid = paste0(MIDM, "/", MIDA),
         mid_pct = round(MID.*100,1),) %>%
  select(Player, rim, rim_pct, mid, mid_pct)
stats <- merge(stats, roster, by="Player")
stats <- stats %>%
  select(num, rim, rim_pct, mid, mid_pct)
stats$num = as.numeric(as.character(stats$num))

shooting_player_stats <- bart_player_stats %>%
  filter(team == team_name) %>%
  select(team, player, num, mpg, efg, ts, three_m, three_a, three_pct, ftm, fta, ft_pct)

shooting_player_stats <- merge(shooting_player_stats, stats, by="num")

shooting_player_stats <- shooting_player_stats %>%
  filter(team == team_name) %>%
  arrange(desc(mpg)) %>%
  mutate(three = paste0(three_m, "/", three_a),
         three_pct = round(three_pct*100,1),
         ft = paste0(ftm, "/", fta),
         ft_pct = round(ft_pct*100,1)) %>%
  select(player, num, mpg, efg, ts, rim, rim_pct, mid, mid_pct, three, three_pct, ft, ft_pct)
shooting_player_stats[is.na(shooting_player_stats)] <- 0



advanced_player_stats <- bart_player_stats %>%
  filter(team == team_name) %>%
  mutate(
    ast_to = round(ast_to,1),
    net_rating = round(ortg-drtg,1),
    obpm = round(obpm,1), 
    dbpm = round(dbpm,1), 
    bpm = round(bpm,1)
  ) %>%
  select(player, num, mpg, usg, net_rating, oreb_rate, dreb_rate, ast, to, ast_to, blk, stl, ftr, pfr, obpm, dbpm, bpm)


team_roster <- ncaahoopR::get_roster(hoop_r_team_name, season = "2022-23")
team_roster <- team_roster %>%
  mutate(num = number) %>%
  select(num, name, position, height, player_image, class, weight)

basic_player_stats <- merge(basic_player_stats, team_roster, by="num")
basic_player_stats <- basic_player_stats %>%
  select(player_image, player, class, num, position, hgt, weight, mpg, ppg, rpg, apg, spg, bpg, fg_pct, three_pct) %>%
  arrange(desc(mpg))

shooting_player_stats <- merge(team_roster, shooting_player_stats, by="num")
shooting_player_stats <- shooting_player_stats %>%
  arrange(desc(mpg)) %>%
  select(player_image, player, efg, ts, rim, rim_pct, mid, mid_pct, three, three_pct, ft, ft_pct)

advanced_player_stats <- merge(advanced_player_stats, team_roster, by="num")
advanced_player_stats <- advanced_player_stats %>%
  arrange(desc(mpg)) %>%
  select(player_image, player, usg, net_rating, oreb_rate, dreb_rate, ast, to, ast_to, blk, stl, ftr, pfr, obpm, dbpm, bpm)

plot_1 <- basic_player_stats %>%
  gt() %>%
  cols_label(player_image = "", player = "Player", class = "Year", num = "#", position = "Pos", hgt = "Hgt.", weight = "Wgt.", mpg = "MPG", ppg = "PPG", rpg = "RPG", apg = "APG", spg = "Stl", bpg = "Blk", fg_pct = "FG%", three_pct = "3pt%"
  ) %>%
  tab_header(
    title = md("Player Stats"),
    #subtitle = table_subtitle
  )  %>%
  text_transform(
    locations = cells_body(vars(player_image)),
    fn = function(x) {
      web_image(url = x,
                height = px(22.5))
    }
  ) %>%
  data_color(
    columns = c(ppg),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(apg),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(rpg),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(fg_pct),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(three_pct),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_align(
    align = "left",
    columns = vars(player_image, player, class, num, position, hgt, weight, mpg, ppg, rpg, apg, spg, bpg, fg_pct, three_pct)
  ) %>%
  cols_width(vars(player_image, class, num, hgt, mpg, ppg, rpg, apg, spg, bpg, fg_pct, three_pct) ~ px(35),
             vars(player) ~ px(115),
             vars(weight) ~ px(50),
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
    #column_labels.hidden = TRUE
  ) %>%
  gtsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Basic Player Stats.png", expand = 0)

plot_2 <- shooting_player_stats %>%
  gt() %>%
  cols_label(player_image = "", player = "Player", efg = "eFG%", ts = "TS%", rim  ="Rim", rim_pct = "Rim%", mid = "Mid", mid_pct = "Mid%", three = "3", three_pct = "3%", ft = "FT", ft_pct = "FT%"
  ) %>%
  tab_header(
    title = md("Player Shooting Stats"),
    #subtitle = table_subtitle
  )  %>%
  text_transform(
    locations = cells_body(vars(player_image)),
    fn = function(x) {
      web_image(url = x,
                height = px(22.5))
    }
  ) %>%
  data_color(
    columns = c(efg),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(ts),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(rim_pct),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(mid_pct),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(three_pct),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(ft_pct),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_align(
    align = "left",
    columns = vars(player_image, player, efg, ts, rim, rim_pct, mid, mid_pct, three, three_pct, ft, ft_pct)
  ) %>%
  cols_width(vars(player_image, efg, ts, rim_pct, mid_pct, three_pct, ft_pct) ~ px(35),
             vars(player) ~ px(115),
             vars(rim, mid, three, ft) ~ px(45),
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
    #column_labels.hidden = TRUE
  ) %>%
  gtsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Shooting Player Stats.png", expand = 0)

plot_3 <- advanced_player_stats %>%
  gt() %>%
  cols_label(player_image = "", player = "Player", usg = "Usg%", net_rating = "Net Rtg", oreb_rate = "OReb%", dreb_rate = "DReb%", ast = "Ast%", to = "TOV%", ast_to = "Ast/TOV", blk = "Blk%", stl = "Stl%", ftr = "FTr", pfr = "PFr", obpm = "OBPM", dbpm = "DBPM", bpm = "BPM"
  ) %>%
  tab_header(
    title = md("Advanced Player Stats"),
    #subtitle = table_subtitle
  )  %>%
  text_transform(
    locations = cells_body(vars(player_image)),
    fn = function(x) {
      web_image(url = x,
                height = px(22.5))
    }
  ) %>%
  data_color(
    columns = c(usg),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(net_rating),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(bpm),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(oreb_rate),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(dreb_rate),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(blk),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(stl),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_align(
    align = "left",
    columns = vars(player_image, player, usg, net_rating, oreb_rate, dreb_rate, ast, to, ast_to, blk, stl, ftr, pfr, obpm, dbpm, bpm)
  ) %>%
  cols_width(vars(player_image, usg, dreb_rate, ast, to, blk, stl, ftr, pfr, obpm, dbpm, bpm) ~ px(35),
             vars(player) ~ px(115),
             vars(net_rating, ast_to) ~ px(55),
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
    #column_labels.hidden = TRUE
  ) %>%
  gtsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Advanced Player Stats.png", expand = 0)

# for (i in 1:length(player_stats$num)){
#   individual_stats <- data.frame(Type = c("Min", 'PPG', 'RPG', 'APG', 'Stl', 'Blk', "FG%", "3pt%"), Stat = c(player_stats$mpg[i],player_stats$ppg[i],player_stats$rpg[i],player_stats$apg[i],player_stats$spg[i],player_stats$bpg[i],player_stats$fg_pct[i],player_stats$three_pct[i]), Notes = c("","","","","","","",""))
#   
#   title_image <- paste0("<img src='",player_stats$player_image[i],"' style='height:60px'>",player_stats$player[i])
#   table_label <- paste0("#",player_stats$num[i], " | ", player_stats$position[i], " | ", player_stats$class[i], " | ", player_stats$height[i], " | ", player_stats$weight[i])
#   save_link <- paste0("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Dayton\\", player_stats$player[i],".png")
#   
#   individual_stats %>%
#     gt()  %>%
#     cols_label(Type = "", Stat = "", Notes = "Notes") %>%
#     tab_header(
#       #title = md("<p style='float: left;'><img src='https://a.espncdn.com/combiner/i?img=/i/headshots/mens-college-basketball/players/full/4433607.png&w=350&h=254' height='75px' width='75px'></p>
#       #  <p style = 'font-size: 15px'>Day'Ron Holmes II</p>")
#       #title = md("<img src='https://a.espncdn.com/combiner/i?img=/i/headshots/mens-college-basketball/players/full/4433607.png&w=350&h=254' style='height:60px'>  Da'Ron Holmes II"),
#       title = md(title_image),
#       #title = "Stats",
#       #subtitle = "Center | Sophomore | 6'10 | 235"
#     )  %>%
#     tab_spanner(label = table_label, columns = vars(Notes)) %>%
#     cols_align(
#       align = "left",
#       columns = vars(Stat)
#     ) %>%
#     cols_align(
#       align = "center",
#       columns = vars(Notes)
#     ) %>%
#     cols_width(vars(Type, Stat) ~ px(40),
#                vars(Notes) ~ px(300),
#     ) %>%
#     tab_style(
#       style = cell_borders(
#         sides = c("all"),
#         color = "floralwhite",
#         weight = px(0),
#         style = "solid"
#       ),
#       locations = cells_body(
#         columns = vars(Notes),
#         rows = everything()
#       )
#     ) %>%
#     tab_style(
#       style = cell_borders(
#         sides = c("right"),
#         color = "gray55",
#         weight = px(1),
#         style = "solid"
#       ),
#       locations = cells_body(
#         columns = vars(Stat),
#         rows = everything()
#       )
#     ) %>%
#     tab_options(
#       table.background.color = "floralwhite",
#       column_labels.font.size = 10.5,
#       table.font.size = 10,
#       heading.title.font.size  = 24,
#       heading.title.font.weight = 'bold',
#       heading.subtitle.font.size = 11,
#       heading.subtitle.font.weight = 'bold',
#       table.font.names = "Consolas",
#       table.font.color = 'black',
#       #table.border.top.color = "transparent",
#       data_row.padding = px(0),
#       footnotes.font.size = 0,
#       source_notes.font.size = 0,
#       footnotes.padding = px(0),
#     ) %>%
#     gtsave(save_link)
# }

#--------------------------------------------------------------------------------------------------------------------------------
# team_name <- "Dayton"
# 
# bart_player_stats <- bart_player_season(year=2022, stat = 'all')
# bart_player_stats <- bart_player_stats %>%
#   filter(team == team_name) %>%
#   mutate(mpg = round(mpg,1),
#          ppg = round(ppg,1),
#          apg = round(apg,1),
#          rpg = round(rpg,1),
#          spg = round(spg,1),
#          bpg = round(bpg,1),
#          fg_pct = round(fg_pct*100,1),
#          three_pct = round(three_pct*100,1)) %>%
#   select(player, pos, num, hgt, mpg, ppg, rpg, apg, spg, bpg, fg_pct, three_pct)
# 
# team_roster <- ncaahoopR::get_roster(team_name, season = "2021-22")
# team_roster <- team_roster %>%
#   mutate(num = number) %>%
#   select(num, name, position, height, player_image, class, weight)
# 
# rep_str = c('F'='Forward','C'='Center','G'='Guard')
# team_roster$position <- str_replace_all(team_roster$position, rep_str)
# 
# player_stats <- merge(bart_player_stats, team_roster, by="num")
# 
# 
# individual_stats <- data.frame(Type = c("Min", 'PPG', 'RPG', 'APG', 'Stl', 'Blk', "FG%", "3pt%"), Stat = c(player_stats$mpg[8],player_stats$ppg[8],player_stats$rpg[8],player_stats$apg[8],player_stats$spg[8],player_stats$bpg[8],player_stats$fg_pct[8],player_stats$three_pct[8]), Notes = c("","","","","","","",""))
# 
# title_image <- paste0("<img src='",player_stats$player_image[8],"' style='height:60px'>",player_stats$player[8])
# table_label <- paste0("#",player_stats$num[8], " | ", player_stats$position[8], " | ", player_stats$class[8], " | ", player_stats$height[8], " | ", player_stats$weight[8])
# save_link <- paste0("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Dayton\\", player_stats$player[8],".png")
# 
# individual_stats %>%
#   gt()  %>%
#   cols_label(Type = "", Stat = "", Notes = "Notes") %>%
#   tab_header(
#     #title = md("<p style='float: left;'><img src='https://a.espncdn.com/combiner/i?img=/i/headshots/mens-college-basketball/players/full/4433607.png&w=350&h=254' height='75px' width='75px'></p>
#     #  <p style = 'font-size: 15px'>Day'Ron Holmes II</p>")
#     #title = md("<img src='https://a.espncdn.com/combiner/i?img=/i/headshots/mens-college-basketball/players/full/4433607.png&w=350&h=254' style='height:60px'>  Da'Ron Holmes II"),
#     title = md(title_image),
#     #title = "Stats",
#     #subtitle = "Center | Sophomore | 6'10 | 235"
#   )  %>%
#   tab_spanner(label = table_label, columns = vars(Notes)) %>%
#   cols_align(
#     align = "left",
#     columns = vars(Stat)
#   ) %>%
#   cols_align(
#     align = "center",
#     columns = vars(Notes)
#   ) %>%
#   cols_width(vars(Type, Stat) ~ px(40),
#              vars(Notes) ~ px(300),
#              ) %>%
#   tab_style(
#     style = cell_borders(
#       sides = c("all"),
#       color = "floralwhite",
#       weight = px(0),
#       style = "solid"
#     ),
#     locations = cells_body(
#       columns = vars(Notes),
#       rows = everything()
#     )
#   ) %>%
#     tab_style(
#       style = cell_borders(
#         sides = c("right"),
#         color = "gray55",
#         weight = px(1),
#         style = "solid"
#       ),
#       locations = cells_body(
#         columns = vars(Stat),
#         rows = everything()
#       )
#     ) %>%
#   tab_options(
#     table.background.color = "floralwhite",
#     column_labels.font.size = 10.5,
#     table.font.size = 10,
#     heading.title.font.size  = 24,
#     heading.title.font.weight = 'bold',
#     heading.subtitle.font.size = 11,
#     heading.subtitle.font.weight = 'bold',
#     table.font.names = "Consolas",
#     table.font.color = 'black',
#     #table.border.top.color = "transparent",
#     data_row.padding = px(0),
#     footnotes.font.size = 0,
#     source_notes.font.size = 0,
#     footnotes.padding = px(0),
#   ) %>%
#   gtsave(save_link)





# par(mar = c(0,0,0,0))
# plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
# 
# text(x = 0.34, y = 0.9, paste("Da'Ron Holmes II"), 
#      cex = 1.5, col = "black", family="serif", font=2, adj=0.5)
# text(x = 0.34, y = 0.85, paste("Center | Sophomore | 6'10 | 235"), 
#      cex = 1.2, col = "gray30", family="sans", font=1, adj=0.5)

# plot2 <- ggplot() +
#   annotate("text",
#            x = 1,
#            y = 1,
#            size = 5,
#            fontface="bold",
#            label = "Day'Ron Holmes II") + 
#   annotate("text",
#            x = 1,
#            y = 0.99,
#            size = 4,
#            label = "Center | Sophomore | 6'10 | 235\n\n\n\n\n\n\n\n") +
#   theme_void()
# 
# plot0 <- ggdraw() + draw_image("https://a.espncdn.com/combiner/i?img=/i/headshots/mens-college-basketball/players/full/4433607.png&w=350&h=254", scale=1)
# plot1 <- ggdraw() + draw_image("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Stats_Test.png", scale = 1)
# total_plot<-ggarrange(plot0, plot2, plot1,  ncol=2, nrow=2) + bgcolor("white")
# ggsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Test_Frame.png")