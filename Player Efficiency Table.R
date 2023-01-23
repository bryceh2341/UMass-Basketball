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

bart_player_stats <- bart_player_season(year=2023, stat = 'all')

bart_player_stats <- bart_player_stats %>%
  mutate(eff = ppg+apg+rpg+spg+bpg+(fta*0.44/g) - (((fga-fgm)/g)+((fta-ftm)/g)+tov+(pfr/40*mpg)),
         eff = round(eff,1)) %>%
  filter(team == "Massachusetts") %>%
  select(player, num, eff)

team_roster <- ncaahoopR::get_roster("UMass", season = "2022-23")
team_roster <- team_roster %>%
  mutate(num = number) %>%
  select(player_image, num)

team_roster <- merge(team_roster,bart_player_stats, by="num")

team_roster <- team_roster %>%
  select(player_image, player, eff) %>%
  arrange(desc(eff)) %>%
  slice(1:12)

team_roster %>%
  gt() %>%
  cols_label(player_image = "",
             player = "Player",
             eff = "Eff."
  ) %>%
  tab_header(
    title = md("UMass Player Efficiency"),
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
    columns = c(eff),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_align(
    align = "left",
    columns = vars(eff)
  ) %>%
  cols_align(
    align = "center",
    columns = vars(player_image, player)
  ) %>%
  cols_width(vars(player_image) ~ px(35),
             vars(player) ~ px(175),
             vars(eff) ~ px(79),
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
      rows = player == "League Average"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "floralwhite"),
    locations = cells_body(
      rows = player == "League Average")
  ) %>%
  tab_options(
    table.background.color = "floralwhite",
    column_labels.font.size = 10.5,
    table.font.size = 10,
    heading.title.font.size  = 20,
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
  gtsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Player Efficiency.png", expand = 0)

# schedule <- get_team_schedule(team.name = "Massachusetts", season = "2022-23")
# stats <- get_player_stats(play_by_play_data = get_play_by_play(schedule$Game_ID[!is.na(schedule$Game_ID)]), multi.games = T, simple = F)