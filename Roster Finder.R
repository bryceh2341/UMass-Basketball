library(bigballR)
library(ncaahoopR)
library(gt)
library(cowplot)
library(ggpubr)

team_name <- "Kentucky"

team <- get_roster(team_name, season = "2021-22")
team <- team %>%
  mutate(Jersey = number) %>%
  select(Jersey, weight, player_image)

roster <- get_team_roster(season = "2021-22", team.name = team_name)

roster <- merge(team, roster, by="Jersey")
printable_roster <- roster %>%
  select(player_image, Jersey, CleanName, Pos, Yr, Ht, weight)

printable_roster %>%
  gt()  %>%
  cols_label(player_image="", Jersey="#", CleanName="Player", Pos="Pos.", Yr="Yr.", Ht="Ht.", weight="Wt.") %>%
  tab_header(
    title = team_name,
    subtitle = "2021-22 Roster"
  )  %>%
  text_transform(
    locations = cells_body(vars(player_image)),
    fn = function(x) {
      web_image(url = x,
                height = px(22))
    }
  ) %>%
  cols_align(
    align = "left",
    columns = vars(player_image, Jersey, CleanName, Pos, Yr, Ht, weight)
  ) %>%
  cols_width(
    vars(CleanName) ~ px(120),
    vars(Jersey, Pos, Yr, Ht) ~ px(40),
    vars(player_image,weight) ~ px(55)) %>%
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
  gtsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Roster.png")