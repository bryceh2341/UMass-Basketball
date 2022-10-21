library(gt)
library(cowplot)
library(ggpubr)

setwd("C:/Users/Bryce Haase/Desktop/UMass Basketball")

schedule_data <- read.csv("Schedule Projections.csv")

schedule_data %>%
  gt()  %>%
  cols_label(Date = "Date", Venue = "Venue", url="", Opponent = "Opponent", Projected.Result = "Result Proj.", 
             Odds.to.Win = "Win %", KenPom.Rank = "KenPom", Barttorvik.Rank = "Torvik", Haslametrics.Rank = "Haslam", EvanMiya.Rank = "EvanMiya") %>%
  tab_header(
    title = "Schedule Projections"
    #subtitle = "2021-22 Regular Season"
  )  %>%
  text_transform(
    locations = cells_body(vars(url)),
    fn = function(x) {
      web_image(url = x,
                height = px(15))
    }
  ) %>%
  # tab_spanner(
  #   label = "Layups",
  #   columns = vars(layup_tot, layup_per, layup_per_perc)
  # ) %>%
  # tab_spanner(
  #   label = "Midrange",
  #   columns = vars(jumper_tot, jumper_per, jumper_per_perc)
  # ) %>%
  # tab_spanner(
  #   label = "3-Pointers",
  #   columns = vars(three_pointer_tot, three_pointer_per, three_pointer_per_perc)
  # ) %>%
  # fmt_percent(
  #   columns = vars(layup_per_perc, jumper_per_perc, three_pointer_per_perc),
  #   decimals = 0
  # )  %>%
  # fmt_percent(
  #   columns = vars(layup_per, jumper_per, three_pointer_per),
  #   decimals = 1
  # )  %>%
  data_color(
    columns = vars(KenPom.Rank, Barttorvik.Rank, Haslametrics.Rank, EvanMiya.Rank),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "RColorBrewer::RdYlGn",
        direction  = 1
      ) %>% as.character(),
      domain = c(356,0),
      na.color = "#00441BFF"
    )
  ) %>%
  cols_align(
    align = "left",
    columns = vars(Date, Venue, Opponent, Projected.Result, Odds.to.Win, KenPom.Rank, Barttorvik.Rank, Haslametrics.Rank, EvanMiya.Rank)
  ) %>%
  cols_width(
             vars(Opponent) ~ px(120),
             vars(Projected.Result) ~ px(85),
             vars(Venue, Odds.to.Win, Barttorvik.Rank, Haslametrics.Rank, EvanMiya.Rank) ~ px(45),
             vars(Date) ~ px(60))%>%
  tab_style(
    style = list(
      cell_borders(
        side =  "top",
        color = 'gray55',
        weight = px(2)
      )
    ),
    locations = cells_body(
      rows = Date == "League Average"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "floralwhite"),
    locations = cells_body(
      rows = Date == "League Average")
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
  gtsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Schedule Projections.png")

schedule_outlook <- read.csv("Schedule Outlook.csv")

schedule_outlook %>%
  gt()  %>%
  cols_label(Date = "Date", Venue = "Venue", url="", Opponent = "Opponent", Record = "Record", Offense = "Off.", Defense = "Def.", KenPom = "KenPom", NET = "NET") %>%
  tab_header(
    title = "Schedule Outlook"
    #subtitle = "2021-22 Regular Season"
  )  %>%
  text_transform(
    locations = cells_body(vars(url)),
    fn = function(x) {
      web_image(url = x,
                height = px(14))
    }
  ) %>%
  # tab_spanner(
  #   label = "Layups",
  #   columns = vars(layup_tot, layup_per, layup_per_perc)
  # ) %>%
  # tab_spanner(
  #   label = "Midrange",
  #   columns = vars(jumper_tot, jumper_per, jumper_per_perc)
  # ) %>%
  # tab_spanner(
  #   label = "3-Pointers",
  #   columns = vars(three_pointer_tot, three_pointer_per, three_pointer_per_perc)
# ) %>%
# fmt_percent(
#   columns = vars(layup_per_perc, jumper_per_perc, three_pointer_per_perc),
#   decimals = 0
# )  %>%
# fmt_percent(
#   columns = vars(layup_per, jumper_per, three_pointer_per),
#   decimals = 1
# )  %>%
data_color(
  columns = vars(Offense, Defense, KenPom, NET),
  colors = scales::col_numeric(
    palette = paletteer::paletteer_d(
      palette = "RColorBrewer::RdYlGn",
      direction  = 1
    ) %>% as.character(),
    domain = c(356,0),
    na.color = "#00441BFF"
  )
) %>%
  cols_align(
    align = "left",
    columns = vars(Date, Venue, Opponent, Record, Offense, Defense, KenPom, NET)
  ) %>%
  cols_width(
    vars(Opponent) ~ px(120),
    vars(Venue, Record, Offense, Defense, KenPom, NET) ~ px(40),
    vars(Date) ~ px(60)) %>%
  tab_style(
    style = list(
      cell_borders(
        side =  "top",
        color = 'gray55',
        weight = px(2)
      )
    ),
    locations = cells_body(
      rows = Date == "League Average"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "floralwhite"),
    locations = cells_body(
      rows = Date == "League Average")
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
  gtsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Schedule Outlook.png")