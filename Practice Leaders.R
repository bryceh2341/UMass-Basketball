library(gt)
library(cowplot)
library(ggpubr)
library(dplyr)
library(gtExtras)

setwd("C:/Users/Bryce Haase/Desktop/UMass Basketball")

practice_data <- read.csv("Stat Sheet Printable.csv")

drill_name <- "Points"

#table_subtitle <- paste0(format(Sys.Date(), format="%B %d, %Y"), " | ", drill_name)
#table_subtitle <- "October 20 - October 27 Totals"

save_name <- paste0(format(Sys.Date(), format="%B %d"), " - ", drill_name, " Stat Sheet.png")

practice_data[is.na(practice_data)] = 0

full_practice_data <- practice_data %>%
  mutate(Rim_Per = round(Rim_Make/(Rim_Make+Rim_Miss)*100, 1),
         Mid_Per = round(Mid_Make/(Mid_Make+Mid_Miss)*100, 1),
         Three_Per = round(Three_Make/(Three_Make+Three_Miss)*100,1),
         Rim = paste0(Rim_Make, "/", Rim_Make+Rim_Miss),
         Mid = paste0(Mid_Make, "/", Mid_Make+Mid_Miss),
         Three = paste0(Three_Make, "/", Three_Make+Three_Miss),
         Reb = Off_Reb+Def_Reb) %>%
  select(url, Player, Pts, Reb, Ast, Rim, Rim_Per, Mid, Mid_Per, Three, Three_Per, Off_Reb, Def_Reb, Stl, Blk, Tov, Charges, Seals, Fouls, Successful_screen, Unsuccessful_screen)

full_practice_data[is.na(full_practice_data)] = 0

basic_practice_data <- practice_data %>%
  mutate(Two = paste0(Rim_Make+Mid_Make, "/", Rim_Make+Rim_Miss+Mid_Make+Mid_Miss),
         Two_Per = round((Rim_Make+Mid_Make)/(Rim_Make+Rim_Miss+Mid_Make+Mid_Miss)*100,1),
         Reb = Off_Reb+Def_Reb,
         Three_Per = round(Three_Make/(Three_Make+Three_Miss)*100,1),
         Three = paste0(Three_Make, "/", Three_Make+Three_Miss),
  ) %>%
  select(url, Player, Pts, Reb, Ast, Two, Two_Per, Three, Three_Per, Off_Reb, Def_Reb, Stl, Blk, Tov, Fouls)

basic_practice_data[is.na(basic_practice_data)] <- 0


points_leaders <- full_practice_data %>%
  arrange(desc(Pts)) %>%
  slice(1:3) %>%
  select(url, Player, Pts)

rebounds_leaders <- full_practice_data %>%
  arrange(desc(Reb)) %>%
  slice(1:3) %>%
  select(url, Player, Reb)

assists_leaders <- full_practice_data %>%
  arrange(desc(Ast)) %>%
  slice(1:3) %>%
  select(url, Player, Ast)

two_leaders <- basic_practice_data %>%
  arrange(desc(Two_Per)) %>%
  slice(1:3) %>%
  select(url, Player, Two, Two_Per)

three_leaders <- basic_practice_data %>%
  arrange(desc(Three_Per)) %>%
  slice(1:3) %>%
  select(url, Player, Three, Three_Per)

tov_leaders <- full_practice_data %>%
  arrange(desc(Tov)) %>%
  slice(1:3) %>%
  select(url, Player, Tov)

plot_1 <- points_leaders %>%
  gt() %>%
  cols_label(url = "",
             Player = "",
             Pts = ""
  ) %>%
  tab_header(
    title = md("Points"),
    #subtitle = table_subtitle
  )  %>%
  text_transform(
    locations = cells_body(vars(url)),
    fn = function(x) {
      web_image(url = x,
                height = px(22.5))
    }
  ) %>%
  cols_align(
    align = "left",
    columns = vars(url, Player, Pts)
  ) %>%
  cols_width(vars(url, Pts) ~ px(35),
             vars(Player) ~ px(115),
             #vars(Charges, Rim, Three) ~ px(45),
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
      rows = url == "League Average"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "floralwhite"),
    locations = cells_body(
      rows = url == "League Average")
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
    column_labels.hidden = TRUE
  ) %>%
  gtsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Pts Practice Leaders.png", expand = 0)

plot_2 <- rebounds_leaders %>%
  gt() %>%
  cols_label(url = "",
             Player = "",
             Reb = ""
  ) %>%
  tab_header(
    title = md("Rebounds"),
    #subtitle = table_subtitle
  )  %>%
  text_transform(
    locations = cells_body(vars(url)),
    fn = function(x) {
      web_image(url = x,
                height = px(22.5))
    }
  ) %>%
  cols_align(
    align = "left",
    columns = vars(url, Player, Reb)
  ) %>%
  cols_width(vars(url, Reb) ~ px(35),
             vars(Player) ~ px(115),
             #vars(Charges, Rim, Three) ~ px(45),
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
      rows = url == "League Average"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "floralwhite"),
    locations = cells_body(
      rows = url == "League Average")
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
    column_labels.hidden = TRUE
  ) %>%
  gtsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Reb Practice Leaders.png", expand = 0)

plot_3 <- assists_leaders %>%
  gt() %>%
  cols_label(url = "",
             Player = "",
             Ast = ""
  ) %>%
  tab_header(
    title = md("Assists"),
    #subtitle = table_subtitle
  )  %>%
  text_transform(
    locations = cells_body(vars(url)),
    fn = function(x) {
      web_image(url = x,
                height = px(22.5))
    }
  ) %>%
  cols_align(
    align = "left",
    columns = vars(url, Player, Ast)
  ) %>%
  cols_width(vars(url, Ast) ~ px(35),
             vars(Player) ~ px(115),
             #vars(Charges, Rim, Three) ~ px(45),
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
      rows = url == "League Average"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "floralwhite"),
    locations = cells_body(
      rows = url == "League Average")
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
    column_labels.hidden = TRUE
  ) %>%
  gtsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Ast Practice Leaders.png", expand = 0)

plot_4 <- tov_leaders %>%
  gt() %>%
  cols_label(url = "",
             Player = "",
             Tov = ""
  ) %>%
  tab_header(
    title = md("Turnovers"),
    #subtitle = table_subtitle
  )  %>%
  text_transform(
    locations = cells_body(vars(url)),
    fn = function(x) {
      web_image(url = x,
                height = px(22.5))
    }
  ) %>%
  cols_align(
    align = "left",
    columns = vars(url, Player, Tov)
  ) %>%
  cols_width(vars(url, Tov) ~ px(35),
             vars(Player) ~ px(115),
             #vars(Charges, Rim, Three) ~ px(45),
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
      rows = url == "League Average"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "floralwhite"),
    locations = cells_body(
      rows = url == "League Average")
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
    column_labels.hidden = TRUE
  ) %>%
  gtsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Tov Practice Leaders.png", expand = 0)

plot_5 <- two_leaders %>%
  gt() %>%
  cols_label(url = "",
             Player = "",
             Two = "",
             Two_Per = ""
  ) %>%
  tab_header(
    title = md("2pt%"),
    #subtitle = table_subtitle
  )  %>%
  text_transform(
    locations = cells_body(vars(url)),
    fn = function(x) {
      web_image(url = x,
                height = px(22.5))
    }
  ) %>%
  cols_align(
    align = "left",
    columns = vars(url, Player, Two, Two_Per)
  ) %>%
  cols_width(vars(url, Two, Two_Per) ~ px(35),
             vars(Player) ~ px(115),
             #vars(Charges, Rim, Three) ~ px(45),
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
      rows = url == "League Average"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "floralwhite"),
    locations = cells_body(
      rows = url == "League Average")
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
    column_labels.hidden = TRUE
  ) %>%
  gtsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Two Practice Leaders.png", expand = 0)

plot_6 <- three_leaders %>%
  gt() %>%
  cols_label(url = "",
             Player = "",
             Three = "",
             Three_Per = ""
  ) %>%
  tab_header(
    title = md("3pt%"),
    #subtitle = table_subtitle
  )  %>%
  text_transform(
    locations = cells_body(vars(url)),
    fn = function(x) {
      web_image(url = x,
                height = px(22.5))
    }
  ) %>%
  cols_align(
    align = "left",
    columns = vars(url, Player, Three, Three_Per)
  ) %>%
  cols_width(vars(url, Three, Three_Per) ~ px(35),
             vars(Player) ~ px(115),
             #vars(Charges, Rim, Three) ~ px(45),
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
      rows = url == "League Average"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "floralwhite"),
    locations = cells_body(
      rows = url == "League Average")
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
    column_labels.hidden = TRUE
  ) %>%
  gtsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Three Practice Leaders.png", expand = 0)

plot_1 <- ggdraw() + draw_image("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Pts Practice Leaders.png", scale = 1)
plot_2 <- ggdraw() + draw_image("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Reb Practice Leaders.png", scale = 1)
plot_3 <- ggdraw() + draw_image("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Ast Practice Leaders.png", scale = 1)
plot_4 <- ggdraw() + draw_image("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Tov Practice Leaders.png", scale = 1)
plot_5 <- ggdraw() + draw_image("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Two Practice Leaders.png", scale = 1)
plot_6 <- ggdraw() + draw_image("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Three Practice Leaders.png", scale = 1)
total_plot<-ggarrange(plot_1, plot_2, plot_3, plot_4, plot_5, plot_6, ncol=2, nrow=3) + bgcolor("transparent")
#annotate_figure(total_plot, top = text_grob("Complete Leaders", color = "black", face = "bold", family = "Consolas", size = 25))
ggsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Complete Leaders.png")