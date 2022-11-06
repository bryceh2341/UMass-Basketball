library(gt)
library(cowplot)
library(ggpubr)
library(dplyr)
library(gtExtras)

setwd("C:/Users/Bryce Haase/Desktop/UMass Basketball")

practice_data <- read.csv("Stat Sheet Printable.csv")

drill_name <- "Overall"

table_subtitle <- paste0(format(Sys.Date(), format="%B %d, %Y"), " | ", drill_name)
#table_subtitle <- "October 20 - October 27 Totals"

save_name <- paste0(format(Sys.Date(), format="%B %d"), " - ", drill_name, " Stat Sheet.png")

practice_data[is.na(practice_data)] = 0

full_practice_data <- practice_data %>%
  mutate(Rim_Per = round(Rim_Make/(Rim_Make+Rim_Miss)*100, 1),
         Mid_Per = round(Mid_Make/(Mid_Make+Mid_Miss)*100, 1),
         Three_Per = round(Three_Make/(Three_Make+Three_Miss)*100,1),
         FT_Per = round(FTM/(FTA+FTM)*100,1),
         Rim = paste0(Rim_Make, "/", Rim_Make+Rim_Miss),
         Mid = paste0(Mid_Make, "/", Mid_Make+Mid_Miss),
         Three = paste0(Three_Make, "/", Three_Make+Three_Miss),
         FT = (paste0(FTM, "/", FTM+FTA)),
         Reb = Off_Reb+Def_Reb) %>%
  select(url, Player, Pts, Reb, Ast, Rim, Rim_Per, Mid, Mid_Per, Three, Three_Per, FT, FT_Per, Off_Reb, Def_Reb, Stl, Blk, Tov, Charges, Seals, Fouls, Successful_screen, Unsuccessful_screen)

full_practice_data[is.na(full_practice_data)] = 0

totals_row <- c('https://upload.wikimedia.org/wikipedia/commons/thumb/2/23/UMass_Amherst_athletics_logo.svg/1200px-UMass_Amherst_athletics_logo.svg.png',
                "Totals",
                sum(practice_data$Pts),
                sum(practice_data$Off_Reb)+sum(practice_data$Def_Reb),
                sum(practice_data$Ast),
                paste0(sum(practice_data$Rim_Make), "/", sum(practice_data$Rim_Make)+sum(practice_data$Rim_Miss)),
                round(sum(practice_data$Rim_Make)/(sum(practice_data$Rim_Make)+sum(practice_data$Rim_Miss))*100,1),
                paste0(sum(practice_data$Mid_Make), "/", sum(practice_data$Mid_Make)+sum(practice_data$Mid_Miss)),
                round(sum(practice_data$Mid_Make)/(sum(practice_data$Mid_Make)+sum(practice_data$Mid_Miss))*100,1),
                paste0(sum(practice_data$Three_Make), "/", sum(practice_data$Three_Make)+sum(practice_data$Three_Miss)),
                round(sum(practice_data$Three_Make)/(sum(practice_data$Three_Make)+sum(practice_data$Three_Miss))*100,1),
                paste0(sum(practice_data$FTM), "/", sum(practice_data$FTM)+sum(practice_data$FTA)),
                round(sum(practice_data$FTM)/(sum(practice_data$FTA)+sum(practice_data$FTM))*100,1),
                sum(practice_data$Off_Reb),
                sum(practice_data$Def_Reb),
                sum(practice_data$Stl),
                sum(practice_data$Blk),
                sum(practice_data$Tov),
                sum(practice_data$Charges),
                sum(practice_data$Seals),
                sum(practice_data$Fouls),
                sum(practice_data$Successful_screen),
                sum(practice_data$Unsuccessful_screen)
)
# round(sum(practice_data$Three_Make)/(sum(practice_data$Three_Make)+sum(practice_data$Three_Miss))*100,1),
full_practice_data[nrow(full_practice_data) + 1,] <- totals_row

full_practice_data$Rim_Per = as.numeric(as.character(full_practice_data$Rim_Per))
full_practice_data$Mid_Per = as.numeric(as.character(full_practice_data$Mid_Per))
full_practice_data$Three_Per = as.numeric(as.character(full_practice_data$Three_Per))
full_practice_data$FT_Per = as.numeric(as.character(full_practice_data$FT_Per))

full_practice_data %>%
  gt() %>%
  gt_highlight_rows(rows = c(2, 4, 6, 8, 10, 12, 14), fill = "lightgrey") %>%
  cols_label(url = "",
             Player = "Player",
             Pts = "Pts",
             Rim = "Rim",
             Rim_Per = "Rim%",
             Mid = "Mid",
             Mid_Per = "Mid%",
             Three = "3pt.",
             Three_Per = "3pt%",
             FT = "FT",
             FT_Per = "FT%",
             Off_Reb = "OReb.",
             Def_Reb = "DReb.",
             Reb = "Reb",
             Ast = "Ast",
             Stl = "Stl",
             Blk = "Blk",
             Tov = "Tov",
             Charges = "Charges",
             Seals = "Seals",
             Fouls = "Fouls",
             Successful_screen = "Good Screen Def.",
             Unsuccessful_screen = "Bad Screen Def."
  ) %>%
  tab_header(
    title = md("AIC Scrimmage"),
    subtitle = table_subtitle
  )  %>%
  text_transform(
    locations = cells_body(vars(url)),
    fn = function(x) {
      web_image(url = x,
                height = px(22.5))
    }
  ) %>%
  data_color(
    columns = c(Rim_Per),
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
    columns = c(Mid_Per),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(Three_Per),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(FT_Per),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_align(
    align = "left",
    columns = vars(url, Player, Pts, Rim, Rim_Per, Mid, Mid_Per, Three, Three_Per,FT, FT_Per,  Off_Reb, Def_Reb, Reb, Ast, Stl, Blk, Tov, Charges, Seals, Fouls, Successful_screen, Unsuccessful_screen)
  ) %>%
  cols_width(vars(url, Pts, Mid, Mid_Per, Off_Reb, Def_Reb, Reb, Ast, Stl, Blk,FT, FT_Per,  Tov, Seals, Fouls, Three_Per, Rim_Per) ~ px(35),
             vars(Player) ~ px(115),
             vars(Charges, Rim, Three) ~ px(45),
             vars(Successful_screen, Unsuccessful_screen) ~ px(45),
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
  gtsave(save_name)

#---------------------------------------------------------------------------------------------------------------------------------

basic_practice_data <- practice_data %>%
  mutate(Two = paste0(Rim_Make+Mid_Make, "/", Rim_Make+Rim_Miss+Mid_Make+Mid_Miss),
         Two_Per = round((Rim_Make+Mid_Make)/(Rim_Make+Rim_Miss+Mid_Make+Mid_Miss)*100,1),
         Reb = Off_Reb+Def_Reb,
         Three_Per = round(Three_Make/(Three_Make+Three_Miss)*100,1),
         Three = paste0(Three_Make, "/", Three_Make+Three_Miss),
         FT_Per = round(FTM/(FTA+FTM)*100,1),
         FT = (paste0(FTM, "/", FTM+FTA)),
  ) %>%
  select(url, Player, Pts, Reb, Ast, Two, Two_Per, Three, Three_Per, FT, FT_Per, Off_Reb, Def_Reb, Stl, Blk, Tov, Fouls)

basic_practice_data[is.na(basic_practice_data)] <- 0

totals_row <- c('https://upload.wikimedia.org/wikipedia/commons/thumb/2/23/UMass_Amherst_athletics_logo.svg/1200px-UMass_Amherst_athletics_logo.svg.png', "Totals", 
                sum(basic_practice_data$Pts), 
                sum(practice_data$Off_Reb)+sum(practice_data$Def_Reb), 
                sum(basic_practice_data$Ast), 
                paste0(sum(practice_data$Rim_Make)+sum(practice_data$Mid_Make), "/", sum(practice_data$Rim_Make)+sum(practice_data$Mid_Make)+sum(practice_data$Rim_Miss)+sum(practice_data$Mid_Miss)),
                round((sum(practice_data$Rim_Make)+sum(practice_data$Mid_Make))/(sum(practice_data$Rim_Make)+sum(practice_data$Mid_Make)+sum(practice_data$Rim_Miss)+sum(practice_data$Mid_Miss))*100,1),
                paste0(sum(practice_data$Three_Make), "/", sum(practice_data$Three_Make)+sum(practice_data$Three_Miss)),
                round(sum(practice_data$Three_Make)/(sum(practice_data$Three_Make)+sum(practice_data$Three_Miss))*100,1),
                paste0(sum(practice_data$FTM), "/", sum(practice_data$FTM)+sum(practice_data$FTA)),
                round(sum(practice_data$FTM)/(sum(practice_data$FTA)+sum(practice_data$FTM))*100,1),
                sum(basic_practice_data$Off_Reb), 
                sum(basic_practice_data$Def_Reb), 
                sum(basic_practice_data$Stl), 
                sum(basic_practice_data$Blk), 
                sum(basic_practice_data$Tov), 
                sum(basic_practice_data$Fouls)
)
basic_practice_data[nrow(basic_practice_data) + 1,] <- totals_row


save_name <- paste0(format(Sys.Date(), format="%B %d"), " - ", drill_name, " Basic Stat Sheet.png")

basic_practice_data %>%
  gt() %>%
  gt_highlight_rows(rows = c(2, 4, 6, 8, 10, 12, 14), fill = "lightgrey") %>%
  cols_label(url = "",
             Player = "Player",
             Pts = "Pts",
             Two = "2pt.",
             Two_Per = "2pt%",
             Three = "3pt.",
             Three_Per = "3pt%",
             FT = "FT",
             FT_Per = "FT%",
             Off_Reb = "OReb.",
             Def_Reb = "DReb.",
             Reb = "Reb",
             Ast = "Ast",
             Stl = "Stl",
             Blk = "Blk",
             Tov = "Tov",
             Fouls = "Fouls"
  ) %>%
  tab_header(
    title = md("Practice Stat Sheet"),
    subtitle = table_subtitle
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
    columns = vars(url, Player, Pts, Two, Two_Per, Three, Three_Per, FT, FT_Per, Off_Reb, Def_Reb, Reb, Ast, Stl, Blk, Tov, Fouls)
  ) %>%
  cols_width(vars(url, Pts, Two, Two_Per, Three, Three_Per, FT, FT_Per, Off_Reb, Def_Reb, Reb, Ast, Stl, Blk, Tov, Fouls) ~ px(38),
             vars(Player) ~ px(115),
             #vars(Reb, Ast) ~ px(45),
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
  gtsave(save_name)