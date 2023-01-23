library(tidyverse)
library(extrafont)
library(jsonlite)
library(httr)
library(hablar)
library(janitor)
library(paletteer)
library(prismatic)
library(scales)
library(tidyverse)
library(nbastatR)
library(extrafont)
library(rvest)
library(ggforce)
library(ggbrace)
library(magick)
library(ggtext)
library(dplyr)
library(gt)
library(toRvik)
library(ggimage)

setwd("C:/Users/Bryce Haase/Desktop/UMass Basketball")

paint_touches <- read.csv("Total Paint Touches.csv")

UMass_paint_touches <- paint_touches %>%
  filter(Team == "UMass")

Opp_paint_touches <- paint_touches %>%
  filter(Team != "UMass")

UMass_paint_touches <- UMass_paint_touches %>%
  select(Opponent, Paint.Touch.., Paint.Touch.Score.., No.Paint.Touch.Score.., Off..Rtg., Pts.vs.Exp) %>%
  arrange(desc(Paint.Touch..))

UMass_paint_touches %>%
  gt() %>%
  cols_label(Opponent = "Opponent", 
             Paint.Touch.. = "Paint Touch %", 
             Paint.Touch.Score.. = "Paint Touch Score %", 
             No.Paint.Touch.Score.. = "No Paint Touch Score %",
             Off..Rtg. = "Off. Rtg.", 
             Pts.vs.Exp = "Pts. vs. Exp."
  ) %>%
  tab_header(
    title = md("UMass Paint Touches"),
    #subtitle = table_subtitle
  )  %>%
  # text_transform(
  #   locations = cells_body(vars(url)),
  #   fn = function(x) {
  #     web_image(url = x,
  #               height = px(22.5))
  #   }
  # ) %>%
  data_color(
    columns = c(Paint.Touch..),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(Paint.Touch.Score..),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(No.Paint.Touch.Score..),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(Off..Rtg.),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(Pts.vs.Exp),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_align(
    align = "left",
    columns = vars(Off..Rtg., Pts.vs.Exp)
  ) %>%
  cols_align(
    align = "center",
    columns = vars(Opponent, Paint.Touch.., Paint.Touch.Score.., No.Paint.Touch.Score..)
  ) %>%
  cols_width(vars(Opponent, Paint.Touch.., Paint.Touch.Score.., No.Paint.Touch.Score..) ~ px(105),
             vars(Off..Rtg., Pts.vs.Exp) ~ px(60),
             #vars(weight) ~ px(50),
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
      rows = Opponent == "League Average"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "floralwhite"),
    locations = cells_body(
      rows = Opponent == "League Average")
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
  gtsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\UMass Paint Touches.png", expand = 0)

Opp_paint_touches <- Opp_paint_touches %>%
  select(Team, Paint.Touch.., Paint.Touch.Score.., No.Paint.Touch.Score.., Off..Rtg., Pts.vs.Exp) %>%
  arrange(desc(Paint.Touch..))

Opp_paint_touches %>%
  gt() %>%
  cols_label(Team = "Opponent", 
             Paint.Touch.. = "Paint Touch %", 
             Paint.Touch.Score.. = "Paint Touch Score %", 
             No.Paint.Touch.Score.. = "No Paint Touch Score %",
             Off..Rtg. = "Off. Rtg.", 
             Pts.vs.Exp = "Pts. vs. Exp."
  ) %>%
  tab_header(
    title = md("UMass Paint Touches"),
    #subtitle = table_subtitle
  )  %>%
  # text_transform(
  #   locations = cells_body(vars(url)),
  #   fn = function(x) {
  #     web_image(url = x,
  #               height = px(22.5))
  #   }
  # ) %>%
  data_color(
    columns = c(Paint.Touch..),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(Paint.Touch.Score..),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(No.Paint.Touch.Score..),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(Off..Rtg.),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(Pts.vs.Exp),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_align(
    align = "left",
    columns = vars(Off..Rtg., Pts.vs.Exp)
  ) %>%
  cols_align(
    align = "center",
    columns = vars(Team, Paint.Touch.., Paint.Touch.Score.., No.Paint.Touch.Score..)
  ) %>%
  cols_width(vars(Team, Paint.Touch.., Paint.Touch.Score.., No.Paint.Touch.Score..) ~ px(105),
             vars(Off..Rtg., Pts.vs.Exp) ~ px(60),
             #vars(weight) ~ px(50),
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
      rows = Team == "League Average"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "floralwhite"),
    locations = cells_body(
      rows = Team == "League Average")
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
  gtsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Opponent Paint Touches.png", expand = 0)


# theme_bryce <- function () {
#   theme_minimal(base_size=12, base_family="Consolas") %+replace%
#     theme(
#       panel.grid.minor = element_blank(),
#       plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
#     )
# }
# 
# ggplot(UMass_paint_touches, aes(Paint.Touch.., Off..Rtg.)) +
#   geom_image(aes(image=url), size=0.09) +
#   geom_hline(yintercept=98.3,linetype=2) +
#   geom_vline(xintercept=62,linetype=2) +
#   geom_smooth(method=lm, level = 0.95, col = 'black') +
#   annotate("text", label = "Average Paint Touch %", x = 66, y = 105.5, size = 3, colour = "black", face = 'bold') +
#   annotate("text", label = "Average Offensive Rating", x = 72.5, y = 98.9, size = 3, colour = "black", face = 'bold') +
#   #geom_segment(aes(xend = 23.1, yend = 43.5, x = 30, y = 49),
#   #             arrow = arrow(length = unit(0.5, "cm")), size=1.1, colour = "#881c1c") +
#   #xlim(75.5, 81.5) +
#   #ylim(76.75, 87.75) +
#   theme_bryce() +
#   theme(strip.text.x = element_blank(),
#         panel.spacing.x = unit(1, "lines"),
#         plot.title.position = 'plot',
#         plot.title = element_text(face =  'bold', size = 15),
#         plot.subtitle = element_text(size = 12),
#         plot.margin = unit(c(.5, .5, 1, .5), "lines")) +
#   labs(x = "% of Possessions w/ Paint Touch",
#        y = "Offensive Rating",
#        title = "Paint Touches vs Offensive Rating",
#        subtitle = "UMass on OFFENSE this season")
# 
# ggsave("UMass Paint Touches vs Offensive Rating.png", w = 6, h = 6, dpi = 300, type = 'cairo')
# 
# ggplot(UMass_paint_touches, aes(Paint.Touch.., Pts.vs.Exp)) +
#   geom_image(aes(image=url), size=0.09) +
#   geom_hline(yintercept=0,linetype=2) +
#   geom_vline(xintercept=62,linetype=2) +
#   geom_smooth(method=lm, level = 0.95, col = 'black') +
#   annotate("text", label = "Average Paint Touch %", x = 65, y = 11.5, size = 3, colour = "black", face = 'bold') +
#   #annotate("text", label = "Average Offensive Rating", x = 72.5, y = 98.9, size = 3, colour = "black", face = 'bold') +
#   theme_bryce() +
#   theme(strip.text.x = element_blank(),
#         panel.spacing.x = unit(1, "lines"),
#         plot.title.position = 'plot',
#         plot.title = element_text(face =  'bold', size = 15),
#         plot.subtitle = element_text(size = 12),
#         plot.margin = unit(c(.5, .5, 1, .5), "lines")) +
#   labs(x = "% of Possessions w/ Paint Touch",
#        y = "Points vs Expected (Expected Pts - Pts Scored)",
#        title = "Paint Touches vs Points versus Expected",
#        subtitle = "UMass on OFFENSE this season")
# 
#   ggsave("UMass Paint Touches vs Pts vs Expected.png", w = 6, h = 6, dpi = 300, type = 'cairo')
# 
# ggplot(Opp_paint_touches, aes(Paint.Touch.., Off..Rtg.)) +
#   geom_image(aes(image=url), size=0.09) +
#   geom_hline(yintercept=92.6,linetype=2) +
#   geom_vline(xintercept=63,linetype=2) +
#   geom_smooth(method=lm, level = 0.95, col = 'black') +
#   annotate("text", label = "Average Paint Touch %", x = 66, y = 105.5, size = 3, colour = "black", face = 'bold') +
#   annotate("text", label = "Average Defensive Rating", x = 72.5, y = 93.5, size = 3, colour = "black", face = 'bold') +
#   #geom_segment(aes(xend = 23.1, yend = 43.5, x = 30, y = 49),
#   #             arrow = arrow(length = unit(0.5, "cm")), size=1.1, colour = "#881c1c") +
#   #xlim(75.5, 81.5) +
#   #ylim(76.75, 87.75) +
#   theme_bryce() +
#   theme(strip.text.x = element_blank(),
#         panel.spacing.x = unit(1, "lines"),
#         plot.title.position = 'plot',
#         plot.title = element_text(face =  'bold', size = 15),
#         plot.subtitle = element_text(size = 12),
#         plot.margin = unit(c(.5, .5, 1, .5), "lines")) +
#   labs(x = "% of Possessions w/ Paint Touch",
#        y = "Defensive Rating",
#        title = "Paint Touches vs Defensive Rating",
#        subtitle = "UMass on DEFENSE this season")
# 
# ggsave("UMass Paint Touches vs Defensive Rating.png", w = 6, h = 6, dpi = 300, type = 'cairo')
#   
#   ggplot(Opp_paint_touches, aes(Paint.Touch.., Pts.vs.Exp)) +
#     geom_image(aes(image=url), size=0.09) +
#     geom_hline(yintercept=0,linetype=2) +
#     geom_vline(xintercept=62,linetype=2) +
#     geom_smooth(method=lm, level = 0.95, col = 'black') +
#     annotate("text", label = "Average Paint Touch %", x = 65, y = 15.5, size = 3, colour = "black", face = 'bold') +
#     #annotate("text", label = "Average Offensive Rating", x = 72.5, y = 98.9, size = 3, colour = "black", face = 'bold') +
#     theme_bryce() +
#     theme(strip.text.x = element_blank(),
#           panel.spacing.x = unit(1, "lines"),
#           plot.title.position = 'plot',
#           plot.title = element_text(face =  'bold', size = 15),
#           plot.subtitle = element_text(size = 12),
#           plot.margin = unit(c(.5, .5, 1, .5), "lines")) +
#     labs(x = "% of Possessions w/ Paint Touch",
#          y = "Points Allowed vs Expected (Expected Pts - Pts Allowed)",
#          title = "Paint Touches vs Points Allowed versus Expected",
#          subtitle = "UMass on DEFENSE this season")
#   
#   ggsave("UMass Paint Touches vs Pts Allowed vs Expected.png", w = 6, h = 6, dpi = 300, type = 'cairo')