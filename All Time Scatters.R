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
library(scales)
library(prismatic)

setwd("C:/Users/Bryce Haase/Desktop/UMass Basketball")

years <- c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)

stats <- bart_player_season(year=2008, stat = 'all')

for (i in years) {
  bart_player_stats <- bart_player_season(year=i, stat = 'all')
  stats <- rbind(stats, bart_player_stats)
}

stats <- stats %>%
  filter(min > 40,
         fga >= 65)

# theme_bryce <- function () {
#   theme_minimal(base_size=12, base_family="Consolas") %+replace%
#     theme(
#       panel.grid.minor = element_blank(),
#       plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
#     )
# }
# 
# ggplot(stats, aes(apg, ast)) +
#   geom_point(aes(fill = apg, color = after_scale(clr_darken(fill, 0.3)))) +
#   #annotate("text", label = "Yuri Collins", x = 10.6, y = 45, size = 5, colour = "blue", face = 'bold') +
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
#         plot.margin = unit(c(.5, .5, 1, .5), "lines"),
#         legend.position = "none") +
#   labs(x = "Assists Per Game",
#        y = "Assist Rate",
#        title = "Assists per Game & Assist Rate",
#        subtitle = "Since 2007-08")
# 
# ggsave("All Time APG vs Ast.png", w = 6, h = 6, dpi = 300, type = 'cairo')