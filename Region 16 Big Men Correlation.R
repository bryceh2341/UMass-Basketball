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
library(ggrepel)

setwd("C:/Users/Bryce Haase/Desktop/UMass Basketball")

data <- read.csv("Region 16 Big Men.csv")

theme_bryce <- function () {
  theme_minimal(base_size=12, base_family="Consolas") %+replace%
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
    )
}

ggplot(data, aes(PTSC, PTSJ)) +
  #geom_point(aes(fill = ppg, color = after_scale(clr_darken(fill, 0.3)))) +
  #annotate("text", label = "Yuri Collins", x = 10.6, y = 45, size = 5, colour = "blue", face = 'bold') +
  #geom_segment(aes(xend = 23.1, yend = 43.5, x = 30, y = 49),
  #             arrow = arrow(length = unit(0.5, "cm")), size=1.1, colour = "#881c1c") +
  #xlim(75.5, 81.5) +
  #ylim(76.75, 87.75) +
  theme_bryce() +
  geom_point(alpha = .8,
             size = 3,
             shape = 21,
             fill = 'red',
             show.legend = FALSE) +
  geom_smooth(method=lm, level = 0.95, col = 'black', alpha=.3) +
  geom_label_repel(aes(label = Name, size = NULL),
                   arrow = arrow(length = unit(0.03, "npc"),
                                 type = "closed", ends = "last"),
                   nudge_y = 0.3,
                   segment.size  = 0.3) +
  # geom_point(data = . %>% filter(player == stats$player[stats$player == "Zach Edey"]),
  #            aes(fill = "gold"),
  #            size = 3,
  #            shape = 21,
  #            show.legend = FALSE) +
  # geom_text_repel(data = . %>% filter(player == stats$player[stats$player == "Zach Edey"]),
  #                 aes(label = "Zach Edey", color = "gold"),
  #                 show.legend = FALSE,
  #                 fontface = 'bold',
  #                 family = "Consolas",
  #                 nudge_x = -.03,
  #                 nudge_y = 1.5) +
  theme(strip.text.x = element_blank(),
        panel.spacing.x = unit(1, "lines"),
        plot.title.position = 'plot',
        plot.title = element_text(face =  'bold', size = 15),
        plot.subtitle = element_text(size = 12),
        plot.margin = unit(c(.5, .5, 1, .5), "lines"),
        legend.position = "none") +
  labs(x = "College PPG",
       y = "JUCO PPG",
       title = "Scoring Translation",
       subtitle = "Region 16 Big Men to Transfer to Division 1")

ggsave("Region 16 Points.png", w = 6, h = 6, dpi = 300, type = 'cairo')

ggplot(data, aes(REBC, REBJ)) +
  #geom_point(aes(fill = ppg, color = after_scale(clr_darken(fill, 0.3)))) +
  #annotate("text", label = "Yuri Collins", x = 10.6, y = 45, size = 5, colour = "blue", face = 'bold') +
  #geom_segment(aes(xend = 23.1, yend = 43.5, x = 30, y = 49),
  #             arrow = arrow(length = unit(0.5, "cm")), size=1.1, colour = "#881c1c") +
  #xlim(75.5, 81.5) +
  #ylim(76.75, 87.75) +
  theme_bryce() +
  geom_point(alpha = .8,
             size = 3,
             shape = 21,
             fill = 'red',
             show.legend = FALSE) +
  geom_smooth(method=lm, level = 0.95, col = 'black', alpha=.3) +
  geom_label_repel(aes(label = Name, size = NULL),
                   arrow = arrow(length = unit(0.03, "npc"),
                                 type = "closed", ends = "last"),
                   nudge_y = 0.3,
                   segment.size  = 0.3) +
  # geom_point(data = . %>% filter(player == stats$player[stats$player == "Zach Edey"]),
  #            aes(fill = "gold"),
  #            size = 3,
  #            shape = 21,
  #            show.legend = FALSE) +
  # geom_text_repel(data = . %>% filter(player == stats$player[stats$player == "Zach Edey"]),
  #                 aes(label = "Zach Edey", color = "gold"),
  #                 show.legend = FALSE,
  #                 fontface = 'bold',
  #                 family = "Consolas",
  #                 nudge_x = -.03,
#                 nudge_y = 1.5) +
theme(strip.text.x = element_blank(),
      panel.spacing.x = unit(1, "lines"),
      plot.title.position = 'plot',
      plot.title = element_text(face =  'bold', size = 15),
      plot.subtitle = element_text(size = 12),
      plot.margin = unit(c(.5, .5, 1, .5), "lines"),
      legend.position = "none") +
  labs(x = "College RPG",
       y = "JUCO RPG",
       title = "Rebounding Translation",
       subtitle = "Region 16 Big Men to Transfer to Division 1")

ggsave("Region 16 Rebounds.png", w = 6, h = 6, dpi = 300, type = 'cairo')