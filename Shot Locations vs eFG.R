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

team_stats <- read.csv("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\2022-23 Team Shooting Data.csv")
team_stats <- team_stats %>%
  mutate(SQ = 60.2*(X..shots.at.rim/100) + 36.9*(X..shots.2pt.J/100) + 33.9*1.5*(X.of.shots.3pt/100))

theme_bryce <- function () {
  theme_minimal(base_size=12, base_family="Consolas") %+replace%
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
    )
}

team_stats %>% 
  ggplot(aes(x = eFG., 
             y = X..shots.2pt.J)) + 
  # geom_hline(yintercept = 0, 
  #            color = 'gray70') +
  geom_point(alpha = .1, 
             size = 3, 
             shape = 21, 
             fill = 'black', 
             show.legend = FALSE) + 
  geom_point(data = . %>% filter(Team == team_stats$Team[team_stats$Team == "Massachusetts"]), 
             aes(fill = "#881c1c"), 
             size = 3, 
             shape = 21, 
             show.legend = FALSE) + 
  geom_text_repel(data = . %>% filter(Team == team_stats$Team[team_stats$Team == "Massachusetts"]), 
                  aes(label = "UMass", color = "#881c1c"), 
                  show.legend = FALSE, 
                  fontface = 'bold', 
                  family = "Consolas", 
                  nudge_x = -.03, 
                  nudge_y = 1.5) + 
  # scale_x_continuous(labels = scales::percent_format(.1), 
  #                    breaks = seq(.2, .5, .1)) +
  # scale_y_continuous(breaks = seq(-40, 40, 10), 
  #                    limits = c(-40, 40)) +
  theme_bryce() + 
  labs(x = "Effective Field Goal %", 
       y = "% of Shots from Midrange", 
       title = "Effective Field Goal % vs. Midrange Shooting Volume", 
       subtitle = "Importance of Shot Selection") +
  theme(plot.title.position = 'plot', 
        plot.title = element_text(face = 'bold', size = 12), 
        plot.margin = margin(10, 10, 15, 10))

ggsave("Midrange Shooting Chart.png", w = 6, h = 6, dpi = 300)

team_stats %>% 
  ggplot(aes(x = eFG., 
             y = X..shots.at.rim)) + 
  # geom_hline(yintercept = 0, 
  #            color = 'gray70') +
  geom_point(alpha = .1, 
             size = 3, 
             shape = 21, 
             fill = 'black', 
             show.legend = FALSE) + 
  geom_point(data = . %>% filter(Team == team_stats$Team[team_stats$Team == "Massachusetts"]), 
             aes(fill = "#881c1c"), 
             size = 3, 
             shape = 21, 
             show.legend = FALSE) + 
  geom_text_repel(data = . %>% filter(Team == team_stats$Team[team_stats$Team == "Massachusetts"]), 
                  aes(label = "UMass", color = "#881c1c"), 
                  show.legend = FALSE, 
                  fontface = 'bold', 
                  family = "Consolas", 
                  nudge_x = -.03, 
                  nudge_y = 1.5) + 
  # scale_x_continuous(labels = scales::percent_format(.1), 
  #                    breaks = seq(.2, .5, .1)) +
  # scale_y_continuous(breaks = seq(-40, 40, 10), 
  #                    limits = c(-40, 40)) +
  theme_bryce() + 
  labs(x = "Effective Field Goal %", 
       y = "% of Shots from Rim", 
       title = "Effective Field Goal % vs. Rim Shooting Volume", 
       subtitle = "Importance of Rim Pressure") +
  theme(plot.title.position = 'plot', 
        plot.title = element_text(face = 'bold', size = 12), 
        plot.margin = margin(10, 10, 15, 10))

ggsave("Rim Shooting Chart.png", w = 6, h = 6, dpi = 300)

team_stats %>% 
  ggplot(aes(x = eFG., 
             y = X.of.shots.3pt)) + 
  # geom_hline(yintercept = 0, 
  #            color = 'gray70') +
  geom_point(alpha = .1, 
             size = 3, 
             shape = 21, 
             fill = 'black', 
             show.legend = FALSE) + 
  geom_point(data = . %>% filter(Team == team_stats$Team[team_stats$Team == "Massachusetts"]), 
             aes(fill = "#881c1c"), 
             size = 3, 
             shape = 21, 
             show.legend = FALSE) + 
  geom_text_repel(data = . %>% filter(Team == team_stats$Team[team_stats$Team == "Massachusetts"]), 
                  aes(label = "UMass", color = "#881c1c"), 
                  show.legend = FALSE, 
                  fontface = 'bold', 
                  family = "Consolas", 
                  nudge_x = -.03, 
                  nudge_y = 1.5) + 
  # scale_x_continuous(labels = scales::percent_format(.1), 
  #                    breaks = seq(.2, .5, .1)) +
  # scale_y_continuous(breaks = seq(-40, 40, 10), 
  #                    limits = c(-40, 40)) +
  theme_bryce() + 
  labs(x = "Effective Field Goal %", 
       y = "% of Shots from 3", 
       title = "Effective Field Goal % vs. 3pt. Shooting Volume", 
       subtitle = "Importance of Perimeter Shooting") +
  theme(plot.title.position = 'plot', 
        plot.title = element_text(face = 'bold', size = 12), 
        plot.margin = margin(10, 10, 15, 10))

ggsave("3pt Shooting Chart.png", w = 6, h = 6, dpi = 300)

team_stats %>% 
  ggplot(aes(x = eFG., 
             y = SQ)) + 
  # geom_hline(yintercept = 0, 
  #            color = 'gray70') +
  geom_point(alpha = .1, 
             size = 3, 
             shape = 21, 
             fill = 'black', 
             show.legend = FALSE) + 
  geom_point(data = . %>% filter(Team == team_stats$Team[team_stats$Team == "Massachusetts"]), 
             aes(fill = "#881c1c"), 
             size = 3, 
             shape = 21, 
             show.legend = FALSE) + 
  geom_text_repel(data = . %>% filter(Team == team_stats$Team[team_stats$Team == "Massachusetts"]), 
                  aes(label = "UMass", color = "#881c1c"), 
                  show.legend = FALSE, 
                  fontface = 'bold', 
                  family = "Consolas", 
                  nudge_x = -.03, 
                  nudge_y = 1.5) + 
  # scale_x_continuous(labels = scales::percent_format(.1), 
  #                    breaks = seq(.2, .5, .1)) +
  # scale_y_continuous(breaks = seq(-40, 40, 10), 
  #                    limits = c(-40, 40)) +
  theme_bryce() + 
  labs(x = "Effective Field Goal %", 
       y = "Shot Quality", 
       title = "Effective Field Goal % vs. Shot Quality", 
       subtitle = "Importance of Shot Selection") +
  theme(plot.title.position = 'plot', 
        plot.title = element_text(face = 'bold', size = 12), 
        plot.margin = margin(10, 10, 15, 10))

ggsave("Shot Quality Shooting Chart.png", w = 6, h = 6, dpi = 300)