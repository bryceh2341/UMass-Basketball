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

team_stats <- read.csv("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Height vs Def Reb.csv")

theme_bryce <- function () {
  theme_minimal(base_size=12, base_family="Consolas") %+replace%
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
    )
}

ggplot(team_stats, aes(RankORPct, Height)) +
  scale_x_reverse() +
  scale_y_reverse() +
  #geom_image(aes(image=url), size=0.09) +
  #geom_hline(yintercept=98.3,linetype=2) +
  #geom_vline(xintercept=62,linetype=2) +
  #geom_smooth(method=lm, level = 0.65, col = 'black') +
  geom_point(alpha = .5, 
             size = 3, 
             shape = 21, 
             fill = 'black', 
             show.legend = FALSE) + 
  #annotate("text", label = "Average Paint Touch %", x = 66, y = 105.5, size = 3, colour = "black", face = 'bold') +
  #annotate("text", label = "Average Offensive Rating", x = 72.5, y = 98.9, size = 3, colour = "black", face = 'bold') +
  #geom_segment(aes(xend = 23.1, yend = 43.5, x = 30, y = 49),
  #             arrow = arrow(length = unit(0.5, "cm")), size=1.1, colour = "#881c1c") +
  #xlim(0, 100) +
  #ylim(0, 50) +
  theme_bryce() +
  theme(strip.text.x = element_blank(),
        panel.spacing.x = unit(1, "lines"),
        plot.title.position = 'plot',
        plot.title = element_text(face =  'bold', size = 15),
        plot.subtitle = element_text(size = 12),
        plot.margin = unit(c(.5, .5, 1, .5), "lines")) +
  labs(x = "Defensive Rebounding Rank",
       y = "Height Rank",
       title = "The Impact of Size on Defensive Rebounding",
       subtitle = "The Lack of Correlation Bewteen Team Height and Rebounding")

ggsave("Height vs Rebounding.png", w = 6, h = 6, dpi = 300, type = 'cairo')