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
library(ggpubr)
library(hoopR)
library(bigballR)
library(ncaahoopR)
library(gt)
library(cowplot)
library(ggpubr)
library(ggforce)
library(ggrepel)

setwd("C:/Users/Bryce Haase/Desktop/UMass Basketball")

data <- read.csv("Naismith Defense Candidates.csv")

theme_bryce <- function () { 
  theme_minimal(base_size=12, base_family="Consolas") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
    )
}

ggplot(data, aes(x=Rank, y=DBPM)) +
  scale_x_reverse() +
  geom_point(size=3, color="red") +
  #geom_label(aes(label = sport, size = NULL), nudge_y = 3) +
  geom_label_repel(aes(label = Player, size = NULL),
                   arrow = arrow(length = unit(0.03, "npc"),
                                 type = "closed", ends = "last"),
                   nudge_y = 0.3,
                   segment.size  = 0.3) +
  theme_bryce() +
  theme(axis.text.x=element_text(face = "bold.italic", color = "black", size = 12),
        #axis.title.x=element_blank(),
        plot.title = element_text(size = 20, hjust = .5, color="black", face = "bold"),
        plot.subtitle = element_text(face = 'bold', size = 15, hjust = .5, color="black"),
        plot.caption = element_text(size = 10, hjust = .5, color="black"),
        axis.title = element_text(face = "bold.italic", color = "black"),
        axis.text.y = element_text(face = "bold.italic", color = "black", size = 12)) +
  labs(title = "Naismith DPOY Watch List",
       y = "Defensive Box Plus-Minus",
       x = "Team Defense Rank",
       subtitle = "",
       caption = "")
ggsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Naismith DPOY.png", width = 7, height = 5, dpi = 300, limitsize = F)