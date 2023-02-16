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

setwd("C:/Users/Bryce Haase/Desktop/UMass Basketball")

team <- "Davidson"

schedule <- get_team_schedule(team.name = team, season = "2022-23")

stats <- get_player_stats(play_by_play_data = get_play_by_play(schedule$Game_ID[!is.na(schedule$Game_ID)]), multi.games = F, simple = F)

data <- stats %>%
  filter(Player == "FOSTER.LOYER") 

data <- data%>%
  mutate(PTSx = PTS-mean(data$PTS))
            #%>%
  #select(PTS)
data$game <- c(1:length(data$PTS))

count <- 1
Points <- 0
Average <- c()

for (i in 1:length(data$game)) {
  Points <- (Points+data$PTS[i])
  Average <- c(Average, Points/count-mean(data$PTS))
  count <- count+1
}

theme_bryce <- function () { 
  theme_minimal(base_size=12, base_family="Consolas") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
    )
}

p <- ggplot(data = data, aes(x = game, y = PTSx, fill=PTS)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(low="#EE4B2B", mid="#AA4A44", high="#6E260E") +
  #scale_x_continuous(breaks = seq(1, 5, by = 1)) +
  #ylim(-10, 20) +
  theme_bryce() +
  theme(strip.text.x = element_blank(),
        panel.spacing.x = unit(1, "lines"),
        plot.title.position = 'plot',
        legend.position = "none",
        plot.title = element_text(face =  'bold', size = 15),
        plot.subtitle = element_text(size = 11),
        plot.margin = unit(c(.5, .5, 1, .5), "lines")) +
  labs(x = "Game #",
       y = "Points +/- Avg.",
       title = "Foster Loyer Scoring",
       subtitle = "Points vs. PPG Average By Game During the 2022-23 Season\nLine Represents Rolling Scoring Average")

p + geom_line(data=data, aes(x=game, y=Average), colour="black", size = 2)
ggsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Scoring by Game.png", width = 7, height = 5, dpi = 300, limitsize = F)