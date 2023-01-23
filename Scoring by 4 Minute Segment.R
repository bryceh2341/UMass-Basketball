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

#data <- read.csv("NCAAD1_MBB_PBP_2022.csv")

theme_bryce <- function () { 
  theme_minimal(base_size=12, base_family="Consolas") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
    )
}

points_20_16_1 = 0
points_16_12_1 = 0
points_12_8_1 = 0
points_8_4_1 = 0
points_4_0_1 = 0
points_20_16_2 = 0
points_16_12_2 = 0
points_12_8_2 = 0
points_8_4_2 = 0
points_4_0_2 = 0

for (i in 1:length(data$GameID)) {
  if (data$secs_left_reg[i] >= 2160) {
    if (data$scoring_play[i] == "True") {
      if (data$home_score[i]-data$home_score[i-1] >= 0 && data$home_score[i]-data$home_score[i-1] <= 3 && data$away_score[i]-data$away_score[i-1] >= 0 && data$away_score[i]-data$away_score[i-1] <= 3) {
        points_20_16_1 = points_20_16_1 + data$home_score[i]-data$home_score[i-1] + data$away_score[i]-data$away_score[i-1]
      }
    }
  }
  
  if (data$secs_left_reg[i] < 2160 && 1920 <= data$secs_left_reg[i]) {
    if (data$scoring_play[i] == "True") {
      if (data$home_score[i]-data$home_score[i-1] >= 0 && data$home_score[i]-data$home_score[i-1] <= 3 && data$away_score[i]-data$away_score[i-1] >= 0 && data$away_score[i]-data$away_score[i-1] <= 3) {
        points_16_12_1 = points_16_12_1 + data$home_score[i]-data$home_score[i-1] + data$away_score[i]-data$away_score[i-1]
        }
    }
  }
  if (data$secs_left_reg[i] < 1920 && 1680 <= data$secs_left_reg[i]) {
    if (data$scoring_play[i] == "True") {
      if (data$home_score[i]-data$home_score[i-1] >= 0 && data$home_score[i]-data$home_score[i-1] <= 3 && data$away_score[i]-data$away_score[i-1] >= 0 && data$away_score[i]-data$away_score[i-1] <= 3) {
        points_12_8_1 = points_12_8_1 + data$home_score[i]-data$home_score[i-1] + data$away_score[i]-data$away_score[i-1]
      }
    }
  }
  if (data$secs_left_reg[i] < 1680 && 1440 <= data$secs_left_reg[i]) {
    if (data$scoring_play[i] == "True") {
      if (data$home_score[i]-data$home_score[i-1] >= 0 && data$home_score[i]-data$home_score[i-1] <= 3 && data$away_score[i]-data$away_score[i-1] >= 0 && data$away_score[i]-data$away_score[i-1] <= 3) {
        points_8_4_1 = points_8_4_1 + data$home_score[i]-data$home_score[i-1] + data$away_score[i]-data$away_score[i-1]
      }
    }
  }
  if (data$secs_left_reg[i] < 1440 && 1200 <= data$secs_left_reg[i]) {
    if (data$scoring_play[i] == "True") {
      if (data$home_score[i]-data$home_score[i-1] >= 0 && data$home_score[i]-data$home_score[i-1] <= 3 && data$away_score[i]-data$away_score[i-1] >= 0 && data$away_score[i]-data$away_score[i-1] <= 3) {
        points_4_0_1 = points_4_0_1 + data$home_score[i]-data$home_score[i-1] + data$away_score[i]-data$away_score[i-1]
      }
    }
  }
  
  if (data$secs_left_reg[i] < 1200 && 960 <= data$secs_left_reg[i]) {
    if (data$scoring_play[i] == "True") {
      if (data$home_score[i]-data$home_score[i-1] >= 0 && data$home_score[i]-data$home_score[i-1] <= 3 && data$away_score[i]-data$away_score[i-1] >= 0 && data$away_score[i]-data$away_score[i-1] <= 3) {
        points_20_16_2 = points_20_16_2 + data$home_score[i]-data$home_score[i-1] + data$away_score[i]-data$away_score[i-1]
      }
    }
  }
  
  if (data$secs_left_reg[i] < 960 && 720 <= data$secs_left_reg[i]) {
    if (data$scoring_play[i] == "True") {
      if (data$home_score[i]-data$home_score[i-1] >= 0 && data$home_score[i]-data$home_score[i-1] <= 3 && data$away_score[i]-data$away_score[i-1] >= 0 && data$away_score[i]-data$away_score[i-1] <= 3) {
        points_16_12_2 = points_16_12_2 + data$home_score[i]-data$home_score[i-1] + data$away_score[i]-data$away_score[i-1]
      }
    }
  }
  if (data$secs_left_reg[i] < 720 && 480 <= data$secs_left_reg[i]) {
    if (data$scoring_play[i] == "True") {
      if (data$home_score[i]-data$home_score[i-1] >= 0 && data$home_score[i]-data$home_score[i-1] <= 3 && data$away_score[i]-data$away_score[i-1] >= 0 && data$away_score[i]-data$away_score[i-1] <= 3) {
        points_12_8_2 = points_12_8_2 + data$home_score[i]-data$home_score[i-1] + data$away_score[i]-data$away_score[i-1]
      }
    }
  }
  if (data$secs_left_reg[i] < 480 && 240 <= data$secs_left_reg[i]) {
    if (data$scoring_play[i] == "True") {
      if (data$home_score[i]-data$home_score[i-1] >= 0 && data$home_score[i]-data$home_score[i-1] <= 3 && data$away_score[i]-data$away_score[i-1] >= 0 && data$away_score[i]-data$away_score[i-1] <= 3) {
        points_8_4_2 = points_8_4_2 + data$home_score[i]-data$home_score[i-1] + data$away_score[i]-data$away_score[i-1]
      }
    }
  }
  if (data$secs_left_reg[i] < 240 && 0 <= data$secs_left_reg[i]) {
    if (data$scoring_play[i] == "True") {
      if (data$home_score[i]-data$home_score[i-1] >= 0 && data$home_score[i]-data$home_score[i-1] <= 3 && data$away_score[i]-data$away_score[i-1] >= 0 && data$away_score[i]-data$away_score[i-1] <= 3) {
        points_4_0_2 = points_4_0_2 + data$home_score[i]-data$home_score[i-1] + data$away_score[i]-data$away_score[i-1]
      }
    }
  }
  
}

points_20_16_1 = round(points_20_16_1/length(unique(data$GameID))/2,1)
points_16_12_1 = round(points_16_12_1/length(unique(data$GameID))/2,1)
points_12_8_1 = round(points_12_8_1/length(unique(data$GameID))/2,1)
points_8_4_1 = round(points_8_4_1/length(unique(data$GameID))/2,1)
points_4_0_1 = round(points_4_0_1/length(unique(data$GameID))/2,1)

points_20_16_2 = round(points_20_16_2/length(unique(data$GameID))/2,1)
points_16_12_2 = round(points_16_12_2/length(unique(data$GameID))/2,1)
points_12_8_2 = round(points_12_8_2/length(unique(data$GameID))/2,1)
points_8_4_2 = round(points_8_4_2/length(unique(data$GameID))/2,1)
points_4_0_2 = round(points_4_0_2/length(unique(data$GameID))/2,1)

segment_data <- data.frame(Frame = c(1,2,3,4,5,6,7,8,9,10), Points = c(points_20_16_1,points_16_12_1,points_12_8_1,points_8_4_1,points_4_0_1,
                                                                  points_20_16_2,points_16_12_2,points_12_8_2,points_8_4_2,points_4_0_2))

ggplot(data = segment_data, aes(x = Frame, y = Points, fill=Points)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(low="#002B5E", mid="#00538C", high="#B8C4CA") +
  theme_bryce() +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10)) +
  scale_y_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10)) +
  theme(strip.text.x = element_blank(),
        panel.spacing.x = unit(1, "lines"),
        plot.title.position = 'plot',
        legend.position = "none",
        plot.title = element_text(face =  'bold', size = 12),
        plot.subtitle = element_text(size = 10),
        plot.margin = unit(c(.5, .5, 1, .5), "lines")) + 
  labs(x = "4-Minute Segment", 
       y = "Points Scored", 
       title = "Points Scored per 4 Minute Segment", 
       subtitle = "Do teams score less in the first 4 minutes?")
ggsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Segment Chart.png", width = 7, height = 5, dpi = 300, limitsize = F)
