library(XML)
library(rvest)
library(stringr)
library(dplyr)
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
library(ggrepel)

# bart_player_stats <- bart_player_season(year=2023, stat = 'all')
# 
# bart_player_stats <- bart_player_stats %>%
#   filter(min >= 50,
#          g >= 5, 
#          ppg >= 20,
#          rpg >= 5,
#          apg >= 4)

#bart_player_stats$Rank<-rank(bart_player_stats$apg)

bart_player_stats <- bart_player_season(year=2023, stat = 'all')

bart_player_stats <- bart_player_stats %>%
  filter(min >= 40,
         fta >= 18,
  ) %>%
  mutate(ft_pct = 100*ft_pct)

setwd("C:/Users/Bryce Haase/Desktop/UMass Basketball")

theme_bryce <- function () {
  theme_minimal(base_size=12, base_family="Consolas") %+replace%
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
    )
}

ggplot(bart_player_stats, aes(x=pick, y=ft_pct)) + 
  geom_violin(trim=FALSE, fill='red', color="white", alpha = .5) +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=1, binwidth = 1, fill="red", alpha = 0.5) +
  geom_point(data = . %>% filter(player == bart_player_stats$player[bart_player_stats$player == "Rahsool Diggins"]),
             aes(fill = "red"),
             size = 1,
             shape = 23,
             show.legend = FALSE) +
  geom_text_repel(data = . %>% filter(player == bart_player_stats$player[bart_player_stats$player == "Rahsool Diggins"]),
                  aes(label = "Rahsool Diggins (16.7%)", color = "red"),
                  show.legend = FALSE,
                  color = "red",
                  fontface = 'bold',
                  family = "Consolas",
                  nudge_x = 0.1,
                  nudge_y = 1.5) +
  #scale_y_reverse() +
  #scale_y_continuous(breaks = seq(80, 120, by = 10)) +
  theme_bryce() + 
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        plot.title = element_text(size = 20, hjust = .5, color="black"),
        plot.subtitle = element_text(face = 'bold', size = 15, hjust = .5, color="black"),
        plot.caption = element_text(size = 10, hjust = .5, color="black"),
        axis.title = element_text(face = "bold.italic", color = "black"),
        axis.text.y = element_text(face = "bold.italic", color = "black", size = 12)) +
  labs(title = "Distribution of Free Throw Shooting",
       subtitle = "2022-23 Regular Season",
       y = "FT%",
       caption = "")

ggsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Diggins FT Shooting.png", width = 7, height = 5, dpi = 300, limitsize = F)