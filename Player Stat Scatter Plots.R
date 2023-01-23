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
         g >= 5,
         two_a >= 35,
         three_a <= 1
         ) %>%
  mutate(two_pct = two_pct*100)
  #select(player, ppg, ts)

theme_bryce <- function () {
  theme_minimal(base_size=12, base_family="Consolas") %+replace%
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
    )
}

ggplot(bart_player_stats, aes(two_pct, min)) +
  geom_point(size=3) +
  annotate("text", label = "Brandon Martin", x = 30, y = 50, size = 5, colour = "#881c1c", face = 'bold') +
  geom_segment(aes(xend = 23.1, yend = 43.5, x = 30, y = 49),
               arrow = arrow(length = unit(0.5, "cm")), size=1.1, colour = "#881c1c") +
  #xlim(75.5, 81.5) +
  #ylim(76.75, 87.75) +
  theme_bryce() +
  theme(strip.text.x = element_blank(),
        panel.spacing.x = unit(1, "lines"),
        plot.title.position = 'plot',
        plot.title = element_text(face =  'bold', size = 15),
        plot.subtitle = element_text(size = 12),
        plot.margin = unit(c(.5, .5, 1, .5), "lines")) +
  labs(x = "2-Point %",
       y = "% of Minutes Played",
       title = "2-Point % vs Minutes",
       subtitle = "Amongst players w/ 1 or less 3PA")

ggsave("Brandon Martin Two Shooting.png", w = 6, h = 6, dpi = 300, type = 'cairo')