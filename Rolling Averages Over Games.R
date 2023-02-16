library(dplyr)
library(RcppRoll)
library(ggplot2)

setwd("C:/Users/Bryce Haase/Desktop/UMass Basketball")

# theme_bryce <- function () {
#   theme_minimal(base_size=12, base_family="Consolas") %+replace%
#     theme(
#       panel.grid.minor = element_blank(),
#       plot.background = element_rect(fill = 'grey', color = "floralwhite")
#     )
# }

theme_bryce <- function () { 
  theme_minimal(base_size=12, base_family="Consolas") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
    )
}

BrockingtonLog <- read.csv(file = 'GameLog.csv')

BrockingtonLog <- BrockingtonLog %>%
  mutate(TotalPTS = cumsum(PTS), 
         TotalFGA = cumsum(FGA),
         TotalFTA = cumsum(FTA),
         Total3PM = cumsum(X3P),
         Total3PA = cumsum(X3PA),
         TotalMIN = cumsum(MP),
         RollingPTS = roll_sum(PTS, 3, align = "right", fill = 0),
         RollingFGA = roll_sum(FGA, 3, align = "right", fill = 0),
         RollingFTA = roll_sum(FTA, 3, align = "right", fill = 0),
         Rolling3PM = roll_sum(X3P, 3, align = "right", fill = 0),
         Rolling3PA = roll_sum(X3PA, 3, align = "right", fill = 0),
         RollingMIN = roll_sum(MP, 3, align = "right", fill = 0),
         TrueShooting = TotalPTS/(2*(TotalFGA+0.44*TotalFTA)),
         RollingTrueShooting = RollingPTS/(2*(RollingFGA+0.44*RollingFTA)),
         ThreePercentage = Total3PM/Total3PA*100,
         RollingThreePercentage = Rolling3PM/Rolling3PA*100,
         PP36 = TotalPTS/TotalMIN*36,
         RollingPP36 = RollingPTS/RollingMIN*36)



# ggplot(BrockingtonLog, aes(x=Rk)) +
#   geom_line(aes(y = RollingTrueShooting), color = "#00274C", linetype="dashed", size = 1.5) +
#   geom_line(aes(y = TrueShooting), color="#FFCB05", size = 1.5) +
#   theme_bryce() +
#   scale_y_continuous(limits=c(.3, .75)) +
#   theme(axis.text.x=element_text(face = "bold.italic", color = "white", size = 12),
#         #axis.title.x=element_blank(),
#         plot.title = element_text(size = 20, hjust = .5, color="white", face = "bold"),
#         plot.subtitle = element_text(face = 'bold', size = 15, hjust = .5, color="white"),
#         plot.caption = element_text(size = 10, hjust = .5, color="white"),
#         axis.title = element_text(face = "bold.italic", color = "white"),
#         axis.text.y = element_text(face = "bold.italic", color = "white", size = 12)) +
#   labs(title = "Caleb Houstan True Shooting Percentage",
#        y = "True Shooting Percentage",
#        x = "Game #")

ggplot(BrockingtonLog, aes(x=Rk)) +
  geom_line(aes(y = ThreePercentage), color = "red", size = 1.5) +
  geom_line(aes(y = RollingThreePercentage), color = "black", linetype="dashed", size = 1.5) +
  theme_bryce() +
  scale_y_continuous(limits=c(15, 60)) +
  scale_x_continuous(limits=c(3, 22)) +
  # theme(axis.text.x=element_text(face = "bold.italic", color = "white", size = 12),
  #       #axis.title.x=element_blank(),
  #       plot.title = element_text(size = 20, hjust = .5, color="white", face = "bold"),
  #       plot.subtitle = element_text(face = 'bold', size = 15, hjust = .5, color="white"),
  #       plot.caption = element_text(size = 10, hjust = .5, color="white"),
  #       axis.title = element_text(face = "bold.italic", color = "white"),
  #       axis.text.y = element_text(face = "bold.italic", color = "white", size = 12)) +
  theme(strip.text.x = element_blank(),
        panel.spacing.x = unit(1, "lines"),
        plot.title.position = 'plot',
        legend.position = "none",
        plot.title = element_text(face =  'bold', size = 15),
        plot.subtitle = element_text(size = 11),
        plot.margin = unit(c(.5, .5, 1, .5), "lines")) +
  labs(title = "Foster Loyer 3-Point Shooting Shooting",
       y = "3-Point Percentage",
       x = "Game #",
       subtitle = "Solid Line is Total Average | Dashed Line is 3 Game Rolling Average")
  ggsave("Loyer Shooting.png", w = 6, h = 6, dpi = 300)

# ggplot(BrockingtonLog, aes(x=Rk)) +
#   geom_line(aes(y = PP36), color = "#FFCB05", size = 1.5) +
#   geom_line(aes(y = RollingPP36), color = "#00274C", linetype="dashed", size = 1.5) +
#   theme_bryce() +
#   scale_y_continuous(limits=c(5, 20)) +
#   theme(axis.text.x=element_text(face = "bold.italic", color = "white", size = 12),
#         #axis.title.x=element_blank(),
#         plot.title = element_text(size = 20, hjust = .5, color="white", face = "bold"),
#         plot.subtitle = element_text(face = 'bold', size = 15, hjust = .5, color="white"),
#         plot.caption = element_text(size = 10, hjust = .5, color="white"),
#         axis.title = element_text(face = "bold.italic", color = "white"),
#         axis.text.y = element_text(face = "bold.italic", color = "white", size = 12)) +
#   labs(title = "Caleb Houstan Points per 36 Minutes",
#        y = "Points per 36 Minutes",
#        x = "Game #")


#expected_fg3_pct = (3PM + X*league_average_fg3_pct)/(3PA + X)