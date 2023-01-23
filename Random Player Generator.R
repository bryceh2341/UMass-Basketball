library(toRvik)

setwd("C:/Users/Bryce Haase/Desktop/UMass Basketball")

# players <- bart_player_season(year=2023, stat = 'all')
# players <- players %>%
#   filter(min >= 40) %>%
#   mutate(ppg = round(ppg,1),
#          apg = round(apg,1),
#          rpg = round(rpg,1)) %>%
#   select(player, team, ppg, rpg, apg)
# 
# write.csv(players, "C:/Users/Bryce Haase/Desktop/UMass Basketball/Rotation Players.csv")
# 
# players <- read.csv("C:/Users/Bryce Haase/Desktop/UMass Basketball/Rotation Players.csv")



#sample(players$player, 1)

Wins <- 0
Losses <- 0

for (i in 1:100) {
  number <- sample(1:length(players$player),1)
  player <- players$player[number]
  team <- players$team[number]
  ppg <- players$ppg[number]
  rpg <- players$rpg[number]
  apg <- players$apg[number]
  
  print(paste0("Player: ", player, " Stats: ", ppg, " PPG | ", rpg, " RPG | ", apg, " APG"))
  
  guess <- readline(prompt = 'Guess the Team : ')
  
  if (guess == team) {
    Wins <- Wins+1
    print("Correct!")
    print(paste0("Current Record: ", Wins, "/", Wins+Losses, " | ", round(Wins/(Wins+Losses)*100,1), "%"))
  } else {
    print(paste0("Incorrect | ", "Team: ", team))
    Losses <- Losses+1
    print(paste0("Current Record: ", Wins, "/", Wins+Losses, " | ", round(Wins/(Wins+Losses)*100,1), "%"))
    
  }
}

