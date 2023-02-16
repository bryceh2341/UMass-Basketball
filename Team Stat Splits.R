library(dplyr)
library(tidyverse)
library(bigballR)
library(toRvik)
library(ncaahoopR)
library(gt)
library(cowplot)
library(ggpubr)
library(dplyr)
library(gtExtras)

# ncaa_game_id <- c(5387164)
# 
# play_by_play <- get_play_by_play(ncaa_game_id)
# play_by_play_first <- play_by_play[1:209,]
# play_by_play_second <- play_by_play[210:418,]

# team_stats_first <- get_team_stats(play_by_play_data = play_by_play_first, include_transition = T)
# team_stats_second <- get_team_stats(play_by_play_data = play_by_play_second, include_transition = T)

schedule <- get_team_schedule(team.name = "Massachusetts", season = "2022-23")

#no_wildens <- get_play_by_play(schedule$Game_ID[c(1:11,16)])
before_break <- get_play_by_play(schedule$Game_ID[c(13:25)])


#play_by_play <- get_play_by_play(ncaa_game_id)

before_stats <- get_team_stats(play_by_play_data = before_break, include_transition = T)

before_stats <- before_stats %>%
  filter(Team == "Massachusetts")

after_break <- get_play_by_play(schedule$Game_ID[c(18:25)])


#play_by_play <- get_play_by_play(ncaa_game_id)

after_stats <- get_team_stats(play_by_play_data = after_break, include_transition = T)

after_stats <- after_stats %>%
  filter(Team == "Massachusetts")


# print(sum(stats$dPTS)/sum(stats$dPOSS))*100
# print(((sum(stats$dFGM)-sum(stats$dTPM))+(1.5*sum(stats$dTPM)))/sum(stats$dFGA))*100
# print(sum(stats$dTO)/sum(stats$dPOSS))*100
# print(1-sum(stats$dDRB)/(sum(stats$dFGA)-sum(stats$dFGM)))*100
# print(sum(stats$dFTA)/sum(stats$dFGA))*100


# print(sum(before_stats$FGM)/sum(before_stats$FGA)*100)
# print(sum(before_stats$TPM)/sum(before_stats$TPA)*100)
# print(sum(before_stats$dFGM)/sum(before_stats$dFGA)*100)
# print(sum(before_stats$dTPM)/sum(before_stats$dTPA)*100)
# 
# print(sum(after_stats$FGM)/sum(after_stats$FGA)*100)
# print(sum(after_stats$TPM)/sum(after_stats$TPA)*100)
# print(sum(after_stats$dFGM)/sum(after_stats$dFGA)*100)
# print(sum(after_stats$dTPM)/sum(after_stats$dTPA)*100)

print(sum(before_stats$PTS)/sum(before_stats$POSS))
print(sum(after_stats$PTS)/sum(after_stats$POSS))