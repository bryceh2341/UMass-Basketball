library(bigballR)
# setwd("C:/Users/Bryce Haase/Desktop/UMass Basketball")
# stats <- bart_player_season(year=2023, stat='all')
# write.csv(stats, "Season_Stats.csv")
# get_player_stats(play_by_play_data = get_play_by_play())
schedule <- get_team_schedule(team.name="Massachusetts", season="2022-23")
schedule <- schedule %>% filter(!is.na(Game_ID))
stats <- get_player_stats(play_by_play_data = get_play_by_play(schedule$Game_ID), multi.games = F, simple = F)
stats<-stats%>%filter(Player == "NOAH.FERNANDES")

stats <- stats %>%
  mutate(TRB = ORB+DRB) %>%
  select(Player, PTS, )