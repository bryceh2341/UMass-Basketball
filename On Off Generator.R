#schedule <- get_team_schedule(team.name = "Massachusetts", season = "2022-23")

lineups <- get_lineups(play_by_play_data = get_play_by_play(game_ids = schedule$Game_ID[1:17]), include_transition = T)

on_off <- on_off_generator(Lineup_Data = lineups, Players = "NOAH.FERNANDES", include_transition = T)

on_off <- on_off %>%
  mutate()