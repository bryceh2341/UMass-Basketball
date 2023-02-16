schedule <- get_team_schedule(team.name = "South Alabama", season = "2022-23")

lineups <- get_lineups(play_by_play_data = get_play_by_play(game_ids = schedule$Game_ID[!is.na(schedule$Game_ID)]), include_transition = T)

on_off <- on_off_generator(Lineup_Data = lineups, Players = "KEVIN.SAMUEL", include_transition = T)

on_off <- on_off %>%
  select(Status, dRIM., dRIMrate)

print(on_off$dRIM.[1]*100-on_off$dRIM.[2]*100)
print(on_off$dRIMrate[1]*100-on_off$dRIMrate[2]*100)