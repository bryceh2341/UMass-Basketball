# teams <- teamids %>%
#   filter(Season == "2021-22")
# 
# schedule <- c()
#   
# for (i in 1:length(teams$Team)) {
#   team_schedule <- get_team_schedule(season = "2021-22", team.name = teams$Team[i])
#   schedule <- c(schedule, team_schedule$Game_ID)
# }
  
# unique_schedule <- unique(schedule)
# player_stats <- get_player_stats(play_by_play_data = get_play_by_play(unique_schedule), multi.games = T, simple = F)
# setwd("C:/Users/Bryce Haase/Desktop/UMass Basketball")
#write.csv(player_stats, "C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\2021_22_player_stats.csv", , row.names = TRUE)

full_player_stats <- read.csv("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\2021_22_player_stats.csv")

full_player_stats <- full_player_stats %>%
  filter(Team %in% teams$Team) %>%
  mutate(Player = gsub(".", " ", Player, fixed=TRUE),
     #per game basic
     MPG = round(MINS/GP,1),
     PPG = round(PTS/GP,1),
     APG = round(AST/GP,1),
     RPG = round((ORB+DRB)/GP,1),
     TOVPG = round(TOV/GP,1),
     BLKPG = round(BLK/GP,1),
     STLPG = round(STL/GP,1),
     TWOAPG = round((RIMA+MIDA)/GP,1),
     TWOMPG = round((RIMM+MIDM)/GP,1),
     THREEA = round(TPA/GP,1),
     THREEM = round(TPM/GP,1),
     TWOPCT = round(TWOMPG/TWOAPG*100,1),
     THREEPCT = round(THREEM/THREEA*100,1),
     FTAPG = round(FTA/GP,1),
     FTMPG = round(FTM/GP,1),
     FTPCT = round(FT.*100,1),
     ORPG = round(ORB/GP,1),
     DRPG = round(DRB/GP,1),
     #per game totals
     TWOA = round((RIMA+MIDA),1),
     TWOM = round((RIMM+MIDM),1),
     REB = ORB+DRB,
     #Per 40 stats
     PPMin = round(PTS/MINS*40,1),
     APMin = round(AST/MINS*40,1),
     ORPMin = round(ORB/MINS*40,1),
     DRPMin = round(DRB/MINS*40,1),
     RPMin = round((ORB+DRB)/MINS*40,1),
     TOVPMin = round(TOV/MINS*40,1),
     STLPMin = round(STL/MINS*40,1),
     BLKPMin = round(BLK/MINS*40,1),
     TWOAPM = round((RIMA+MIDA)/MINS*40,1),
     TWOMPM = round((RIMM+MIDM)/MINS*40,1),
     THREEAPM = round(TPA/MINS*40,1),
     THREEMPM = round(TPM/MINS*40,1),
     FTAPMin = round(FTA/MINS*40,1),
     FTMPMin = round(FTM/MINS*40,1),
     #Per 70 possessions
     PPPoss = round(PTS/oPOSS*70,1),
     APPoss = round(AST/oPOSS*70,1),
     ORPPoss = round(ORB/oPOSS*70,1),
     DRPPoss = round(DRB/oPOSS*70,1),
     RPPoss = round((ORB+DRB)/oPOSS*70,1),
     TOVPPoss = round(TOV/oPOSS*70,1),
     STLPPoss = round(STL/oPOSS*70,1),
     BLKPPoss = round(BLK/oPOSS*70,1),
     TWOAPPoss = round((RIMA+MIDA)/oPOSS*70,1),
     TWOMPPoss = round((RIMM+MIDM)/oPOSS*70,1),
     THREEAPPoss = round(TPA/oPOSS*70,1),
     THREEMPPoss = round(TPM/oPOSS*70,1),
     FTAPPoss = round(FTA/oPOSS*70,1),
     FTMPPoss = round(FTM/oPOSS*70,1),
     #Per game shooting
     FGMPP = round(FGM/GP,1),
     FGAPG = round(FGA/GP,1),
     RIMMPG = round(RIMM/GP,1),
     RIMAPG = round(RIMA/GP,1),
     MIDMPG = round(MIDM/GP,1),
     MIDAPG = round(MIDA/GP,1),
     #Per 40 minute shooting
     FGMPM = round(FGM/MINS*40,1),
     FGAPM = round(FGA/MINS*40,1),
     RIMMPM = round(RIMM/MINS*40,1),
     RIMAPM = round(RIMA/MINS*40,1),
     MIDMPM = round(MIDM/MINS*40,1),
     MIDAPM = round(MIDA/MINS*40,1),
     #per 70 shooting
     FGMPP = round(FGM/oPOSS*70,1),
     FGAPP = round(FGA/oPOSS*70,1),
     RIMMPP = round(RIMM/oPOSS*70,1),
     RIMAPP = round(RIMA/oPOSS*70,1),
     MIDMPP = round(MIDM/oPOSS*70,1),
     MIDAPP = round(MIDA/oPOSS*70,1),
     #Scoring per game
     FGACREATEDPG = round(FGA_unast/GP,1),
     FGMCREATEDPG = round(FGM_unast/GP,1),
     RIMACREATEDPG = round(RIMA_unast/GP,1),
     RIMMCREATEDPG = round(RIMM_unast/GP,1),
     MIDACREATEDPG = round(MIDA_unast/GP,1),
     MIDMCREATEDPG = round(MIDM_unast/GP,1),
     THREEACREATEDPG = round(TPA_unast/GP,1),
     THREEMCREATEDPG = round(TPM_unast/GP,1),
     PTSCREATEDPG = round(PTS_unast/GP,1),
     PTSASSISTEDPG = round(PTS_ast/GP,1),
     CREATIONPROPORTION = round(FGA_unast/FGA*100, 1),
     #Scoring per 40 minutes
     FGACREATEDPM = round(FGA_unast/MINS*40,1),
     FGMCREATEDPM = round(FGM_unast/MINS*40,1),
     RIMACREATEDPM = round(RIMA_unast/MINS*40,1),
     RIMMCREATEDPM = round(RIMM_unast/MINS*40,1),
     MIDACREATEDPM = round(MIDA_unast/MINS*40,1),
     MIDMCREATEDPM = round(MIDM_unast/MINS*40,1),
     THREEACREATEDPM = round(TPA_unast/MINS*40,1),
     THREEMCREATEDPM = round(TPM_unast/MINS*40,1),
     PTSCREATEDPM = round(PTS_unast/MINS*40,1),
     PTSASSISTEDPM = round(PTS_ast/MINS*40,1),
     #Scoring per 70 possessions
     FGACREATEDPP = round(FGA_unast/oPOSS*70,1),
     FGMCREATEDPP = round(FGM_unast/oPOSS*70,1),
     RIMACREATEDPP = round(RIMA_unast/oPOSS*70,1),
     RIMMCREATEDPP = round(RIMM_unast/oPOSS*70,1),
     MIDACREATEDPP = round(MIDA_unast/oPOSS*70,1),
     MIDMCREATEDPP = round(MIDM_unast/oPOSS*70,1),
     THREEACREATEDPP = round(TPA_unast/oPOSS*70,1),
     THREEMCREATEDPP = round(TPM_unast/oPOSS*70,1),
     PTSCREATEDPP = round(PTS_unast/oPOSS*70,1),
     PTSASSISTEDPP = round(PTS_ast/oPOSS*70,1),
     #Advanced per game
     PTSADDEDPG = round((((RIM.-mean(full_player_stats$RIM.[full_player_stats$RIMA>=20]))*RIMA + (MID.-mean(full_player_stats$MID.[full_player_stats$MIDA>=20]))*MIDA + (TP.-mean(full_player_stats$TP.[full_player_stats$TPA>=20]))*TPA) + (FT.-mean(full_player_stats$FT.[full_player_stats$FTA>=20]))*FTA)/GP,1),
     SHOTQUALITY = round((mean(full_player_stats$RIM.[full_player_stats$RIMA>=20])*RIMA/FGA + mean(full_player_stats$MID.[full_player_stats$MIDA>=20])*MIDA/FGA + mean(full_player_stats$TP.[full_player_stats$TPA>=20])*TPA/FGA*1.5)*100,1),
     SHOTMAKING = round((eFG.*100-SHOTQUALITY),1),
     CREATIONVOLUMEPG = round((FGA_unast+AST)/GP,1),
     TRANSITIONPROP = round(PTS_trans/PTS*100,1),
     HALFCOURTPROP = round(PTS_half/PTS*100,1),
     #Advanced total
     PTSADDED = round((((RIM.-mean(full_player_stats$RIM.[full_player_stats$RIMA>=20]))*RIMA + (MID.-mean(full_player_stats$MID.[full_player_stats$MIDA>=20]))*MIDA + (TP.-mean(full_player_stats$TP.[full_player_stats$TPA>=20]))*TPA) + (FT.-mean(full_player_stats$FT.[full_player_stats$FTA>=20]))*FTA),1),
     CREATIONVOLUME = round((FGA_unast+AST),0),
     #Advanced Per 40
     PTSADDEDPM = round((((RIM.-mean(full_player_stats$RIM.[full_player_stats$RIMA>=20]))*RIMA + (MID.-mean(full_player_stats$MID.[full_player_stats$MIDA>=20]))*MIDA + (TP.-mean(full_player_stats$TP.[full_player_stats$TPA>=20]))*TPA) + (FT.-mean(full_player_stats$FT.[full_player_stats$FTA>=20]))*FTA)/MINS*40,1),
     CREATIONVOLUMEPM = round((FGA_unast+AST)/MINS*40,1),
     #Advanced Per 70
     PTSADDEDPP = round((((RIM.-mean(full_player_stats$RIM.[full_player_stats$RIMA>=20]))*RIMA + (MID.-mean(full_player_stats$MID.[full_player_stats$MIDA>=20]))*MIDA + (TP.-mean(full_player_stats$TP.[full_player_stats$TPA>=20]))*TPA) + (FT.-mean(full_player_stats$FT.[full_player_stats$FTA>=20]))*FTA)/oPOSS*70,1),
     CREATIONVOLUMEPP = round((FGA_unast+AST)/oPOSS*70,1),
     )
full_player_stats[is.na(full_player_stats)] = 0

write.csv(full_player_stats, "C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\2021_22_full_player_stats.csv", row.names = TRUE)
