library(toRvik)
library(tidyr)
library(tidyverse)
library(bigballR)
library(ncaahoopR)
library(gt)
library(cowplot)
library(ggpubr)
library(dplyr)
library(gtExtras)

team_stats <- bart_team_box(year=2022)
teams <- bart_teams(year = 2022, conf = NULL)

team.name = "Towson"

team_rating <- bart_ratings(year = 2022)

opp_team_rating <- team_rating %>%
  filter(team == team.name) %>%
  select(barthag_rk, adj_o_rk, adj_d_rk)

umass_team_rating <- team_rating %>%
  filter(team == "Massachusetts") %>%
  select(barthag_rk, adj_o_rk, adj_d_rk)

team_four_factors <- bart_factors(year=2022)

opp_four_factors <- team_four_factors %>%
  mutate(off_efg_rank = round(rank(desc(off_efg)),0), 
         off_to_rank = round(rank(off_to),0), 
         off_or_rank = round(rank(desc(off_or)),0), 
         off_ftr_rank = round(rank(desc(off_ftr)),0), 
         def_efg_rank = round(rank(def_efg),0), 
         def_to_rank = round(rank(desc(def_to)),0),
         def_or_rank = round(rank(def_or),0), 
         def_ftr_rank = round(rank(def_ftr),0)) %>%
  select(team, off_efg, off_efg_rank, off_to, off_to_rank, off_or, off_or_rank, off_ftr, off_ftr_rank, def_efg, def_efg_rank, def_to, def_to_rank, def_or, def_or_rank, def_ftr, def_ftr_rank) %>%
  filter(team == team.name)
opp_four_factors[nrow(opp_four_factors)+1,] <- list("Offense", opp_four_factors$off_efg[1], opp_four_factors$off_efg_rank[1], opp_four_factors$off_to[1], opp_four_factors$off_to_rank[1], opp_four_factors$off_or[1], opp_four_factors$off_or_rank[1], opp_four_factors$off_ftr[1], opp_four_factors$off_ftr_rank[1],0,0,0,0,0,0,0,0)
opp_four_factors[nrow(opp_four_factors)+1,] <- list("Defense", opp_four_factors$def_efg[1], opp_four_factors$def_efg_rank[1], opp_four_factors$def_to[1], opp_four_factors$def_to_rank[1], opp_four_factors$def_or[1], opp_four_factors$def_or_rank[1], opp_four_factors$def_ftr[1], opp_four_factors$def_ftr_rank[1],0,0,0,0,0,0,0,0)

umass_four_factors <- team_four_factors %>%
  mutate(off_efg_rank = round(rank(desc(off_efg)),0), 
         off_to_rank = round(rank(off_to),0), 
         off_or_rank = round(rank(desc(off_or)),0), 
         off_ftr_rank = round(rank(desc(off_ftr)),0), 
         def_efg_rank = round(rank(def_efg),0), 
         def_to_rank = round(rank(desc(def_to)),0),
         def_or_rank = round(rank(def_or),0), 
         def_ftr_rank = round(rank(def_ftr),0)) %>%
  select(team, off_efg, off_efg_rank, off_to, off_to_rank, off_or, off_or_rank, off_ftr, off_ftr_rank, def_efg, def_efg_rank, def_to, def_to_rank, def_or, def_or_rank, def_ftr, def_ftr_rank) %>%
  filter(team == "Massachusetts")
umass_four_factors[nrow(umass_four_factors)+1,] <- list("Offense", umass_four_factors$off_efg[1], umass_four_factors$off_efg_rank[1], umass_four_factors$off_to[1], umass_four_factors$off_to_rank[1], umass_four_factors$off_or[1], umass_four_factors$off_or_rank[1], umass_four_factors$off_ftr[1], umass_four_factors$off_ftr_rank[1],0,0,0,0,0,0,0,0)
umass_four_factors[nrow(umass_four_factors)+1,] <- list("Defense", umass_four_factors$def_efg[1], umass_four_factors$def_efg_rank[1], umass_four_factors$def_to[1], umass_four_factors$def_to_rank[1], umass_four_factors$def_or[1], umass_four_factors$def_or_rank[1], umass_four_factors$def_ftr[1], umass_four_factors$def_ftr_rank[1],0,0,0,0,0,0,0,0)


opp_four_factors <- opp_four_factors %>%
  slice(2:3) %>%
  select(team, off_efg_rank, off_to_rank, off_or_rank, off_ftr_rank)

umass_four_factors <- umass_four_factors %>%
  slice(2:3) %>%
  select(team, off_efg_rank, off_to_rank, off_or_rank, off_ftr_rank)



# full_comp <- data.frame(stat=c("Rank", "Offense", "Defense", "Off. eFG%", "Off. TOV", "Off. OReb", "Off. FT", "Def eFG%", "Def. TOV", "Def. OReb", "Def. FT"),
#                         UMass = c(umass_team_rating$barthag_rk, umass_team_rating$adj_o_rk, umass_team_rating$adj_d_rk, umass_four_factors$off_efg_rank[1], umass_four_factors$off_to_rank[1], umass_four_factors$off_or_rank[1], umass_four_factors$off_ftr_rank[1], umass_four_factors$off_efg_rank[2], umass_four_factors$off_to_rank[2], umass_four_factors$off_or_rank[2], umass_four_factors$off_ftr_rank[2]),
#                         Opp = c(opp_team_rating$barthag_rk, opp_team_rating$adj_o_rk, opp_team_rating$adj_d_rk, opp_four_factors$off_efg_rank[1], opp_four_factors$off_to_rank[1], opp_four_factors$off_or_rank[1], opp_four_factors$off_ftr_rank[1], opp_four_factors$off_efg_rank[2], opp_four_factors$off_to_rank[2], opp_four_factors$off_or_rank[2], opp_four_factors$off_ftr_rank[2]))

full_comp <- data.frame(stat=c("Rank", "Offense", "Defense", "Off. eFG%", "Def eFG%", "Off. TOV", "Def. TOV", "Off. OReb", "Def. OReb", "Off. FT", "Def. FT"),
                        UMass = c(umass_team_rating$barthag_rk, umass_team_rating$adj_o_rk, umass_team_rating$adj_d_rk, umass_four_factors$off_efg_rank[1], umass_four_factors$off_efg_rank[2], umass_four_factors$off_to_rank[1], umass_four_factors$off_to_rank[2], umass_four_factors$off_or_rank[1], umass_four_factors$off_or_rank[2], umass_four_factors$off_ftr_rank[1], umass_four_factors$off_ftr_rank[2]),
                        Opp = c(opp_team_rating$barthag_rk, opp_team_rating$adj_o_rk, opp_team_rating$adj_d_rk, opp_four_factors$off_efg_rank[1], opp_four_factors$off_efg_rank[2], opp_four_factors$off_to_rank[1], opp_four_factors$off_to_rank[2], opp_four_factors$off_or_rank[1], opp_four_factors$off_or_rank[2], opp_four_factors$off_ftr_rank[1], opp_four_factors$off_ftr_rank[2]))


full_comp %>%
  gt() %>%
  cols_label(stat = "Stat", UMass = "UMass", Opp = team.name
  ) %>%
  tab_header(
    title = md("Team Stats Comparison"),
    #subtitle = table_subtitle
  )  %>%
  data_color(
    columns = vars(UMass, Opp),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "RColorBrewer::RdYlGn",
        direction  = -1
      ) %>% as.character(),
      domain = c(358,0),
      na.color = "#00441BFF"
    )
  ) %>%
  cols_align(
    align = "left",
    columns = vars(stat)
  ) %>%
  cols_align(
    align = "center",
    columns = vars(UMass, Opp)
  ) %>%
  cols_width(vars(stat, UMass, Opp) ~ px(125),
             #vars(Player) ~ px(115),
             #vars(Charges, Rim, Three) ~ px(45),
             #vars(Successful_screen, Unsuccessful_screen) ~ px(45),
  ) %>%
  tab_style(
    style = list(
      cell_borders(
        side =  "top",
        color = 'gray55',
        weight = px(2)
      )
    ),
    locations = cells_body(
      rows = UMass == "League Average"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "floralwhite"),
    locations = cells_body(
      rows = UMass == "League Average")
  ) %>%
  tab_options(
    table.background.color = "floralwhite",
    column_labels.font.size = 10.5,
    table.font.size = 10,
    heading.title.font.size  = 15,
    heading.title.font.weight = 'bold',
    heading.subtitle.font.size = 11,
    table.font.names = "Consolas",
    table.font.color = 'black',
    #table.border.top.color = "transparent",
    data_row.padding = px(2),
    footnotes.font.size = 8,
    source_notes.font.size = 9,
    footnotes.padding = px(1),
    #column_labels.hidden = TRUE
  ) %>%
  gtsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\Team Comp.png", expand = 0)