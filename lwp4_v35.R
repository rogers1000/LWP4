
### NEED TO INVESTIGATE "ERRORS"

# Recent Changes
## SuccessRate includes LW and SR

#remove.packages("rlang")
#install.packages("rlang")

library(remotes)
library(nflverse)
library(tidyverse)
library(shiny)
library(gt)
library(stringr)
library(ffsimulator)
library(ffscrapr)
options(scipen = 9999)
.clear_cache()

nfl_season <- 2021

nfl_season_rosters <- 2022

position_data <- load_rosters(seasons = c(1999:nfl_season_rosters)) %>%
  select(gsis_id,season,position) %>%
  mutate(yearly_id = paste0(season,"_",gsis_id)) %>%
  unique() %>%
  mutate(fantasy_pos = ifelse(position == "QB","passer",position)) %>%
  mutate(fantasy_pos = ifelse(position == "RB" | position == "FB","rusher",fantasy_pos)) %>%
  mutate(fantasy_pos = ifelse(position == "WR" | position == "TE","receiver",fantasy_pos)) %>%
  filter(fantasy_pos == "passer" | fantasy_pos == "rusher" | fantasy_pos == "receiver") %>%
  select(yearly_id,fantasy_pos)

position_data_non_fantasy <- load_rosters(seasons = c(1999:nfl_season_rosters)) %>%
  select(gsis_id,season,position) %>%
  mutate(yearly_id = paste0(season,"_",gsis_id)) %>%
  unique() %>%
  mutate(fantasy_pos = ifelse(position == "QB","passer",position)) %>%
  mutate(fantasy_pos = ifelse(position == "RB" | position == "FB","rusher",fantasy_pos)) %>%
  mutate(fantasy_pos = ifelse(position == "WR" | position == "TE","receiver",fantasy_pos)) %>%
  filter(fantasy_pos == "passer" | fantasy_pos == "rusher" | fantasy_pos == "receiver") %>%
  select(yearly_id,position)

### Names for All seasons
name_data <- load_rosters(seasons = c(1999:nfl_season_rosters)) %>%
  group_by(gsis_id) %>%
  summarise(max_season = max(season)) %>%
  mutate(yearly_id = paste0(max_season,"_",gsis_id)) %>%
  select(yearly_id, max_season) 


name_data2 <- load_rosters(seasons = c(1999:nfl_season_rosters)) %>%
  select(gsis_id,season,full_name) %>%
  mutate(yearly_id = paste0(season,"_",gsis_id)) %>%
  left_join(name_data, by = "yearly_id") %>%
  filter(!is.na(max_season)) %>%
  select(gsis_id,full_name) 

teams_2022 <- load_rosters() %>%
  mutate(yearly_id = paste0(season,"_",gsis_id)) %>%
  select(yearly_id,team) %>%
  rename(team_2022 = team) 


#?rank

### Weekly Data

weekly_data <- load_player_stats(seasons = c(1999:nfl_season)) %>%
  filter(week < 18) %>%
  select(season,week,player_id,fantasy_points_ppr) %>%
  rename(points = fantasy_points_ppr) %>%
  mutate(yearly_id = paste0(season,"_",player_id)) %>%
  left_join(position_data, by = "yearly_id") %>%
  mutate(season_week = paste0(season,"_",week)) %>%
  group_by(season_week,season,week,fantasy_pos) %>%
  mutate(weekly_rank = rank(desc(points),ties.method = "min")) %>%
  arrange(season_week,fantasy_pos,weekly_rank) %>%
  ungroup() %>%
  mutate(season_week_player = paste0(season,"_",week,"_",player_id))

### Season Data

season_data <- weekly_data %>%
  mutate(thirty = ifelse(points > 29.99,1,0)) %>%
  mutate(twentyfive = ifelse(points > 24.99, 1,0)) %>%
  mutate(twenty = ifelse(points > 19.99,1,0)) %>%
  group_by(player_id,season,fantasy_pos) %>%
  summarise(PPR = sum(points), games30 = sum(thirty), games25 = sum(twentyfive), games20 = sum(twenty), gamesT10 = sum(ifelse(weekly_rank < 11,1,0))) %>%
  mutate(LWS = games30*2 + games25 + games20 + gamesT10) %>%
  #Q1 = 3x 30+ games
  mutate(Q1 = ifelse(games30 > 2,1,0)) %>%
  #Q2 = 4x 25+ games
  mutate(Q2 = ifelse(games25 > 3,1,0)) %>%
  #Q3 = 5x 20+ games
  mutate(Q3 = ifelse(games20 >4,1,0)) %>%
  #Q4 = 6x T10 games
  mutate(Q4 = ifelse(gamesT10 > 5,1,0)) %>%
  #Q5 = 2x 30+ / 3x 25+
  mutate(Q5 = ifelse(games30 > 1 & games25>2,1,0)) %>%
  #Q6 = 2x 30 / 4x 20+
  mutate(Q6 = ifelse(games30>1 & games20>3,1,0)) %>%
  #Q7 = 2x 30 / 5x T10
  mutate(Q7 = ifelse(games30 > 1 & gamesT10 > 4,1,0)) %>%
  #Q8 = 3X 25+ / 4X 20+
  mutate(Q8 = ifelse(games25 > 2 & games20 > 3,1,0)) %>%
  #Q9 = 3X 25 / 5X T10
  mutate(Q9 = ifelse(games25 >2 & gamesT10 > 4,1,0)) %>%
  #Q10 = 4X 20 / 5X T10
  mutate(Q10 = ifelse(games20 > 3 & gamesT10 > 4,1,0)) %>%
  mutate(Q_Count = Q1+Q2+Q3+Q4+Q5+Q6+Q7+Q8+Q9+Q10) %>%
  mutate(League_Winner = ifelse(Q_Count != 0,1,0)) %>%
  mutate(GA_Q0 = if_else(League_Winner == 1,0,3)) %>%
  mutate(GA_Q1a = if_else(games30 == 2, 1, 3)) %>%
  mutate(GA_Q1b = if_else(games30 == 1, 2, 3)) %>%
  mutate(GA_Q2a = if_else(games25 == 3, 1, 3)) %>%
  mutate(GA_Q2b = if_else(games25 == 2, 2, 3)) %>%
  mutate(GA_Q3a = if_else(games20 == 4, 1, 3)) %>%
  mutate(GA_Q3b = if_else(games20 == 3, 2, 3)) %>%
  mutate(GA_Q4a = if_else(gamesT10 == 5, 1, 3)) %>%
  mutate(GA_Q4b = if_else(gamesT10 == 4, 2, 3)) %>%
  mutate(GA_Q5a = if_else(games30 == 2 & games25 == 2, 1, 3)) %>%
  mutate(GA_Q5b = if_else(games30 == 2 & games25 == 1, 2, 3)) %>%
  mutate(GA_Q5c = if_else(games30 == 1 & games25 == 2, 1, 3)) %>%
  mutate(GA_Q5d = if_else(games30 == 1 & games25 == 1, 2, 3)) %>%
  mutate(GA_Q6a = if_else(games30 == 2 & games20 == 3, 1, 3)) %>%
  mutate(GA_Q6b = if_else(games30 == 2 & games20 == 2, 2, 3)) %>%
  mutate(GA_Q6c = if_else(games30 == 1 & games20 == 3, 1, 3)) %>%
  mutate(GA_Q6d = if_else(games30 == 1 & games20 == 2, 2, 3)) %>%
  mutate(GA_Q7a = if_else(games30 == 2 & gamesT10 == 4, 1, 3)) %>%
  mutate(GA_Q7b = if_else(games30 == 2 & gamesT10 == 3, 2, 3)) %>%
  mutate(GA_Q7c = if_else(games30 == 1 & gamesT10 == 4, 1, 3)) %>%
  mutate(GA_Q7d = if_else(games30 == 1 & gamesT10 == 3, 2, 3)) %>% 
  mutate(GA_Q8a = if_else(games25 == 3 & games20 == 3, 1, 3)) %>%
  mutate(GA_Q8b = if_else(games25 == 3 & games20 == 2, 2, 3)) %>%
  mutate(GA_Q8c = if_else(games25 == 2 & games20 == 3, 1, 3)) %>%
  mutate(GA_Q8d = if_else(games25 == 2 & games20 == 2, 2, 3)) %>%
  mutate(GA_Q9a = if_else(games25 == 3 & gamesT10 == 4, 1, 3)) %>%
  mutate(GA_Q9b = if_else(games25 == 3 & gamesT10 == 3, 2, 3)) %>%
  mutate(GA_Q9c = if_else(games25 == 2 & gamesT10 == 4, 1, 3)) %>%
  mutate(GA_Q9d = if_else(games25 == 2 & gamesT10 == 3, 2, 3)) %>%
  mutate(GA_Q10a = if_else(games20 == 4 & gamesT10 == 4, 1, 3)) %>%
  mutate(GA_Q10b = if_else(games20 == 4 & gamesT10 == 3, 2, 3)) %>%
  mutate(GA_Q10c = if_else(games20 == 3 & gamesT10 == 4, 1, 3)) %>%
  mutate(GA_Q10d = if_else(games20 == 3 & gamesT10 == 3, 2, 3)) %>%
  mutate(Games_Away = min(GA_Q0, GA_Q1a,GA_Q1b, GA_Q2a, GA_Q2b, GA_Q3a, GA_Q3b, GA_Q4a, GA_Q4b, GA_Q5a, GA_Q5b, GA_Q5c, GA_Q5d,GA_Q6a, GA_Q6b, GA_Q6c, GA_Q6d,GA_Q7a, GA_Q7b, GA_Q7c, GA_Q7d,GA_Q8a, GA_Q8b, GA_Q8c, GA_Q8d,GA_Q9a, GA_Q9b, GA_Q9c, GA_Q9d,GA_Q10a, GA_Q10b, GA_Q10c, GA_Q10d)) %>%
  mutate(yearly_id = paste0(season,"_",player_id)) %>%
  ungroup() %>%
  filter(!is.na(fantasy_pos)) %>%
  select(yearly_id,player_id,season,fantasy_pos,PPR,LWS,games30,games25,games20,gamesT10,League_Winner,Games_Away)

### Season Valid Check - Min 10 Games Played

PFR_ID_Mapping <- load_ff_playerids() %>%
  select(pfr_id, gsis_id) %>%
  rename(pfr_player_id = pfr_id)

## 2013 Onwards uses snap count data to work out if player played

season_data_games_played_2013_onwards <- load_snap_counts(seasons = c(2013:nfl_season)) %>%
  select(game_id,pfr_player_id,offense_snaps) %>%
  mutate(game = ifelse(offense_snaps > 0,1,0)) %>%
  left_join(PFR_ID_Mapping, by = "pfr_player_id") %>%
  separate(col = game_id,into = c("season","week","away","home"),sep = "_") %>%
  select(season,week,gsis_id,game) %>%
  mutate(yearly_id = paste0(season,"_",gsis_id)) %>%
  filter(week < 18) %>%
  group_by(yearly_id) %>%
  summarise(game = sum(game)) %>%
  select(yearly_id,game) 

## Before 2013 uses target data to work out if a player played

season_data_games_played_before2013 <- load_pbp(seasons = c(1999:nfl_season)) %>%
  select(season,week,fantasy_id) %>%
  filter(week < 18) %>%
  mutate(yearly_id_week = paste0(season,"_",fantasy_id,"_",week)) %>%
  select(yearly_id_week) %>%
  unique() %>%
  group_by(yearly_id_week) %>%
  summarise(count = 1) %>%
  ungroup() %>%
  unique() %>%
  separate(col = yearly_id_week, into = c("season","player_id","week"), sep = "_") %>%
  mutate(yearly_id = paste0(season,"_",player_id)) %>%
  select(yearly_id,count) %>%
  group_by(yearly_id) %>%
  summarise(game = sum(count))

season_data_games_played <- bind_rows(season_data_games_played_2013_onwards,season_data_games_played_before2013) %>%
  group_by(yearly_id) %>%
  summarise(game = game[1]) %>%
  ungroup()

season_data_games_played <- season_data_games_played %>%
  mutate(Games_Played_Invalid = ifelse(is.na(game) | game < 10,1,0)) %>%
  select(yearly_id,Games_Played_Invalid)

### Checking if a player changed teams midseason

season_data_team_weeks_2013onwards <- load_snap_counts(seasons = c(2013:nfl_season)) %>%
  select(game_id,pfr_player_id,team,offense_snaps) %>%
  left_join(PFR_ID_Mapping, by = "pfr_player_id") %>%
  separate(col = game_id,into = c("season","week","away","home"),sep = "_") %>%
  filter(offense_snaps > 0) %>%
  mutate(yearly_id_week = paste0(season,"_",gsis_id,"_",week)) %>%
  select(yearly_id_week,team) 

season_data_team_change_2013onwards <- load_snap_counts(seasons = c(2013:nfl_season)) %>%
  select(game_id,pfr_player_id,team,offense_snaps) %>%
  left_join(PFR_ID_Mapping, by = "pfr_player_id") %>%
  separate(col = game_id,into = c("season","week","away","home"),sep = "_") %>%
  filter(offense_snaps > 0, week <18, !is.na(gsis_id)) %>%
  group_by(gsis_id,season) %>%
  summarise(min_week = min(week), max_week = max(week)) %>%
  ungroup() %>%
  mutate(yearly_id = paste0(season,"_",gsis_id)) %>%
  select(yearly_id,min_week,max_week) %>%
  mutate(yearly_id_min = paste0(yearly_id,"_",min_week)) %>%
  mutate(yearly_id_max = paste0(yearly_id,"_",max_week)) %>%
  left_join(season_data_team_weeks_2013onwards, by = c("yearly_id_min" = "yearly_id_week")) %>%
  rename(min_week_team = team) %>%
  left_join(season_data_team_weeks_2013onwards, by = c("yearly_id_max" = "yearly_id_week")) %>%
  rename(max_week_team = team) %>%
  mutate(changed_team = ifelse(min_week_team == max_week_team,0,1)) %>%
  select(yearly_id,changed_team) %>%
  unique()

season_data_team_weeks_before2013 <- load_pbp(seasons = c(1999:nfl_season)) %>%
  select(season,week,fantasy_id,posteam) %>%
  mutate(yearly_id_week = paste0(season,"_",fantasy_id,"_",week)) %>%
  select(yearly_id_week,posteam)

season_data_team_change_before2013 <- load_pbp(seasons = c(1999:nfl_season)) %>%
  select(season,week,fantasy_id) %>%
  group_by(fantasy_id,season) %>%
  summarise(min_week = min(week), max_week = max(week)) %>%
  ungroup() %>%
  mutate(yearly_id = paste0(season,"_",fantasy_id)) %>%
  select(yearly_id,min_week,max_week) %>%
  mutate(yearly_id_min = paste0(yearly_id,"_",min_week)) %>%
  mutate(yearly_id_max = paste0(yearly_id,"_",max_week)) %>%
  left_join(season_data_team_weeks_before2013, by = c("yearly_id_min" = "yearly_id_week")) %>%
  rename(min_week_team = posteam) %>%
  left_join(season_data_team_weeks_before2013, by = c("yearly_id_max" = "yearly_id_week")) %>%
  rename(max_week_team = posteam) %>%
  mutate(changed_team = ifelse(min_week_team == max_week_team,0,1)) %>%
  select(yearly_id,changed_team) %>%
  unique()

season_data_team_change <- bind_rows(season_data_team_change_2013onwards,season_data_team_change_before2013) %>%
  group_by(yearly_id) %>%
  summarise(changed_team = changed_team[1]) %>%
  ungroup()

HC_db_1999_2020 <- read.csv(url("https://raw.githubusercontent.com/cooperdff/nfl_data_py/main/data/coaching_history.csv"))
HC_db_2021 <- read.csv(url("https://raw.githubusercontent.com/rogers1000/LWP4/main/2021_NFL_HC.csv"))
HC_db_2022 <- read.csv(url("https://raw.githubusercontent.com/rogers1000/LWP4/main/2022_nfl_hc.csv"))

HC_db <- bind_rows(HC_db_1999_2020,HC_db_2021,HC_db_2022) %>%
  mutate(team = ifelse(team == "GNB","GB",team)) %>%
  mutate(team = ifelse(team == "KAN","KC",team)) %>%
  mutate(team = ifelse(team == "NOR","NO",team)) %>%
  mutate(team = ifelse(team == "NWE","NE",team)) %>%
  mutate(team = ifelse(team == "LVR","LV",team)) %>%
  mutate(team = ifelse(team == "TAM","TB",team)) %>%
  mutate(team = ifelse(team == "SFO","SF",team)) %>%
  mutate(team = ifelse(team == "LAR","LA",team)) %>%
  mutate(team = ifelse(team == "OAK","LV",team)) %>%
  mutate(team = ifelse(team == "SDG","LAC",team)) %>%
  mutate(team = ifelse(team == "STL","LA",team))

SV_HC_Changes_lastyear <- HC_db %>%
  select(season,team,coach) %>%
  mutate(season_team = paste0(season,"_",team)) %>%
  rename(Last_Season_HC = coach) %>%
  select(season_team,Last_Season_HC) 

SV_HC_Changes_same_year <- HC_db %>%
  select(season,team,coach) %>%
  rename(Current_Season_HC = coach) %>%
  mutate(season_team = paste0(season,"_",team)) %>%
  mutate(last_year_season_team_id = paste0(season-1,"_",team)) %>%
  left_join(SV_HC_Changes_lastyear, by = c("last_year_season_team_id" = "season_team")) %>%
  filter(season > 1999) %>%
  select(season,team,season_team,Current_Season_HC,Last_Season_HC) %>%
  group_by(season_team) %>%
  summarise(coach_count = n()) %>%
  mutate(midseason_coach_change = ifelse(coach_count != 1,1,0)) %>%
  ungroup() %>%
  select(season_team, midseason_coach_change)

HC_change_midseason_player_validation_before2013 <- load_pbp(seasons = c(1999:nfl_season)) %>%
  select(season,fantasy_id,posteam) %>%
  rename(team = posteam) %>%
  mutate(team = ifelse(team == "GNB","GB",team)) %>%
  mutate(team = ifelse(team == "KAN","KC",team)) %>%
  mutate(team = ifelse(team == "NOR","NO",team)) %>%
  mutate(team = ifelse(team == "NWE","NE",team)) %>%
  mutate(team = ifelse(team == "LVR","LV",team)) %>%
  mutate(team = ifelse(team == "TAM","TB",team)) %>%
  mutate(team = ifelse(team == "SFO","SF",team)) %>%
  mutate(team = ifelse(team == "LAR","LA",team)) %>%
  mutate(team = ifelse(team == "OAK","LV",team)) %>%
  mutate(team = ifelse(team == "SDG","LAC",team)) %>%
  mutate(team = ifelse(team == "STL","LA",team)) %>%
  mutate(team = ifelse(team == "LARD","LV",team)) %>%
  mutate(team = ifelse(team == "SD","LAC",team)) %>%
  mutate(yearly_id = paste0(season,"_",fantasy_id)) %>%
  group_by(yearly_id,season) %>%
  summarise(team = team) %>%
  ungroup() %>%
  mutate(season_team = paste0(season,"_",team)) %>%
  unique() %>%
  left_join(SV_HC_Changes_same_year, by = "season_team") 

HC_change_midseason_player_validation_2013onwards <- load_snap_counts(seasons = c(2013:nfl_season)) %>%
  select(game_id,pfr_player_id,team,offense_snaps) %>%
  left_join(PFR_ID_Mapping, by = "pfr_player_id") %>%
  separate(col = game_id,into = c("season","week","away","home"),sep = "_") %>%
  filter(offense_snaps > 0) %>%
  mutate(team = ifelse(team == "GNB","GB",team)) %>%
  mutate(team = ifelse(team == "KAN","KC",team)) %>%
  mutate(team = ifelse(team == "NOR","NO",team)) %>%
  mutate(team = ifelse(team == "NWE","NE",team)) %>%
  mutate(team = ifelse(team == "LVR","LV",team)) %>%
  mutate(team = ifelse(team == "TAM","TB",team)) %>%
  mutate(team = ifelse(team == "SFO","SF",team)) %>%
  mutate(team = ifelse(team == "LAR","LA",team)) %>%
  mutate(team = ifelse(team == "OAK","LV",team)) %>%
  mutate(team = ifelse(team == "SDG","LAC",team)) %>%
  mutate(team = ifelse(team == "STL","LA",team)) %>%
  mutate(team = ifelse(team == "LARD","LV",team)) %>%
  mutate(team = ifelse(team == "SD","LAC",team)) %>%
  mutate(yearly_id = paste0(season,"_",gsis_id)) %>%
  mutate(season_team = paste0(season,"_",team)) %>%
  select(yearly_id,season_team) %>%
  unique() %>%
  left_join(SV_HC_Changes_same_year, by = "season_team") 

HC_change_midseason_player_validation_merged <- bind_rows(HC_change_midseason_player_validation_2013onwards,HC_change_midseason_player_validation_before2013) %>%
  filter(season > 1999) %>%
  group_by(yearly_id) %>%
  summarise(midseason_coach_change = midseason_coach_change[1]) %>%
  ungroup()

Valid_Check <- season_data %>%
  filter(season > 1999) %>%
  select(yearly_id) %>%
  unique() %>%
  left_join(season_data_games_played, by = "yearly_id") %>%
  left_join(season_data_team_change, by = "yearly_id") %>%
  left_join(HC_change_midseason_player_validation_merged, by = "yearly_id") %>%
  select(yearly_id,Games_Played_Invalid,changed_team,midseason_coach_change) %>%
  #group_by(yearly_id) %>%
  mutate(Season_Valid = ifelse(Games_Played_Invalid+changed_team+midseason_coach_change == 0,1,0))

season_data_valid <- season_data %>%
  left_join(Valid_Check, by = "yearly_id") %>%
  left_join(name_data2, by = c("player_id" = "gsis_id")) %>%
  arrange(-League_Winner,-LWS,-games30,-games25,-games20,-gamesT10,-Games_Away,-PPR) %>%
  mutate(PPR = round(PPR,2)) %>%
  filter(season > 1999) %>%
  print()

base_df_p1 <- season_data_valid %>%
  select(yearly_id,player_id,season,full_name,fantasy_pos,League_Winner,Season_Valid,Games_Away,LWS,PPR,games30,games25,games20,gamesT10) %>%
  mutate(Season_Valid = ifelse(is.na(Season_Valid),0,Season_Valid)) 

base_df_p2 <- base_df_p1 %>%
  head(1) %>%
  mutate(full_name = "Travis Etienne",
         League_Winner = 0,
         Season_Valid = 0,
         Games_Away = 3,
         LWS = 0,
         PPR = 0,
         games30 = 0,
         games25 = 0,
         games20 = 0,
         gamesT10  = 0,
         season = 2021,
         player_id = "00-0036973",
         yearly_id = "2021_00-0036973") %>%
  print()

missing_players <- read.csv("C:\\Users\\zacro\\Documents\\Apprenticeship Work\\League Winner Predicts\\LWP4.0\\missing_records_vector_pre2022.csv") %>%
  select(-X) %>%
  rename(yearly_id = x) %>%
  mutate(yearly_id2 = yearly_id) %>%
  separate(yearly_id2, into = c("season","player_id"), sep = "_") %>%
  left_join(name_data2, by = c("player_id" = "gsis_id")) %>%
  left_join(position_data, by = "yearly_id") %>%
  mutate(         League_Winner = 0,
                  Season_Valid = 0,
                  Games_Away = 3,
                  LWS = 0,
                  PPR = 0,
                  games30 = 0,
                  games25 = 0,
                  games20 = 0,
                  gamesT10  = 0) %>%
  mutate(season = as.double(season))

base_df <- bind_rows(missing_players,base_df_p1,base_df_p2)

league_winner_ever <- base_df %>%
  filter(League_Winner == 1) %>%
  group_by(player_id) %>%
  summarise(first_time = min(season)) %>%
  ungroup() 


drafted_when1 <- load_draft_picks() %>%
  left_join(PFR_ID_Mapping, by = c("pfr_id" = "pfr_player_id")) %>%
  mutate(yearly_id = paste0(season,"_",gsis_id)) %>%
  select(gsis_id,season,team,position,round) %>%
  mutate(team = ifelse(team == "GNB","GB",team)) %>%
  mutate(team = ifelse(team == "KAN","KC",team)) %>%
  mutate(team = ifelse(team == "NOR","NO",team)) %>%
  mutate(team = ifelse(team == "NWE","NE",team)) %>%
  mutate(team = ifelse(team == "LVR","LV",team)) %>%
  mutate(team = ifelse(team == "TAM","TB",team)) %>%
  mutate(team = ifelse(team == "SFO","SF",team)) %>%
  mutate(team = ifelse(team == "LAR","LA",team)) %>%
  mutate(team = ifelse(team == "OAK","LV",team)) %>%
  mutate(team = ifelse(team == "SDG","LAC",team)) %>%
  mutate(team = ifelse(team == "STL","LA",team)) %>%
  mutate(team = ifelse(team == "LARD","LV",team)) %>%
  mutate(team = ifelse(team == "SD","LAC",team)) %>%
  rename(draft_year = season) 

drafted_when2 <- load_ff_playerids() %>%
  select(gsis_id,draft_year,team,position,round = draft_round) %>%
  print()

drafted_when <- bind_rows(drafted_when1,drafted_when2) %>%
  group_by(gsis_id) %>%
  summarise(draft_year = draft_year[1]) %>%
  ungroup()

## PRESEASON HC CHECK

HC_Changes_preseason_change <- HC_db %>%
  select(season,team,coach) %>%
  rename(Current_Season_HC = coach) %>%
  mutate(season_team = paste0(season,"_",team)) %>%
  mutate(last_year_season_team_id = paste0(season-1,"_",team)) %>%
  left_join(SV_HC_Changes_lastyear, by = c("last_year_season_team_id" = "season_team")) %>%
  filter(season > 1999) %>%
  #mutate(HC_Change = ifelse(Current_Season_HC == Last_Season_HC,0,1)) %>%
  select(season,team,season_team,Current_Season_HC,Last_Season_HC) %>%
  group_by(season_team) %>%
  summarise(last_year_HC_midseasonchange = Last_Season_HC[2], last_year_HC_nochange = Last_Season_HC[1], current_year_HC = Current_Season_HC[1]) %>%
  mutate(lastyear_HC = ifelse(is.na(last_year_HC_midseasonchange),last_year_HC_nochange,last_year_HC_midseasonchange)) %>%
  mutate(Preseason_HC_Change = ifelse(current_year_HC == lastyear_HC,0,1)) %>%
  select(season_team,Preseason_HC_Change)

Players_Preseason_HC_Change_2022 <- teams_2022 %>%
  mutate(season = 2022) %>%
  rename(team = team_2022) %>%
  mutate(season_team = paste0("2022_",team))
  

Players_Preseason_HC_Change_upto_2021 <- load_pbp(seasons = c(1999:nfl_season)) %>%
  select(season,fantasy_id,posteam) %>%
  unique() %>%
  rename(team = posteam) %>%
  mutate(team = ifelse(team == "GNB","GB",team)) %>%
  mutate(team = ifelse(team == "KAN","KC",team)) %>%
  mutate(team = ifelse(team == "NOR","NO",team)) %>%
  mutate(team = ifelse(team == "NWE","NE",team)) %>%
  mutate(team = ifelse(team == "LVR","LV",team)) %>%
  mutate(team = ifelse(team == "TAM","TB",team)) %>%
  mutate(team = ifelse(team == "SFO","SF",team)) %>%
  mutate(team = ifelse(team == "LAR","LA",team)) %>%
  mutate(team = ifelse(team == "OAK","LV",team)) %>%
  mutate(team = ifelse(team == "SDG","LAC",team)) %>%
  mutate(team = ifelse(team == "STL","LA",team)) %>%
  mutate(team = ifelse(team == "LARD","LV",team)) %>%
  mutate(team = ifelse(team == "SD","LAC",team)) %>%
  mutate(yearly_id = paste0(season,"_",fantasy_id)) %>%
  group_by(yearly_id,season) %>%
  summarise(team = team) %>%
  ungroup() %>%
  mutate(season_team = paste0(season,"_",team)) %>%
  unique() %>%
  print()

Players_Preseason_HC_Change <- bind_rows(Players_Preseason_HC_Change_2022,Players_Preseason_HC_Change_upto_2021) %>%
  left_join(HC_Changes_preseason_change, by = "season_team") %>%
  filter(season > 1999) %>%
  select(yearly_id,Preseason_HC_Change) %>%
  group_by(yearly_id) %>%
  summarise(Preseason_HC_Change = sum(Preseason_HC_Change)) %>%
  ungroup() %>%
  mutate(Preseason_HC_Change = ifelse(Preseason_HC_Change > 0,1,0)) 

## PRESEASON PLAYER TEAM CHANGE CHECK

season_data_preseason_team_weeks_2013onwards <- load_snap_counts(seasons = c(2013:nfl_season)) %>%
  select(game_id,pfr_player_id,team,offense_snaps) %>%
  left_join(PFR_ID_Mapping, by = "pfr_player_id") %>%
  separate(col = game_id,into = c("season","week","away","home"),sep = "_") %>%
  filter(offense_snaps > 0) %>%
  mutate(yearly_id_week = paste0(season,"_",gsis_id,"_",week)) %>%
  select(yearly_id_week,season,team) 

preseason_team_change_2013onwards <- load_snap_counts(seasons = c(2013:nfl_season)) %>%
  select(game_id,pfr_player_id,team,offense_snaps) %>%
  left_join(PFR_ID_Mapping, by = "pfr_player_id") %>%
  separate(col = game_id,into = c("season","week","away","home"),sep = "_") %>%
  filter(offense_snaps > 0, week <18, !is.na(gsis_id)) %>%
  group_by(gsis_id,season) %>%
  summarise(min_week = min(week), max_week = max(week)) %>%
  ungroup() %>%
  mutate(yearly_id = paste0(season,"_",gsis_id)) %>%
  select(yearly_id,gsis_id,min_week,max_week) %>%
  mutate(yearly_id_min = paste0(yearly_id,"_",min_week)) %>%
  mutate(yearly_id_max = paste0(yearly_id,"_",max_week)) %>%
  left_join(season_data_preseason_team_weeks_2013onwards, by = c("yearly_id_min" = "yearly_id_week")) %>%
  rename(min_week_team = team) %>%
  left_join(season_data_preseason_team_weeks_2013onwards, by = c("yearly_id_max" = "yearly_id_week")) %>%
  rename(max_week_team = team) %>%
  mutate(changed_team = ifelse(min_week_team == max_week_team,0,1)) %>%
  unique() 

preseason_team_change_lastyear_2013onwards <- preseason_team_change_2013onwards %>%
  select(season.x,gsis_id,max_week_team) %>%
  rename(last_year_team = max_week_team) %>%
  rename(season_minus1 = season.x) %>%
  mutate(season_minus1 = as.numeric(season_minus1)) %>%
  mutate(yearly_id = paste0(season_minus1+1,"_",gsis_id)) %>%
  select(yearly_id,last_year_team) 

preseason_team_change_thisyear_2013onwards <- preseason_team_change_2013onwards %>%
  select(season.y,gsis_id,min_week_team) %>%
  rename(this_year_team = min_week_team) %>%
  rename(season = season.y) %>%
  mutate(yearly_id = paste0(season,"_",gsis_id)) %>%
  select(yearly_id,this_year_team) 

preseason_team_change_df_2013onwards <- preseason_team_change_lastyear_2013onwards %>%
  left_join(preseason_team_change_thisyear_2013onwards, by = "yearly_id") %>%
  left_join(teams_2022, by = "yearly_id") %>%
  mutate(this_year_team = ifelse(is.na(this_year_team),team_2022,this_year_team)) %>%
  mutate(preseason_team_change = ifelse(last_year_team == this_year_team,0,1)) %>%
  mutate(preseason_team_change = ifelse(is.na(preseason_team_change),0,preseason_team_change))

## PRE2013 PRESEASON PLAYER TEAM CHANGE CHECK


preseason_data_team_weeks_before2013 <- load_pbp(seasons = c(1999:nfl_season)) %>%
  select(season,week,fantasy_id,posteam) %>%
  mutate(yearly_id_week = paste0(season,"_",fantasy_id,"_",week)) %>%
  select(yearly_id_week,posteam)

preseason_data_team_change_before2013 <- load_pbp(seasons = c(1999:nfl_season)) %>%
  select(season,week,fantasy_id) %>%
  group_by(fantasy_id,season) %>%
  summarise(min_week = min(week), max_week = max(week)) %>%
  ungroup() %>%
  mutate(yearly_id = paste0(season,"_",fantasy_id)) %>%
  select(yearly_id,season,fantasy_id,min_week,max_week) %>%
  mutate(yearly_id_min = paste0(yearly_id,"_",min_week)) %>%
  mutate(yearly_id_max = paste0(yearly_id,"_",max_week)) %>%
  left_join(preseason_data_team_weeks_before2013, by = c("yearly_id_min" = "yearly_id_week")) %>%
  rename(min_week_team = posteam) %>%
  left_join(preseason_data_team_weeks_before2013, by = c("yearly_id_max" = "yearly_id_week")) %>%
  rename(max_week_team = posteam) %>%
  unique() 

preseason_team_change_last_year_pre2013 <- preseason_data_team_change_before2013 %>%
  select(season,fantasy_id,max_week_team) %>%
  rename(last_year_team = max_week_team) %>%
  mutate(yearly_id = paste0(season+1,"_",fantasy_id)) %>%
  select(yearly_id,last_year_team) 

preseason_team_change_this_year_pre2013 <- preseason_data_team_change_before2013 %>%
  select(yearly_id,min_week_team) %>%
  rename(this_year_team = min_week_team) %>%
  select(yearly_id,this_year_team) 

preseason_team_change_df_pre2013 <- preseason_team_change_last_year_pre2013 %>%
  left_join(preseason_team_change_this_year_pre2013, by = "yearly_id") %>%
  mutate(preseason_team_change = ifelse(last_year_team == this_year_team,0,1)) %>%
  mutate(preseason_team_change = ifelse(is.na(preseason_team_change),0,preseason_team_change))


preseason_team_change_df <- bind_rows(preseason_team_change_df_2013onwards,preseason_team_change_df_pre2013) %>%
  group_by(yearly_id) %>%
  summarise(preseason_team_change = preseason_team_change[1]) %>%
  ungroup() %>%
  print()

#### xyz

trades_df <- load_trades() %>%
  filter(is.na(pick_season), !is.na(pfr_id)) %>%
  select(season,received,pfr_id) %>%
  unique() %>%
  rename(pfr_player_id = pfr_id) %>%
  left_join(PFR_ID_Mapping, by = "pfr_player_id") %>%
  filter(!is.na(gsis_id)) %>%
  mutate(yearly_id = paste0(season,"_",gsis_id)) %>%
  left_join(name_data2, by = "gsis_id") %>%
  left_join(position_data, by = "yearly_id") %>%
  filter(!is.na(fantasy_pos)) %>%
  rename(team = received) %>%
  mutate(team = ifelse(team == "GNB","GB",team)) %>%
  mutate(team = ifelse(team == "KAN","KC",team)) %>%
  mutate(team = ifelse(team == "NOR","NO",team)) %>%
  mutate(team = ifelse(team == "NWE","NE",team)) %>%
  mutate(team = ifelse(team == "LVR","LV",team)) %>%
  mutate(team = ifelse(team == "TAM","TB",team)) %>%
  mutate(team = ifelse(team == "SFO","SF",team)) %>%
  mutate(team = ifelse(team == "LAR","LA",team)) %>%
  mutate(team = ifelse(team == "OAK","LV",team)) %>%
  mutate(team = ifelse(team == "SDG","LAC",team)) %>%
  mutate(team = ifelse(team == "STL","LA",team)) %>%
  mutate(team = ifelse(team == "LARD","LV",team)) %>%
  mutate(team = ifelse(team == "SD","LAC",team)) %>%
  view()

trades_df_players <- dplyr::pull(trades_df,yearly_id)

trades_df_teams_p1 <- trades_df %>%
  mutate(season_team_pos = paste0(season,"_",team,"_",fantasy_pos)) %>%
  mutate(team_traded = 1) %>%
  group_by(season_team_pos) %>%
  summarise(team_traded = sum(team_traded))

trades_df_teams_p2 <- trades_df %>%
  mutate(season_team_pos = paste0(season+1,"_",team,"_",fantasy_pos)) %>%
  mutate(team_traded = 1) %>%
  group_by(season_team_pos) %>%
  summarise(team_traded = sum(team_traded))

trades_df_teams_p3 <- bind_rows(trades_df_teams_p1,trades_df_teams_p2) %>%
  group_by(season_team_pos) %>%
  summarise(team_traded = sum(team_traded)) %>%
  mutate(team_traded = ifelse(team_traded > 0,1,0)) %>%
  print()

trades_df_season_team_pos <- bind_rows(preseason_team_change_df_2013onwards,preseason_team_change_df_pre2013) %>%
  group_by(yearly_id) %>%
  summarise(team = this_year_team[1]) %>%
  separate(yearly_id,into = c("season","player_id"),sep = "_") %>%
  mutate(yearly_id = paste0(season,"_",player_id)) %>%
  left_join(position_data, by = "yearly_id") %>%
  mutate(season_team_pos = paste0(season,"_",team,"_",fantasy_pos)) %>%
  unique() %>%
  left_join(trades_df_teams_p3, by = "season_team_pos") %>%
  mutate(team_traded = ifelse(is.na(team_traded),0,team_traded)) %>%
  select(yearly_id,team_traded) %>%
  mutate(team_traded = ifelse(yearly_id %in% trades_df_players,0,team_traded))
  
  
### TEAM DRAFTED POSITIONS, PLAYERS IMPACTED

Team_drafted_positions <- bind_rows(drafted_when1,drafted_when2) %>%
  filter(round < 4) %>%
  group_by(gsis_id) %>%
  summarise(draft_year = draft_year[1],position = position[1],team = team[1]) %>%
  ungroup() %>%
  filter(position == "QB" | position == "RB" | position == "WR" | position == "TE") %>%
  filter(draft_year > 1998, team != "FA") %>%
  group_by(draft_year,team,position) %>%
  summarise(count = n()) %>%
  mutate(season_team_pos = paste0(draft_year,"_",team,"_",position)) %>%
  mutate(season_team_pos_2 = paste0(draft_year+1,"_",team,"_",position))

Team_drafted_positions_part1 <- dplyr::pull(Team_drafted_positions,season_team_pos)
Team_drafted_positions_part2 <- dplyr::pull(Team_drafted_positions,season_team_pos_2)

Team_drafted_positions_part3 <- c(Team_drafted_positions_part1,Team_drafted_positions_part2)

Team_drafted_positions_part4 <- data.frame(season_team_pos = Team_drafted_positions_part3, count = 1) %>%
  group_by(season_team_pos) %>%
  summarise(count = n())

preseason_teams <- bind_rows(preseason_team_change_this_year_pre2013,preseason_team_change_thisyear_2013onwards) %>%
  group_by(yearly_id) %>%
  summarise(team = this_year_team[1]) %>%
  ungroup() %>%
  left_join(position_data_non_fantasy, by = "yearly_id") %>%
  filter(!is.na(position)) %>%
  separate(yearly_id,sep = "_", into = c("season","player_id")) %>%
  mutate(season_team_pos = paste0(season,"_",team,"_",position)) %>%
  mutate(yearly_id = paste0(season,"_",player_id)) %>%
  select(yearly_id,season_team_pos) %>%
  left_join(Team_drafted_positions_part4, by = "season_team_pos") %>%
  mutate(PA_Team_Drafted = ifelse(is.na(count),0,count)) %>%
  mutate(PA_Team_Drafted = ifelse(PA_Team_Drafted == 0,0,1)) %>%
  select(yearly_id,PA_Team_Drafted) 




Prediction_Rating <- base_df %>%
  mutate(BP_Plat = ifelse(League_Winner == 1 & Season_Valid == 1,1,0),
         BP_Gold = ifelse(League_Winner == 1 & Season_Valid == 0,1,0),
         BP_Silver = ifelse(League_Winner == 0 & Season_Valid == 0,1,0),
         BP_Bronze = ifelse(League_Winner == 0 & Season_Valid == 1,1,0)) %>%
  left_join(league_winner_ever, by = "player_id") %>%
  mutate(first_time = ifelse(is.na(first_time),3000,first_time)) %>%
  ### lw_ever shouldn't be here. Need to fix. Put elsewhere.
  mutate(lw_ever = ifelse(first_time <= season,1,0)) %>%
  #filter(is.na(Season_Valid)) %>%
  mutate(BP_Gold3 = ifelse(BP_Gold + lw_ever > 0,1,0)) %>%
  left_join(drafted_when, by = c("player_id" = "gsis_id")) %>%
  mutate(draft_year = ifelse(is.na(draft_year),1900,draft_year)) %>%
  mutate(drafted_within2 = ifelse(season - draft_year < 2,1,0)) %>%
  mutate(BP_Silver2 = BP_Silver + drafted_within2) %>%
  mutate(Base_Prediction = ifelse(BP_Bronze == 1,"Bronze",""),
         Base_Prediction = ifelse(BP_Silver2 > 0,"Silver",Base_Prediction),
         # Test to see how previous LW affects code: Base_Prediction = ifelse(BP_Gold3 == 1,"Gold",Base_Prediction),
         Base_Prediction = ifelse(BP_Gold == 1,"Gold",Base_Prediction),
         Base_Prediction = ifelse(BP_Plat == 1,"Platinum",Base_Prediction),) %>%
  mutate(PA_Breakout = ifelse(LWS > 4 & PPR > 99.99 & drafted_within2 == 1,1,0)) %>%
  mutate(PA_Breakout_Older = ifelse(LWS > 2 & PPR > 149.99,1,0)) %>%
  mutate(PA_SuccessRate = ifelse(Games_Away < 2,1,0)) %>%
  mutate(yearly_id = paste0(season+1,"_",player_id)) %>%
  mutate(lws_lastyear = LWS) %>%
  mutate(ppr_lastyear = PPR) %>%
  mutate(g30_lastyear = games30) %>%
  mutate(g25_lastyear = games25) %>%
  mutate(g20_lastyear = games20) %>%
  mutate(gT10_lastyear = gamesT10) %>%
  select(yearly_id,Base_Prediction,lw_ever,PA_Breakout,PA_Breakout_Older,PA_SuccessRate,lws_lastyear,ppr_lastyear,g30_lastyear,g25_lastyear,g20_lastyear,gT10_lastyear)

### ADDING 2022 DATA TO PREDICTIONS

base_df_2022 <- Prediction_Rating %>%
  select(yearly_id) %>%
  separate(yearly_id,into = c("season","player_id"), sep = "_") %>%
  filter(season == 2022) %>%
  mutate(yearly_id = paste0(season,"_",player_id)) %>%
  mutate(yearly_id_2021 = paste0("2021_",player_id)) %>%
  left_join(position_data, by = c(yearly_id_2021 = "yearly_id")) %>%
  mutate(League_Winner = 0,
         Season_Valid = 0,
         Games_Away = 3,
         LWS = 0,
         PPR = 0,
         games30 = 0,
         games25 = 0,
         games20 = 0,
         gamesT10 = 0) %>%
  left_join(name_data2, by = c("player_id" = "gsis_id")) %>%
  mutate(season = as.numeric(season)) %>%
  select(yearly_id,player_id,season,full_name,fantasy_pos,League_Winner,Season_Valid,Games_Away,LWS,PPR,games30,games25,games20,gamesT10)

base_df_2022_players <- dplyr::pull(base_df_2022,player_id)

base_df_2022_from_2020 <- Prediction_Rating %>%
  select(yearly_id) %>%
  separate(yearly_id,into = c("season","player_id"), sep = "_") %>%
  filter(season == 2021) %>%
  mutate(yearly_id = paste0(season,"_",player_id)) %>%
  mutate(yearly_id_2020 = paste0("2020_",player_id)) %>%
  left_join(position_data, by = c(yearly_id_2020 = "yearly_id")) %>%
  mutate(League_Winner = 0,
         Season_Valid = 0,
         Games_Away = 3,
         LWS = 0,
         PPR = 0,
         games30 = 0,
         games25 = 0,
         games20 = 0,
         gamesT10 = 0) %>%
  left_join(name_data2, by = c("player_id" = "gsis_id")) %>%
  mutate(season = as.numeric(season)) %>%
  select(yearly_id,player_id,season,full_name,fantasy_pos,League_Winner,Season_Valid,Games_Away,LWS,PPR,games30,games25,games20,gamesT10) %>%
  mutate(season = 2022) %>%
  mutate(yearly_id = paste0("2022_",player_id)) %>%
  filter(!(player_id %in% base_df_2022_players)) %>%
  print()

base_df_2022_etienne <- base_df_2022_from_2020 %>%
  head(1) %>%
  mutate(full_name = "Travis Etienne") %>%
  mutate(player_id = "00-0036973") %>%
  mutate(yearly_id = paste0("2022_",player_id)) %>%
  print()

base_df_2022_merged <- bind_rows(base_df,base_df_2022,base_df_2022_from_2020,base_df_2022_etienne) %>%
  view()

position_data_non_fantasy_2022 <- position_data_non_fantasy %>%
  separate(yearly_id,into = c("season","player_id"), sep = "_") %>%
  filter(season == 2021) %>%
  mutate(season = as.numeric(season)) %>%
  mutate(yearly_id = paste0(season+1,"_",player_id)) %>%
  select(yearly_id,position)

position_data_non_fantasy_2022_merged <- bind_rows(position_data_non_fantasy_2022,position_data_non_fantasy)

Prediction_Rating2 <- base_df_2022_merged %>%
  filter(season > 2000) %>%
  left_join(position_data_non_fantasy_2022_merged, by = c("yearly_id")) %>%
  left_join(Prediction_Rating, by = "yearly_id") %>%
  left_join(drafted_when, by = c("player_id" = "gsis_id")) %>%
  mutate(draft_year = ifelse(is.na(draft_year),1900,draft_year)) %>%
  mutate(Base_Prediction = ifelse(is.na(Base_Prediction) & season == draft_year,"Rookie",Base_Prediction)) %>%
  left_join(league_winner_ever, by = "player_id") %>%
  mutate(first_time = ifelse(is.na(first_time),3000,first_time)) %>%
  mutate(Base_Prediction = ifelse(is.na(Base_Prediction) & season != draft_year & first_time < season,"Gold",Base_Prediction)) %>%
  mutate(Base_Prediction = ifelse(is.na(Base_Prediction) & season != draft_year & first_time >= season,"Silver",Base_Prediction)) %>%
  left_join(Players_Preseason_HC_Change, by = "yearly_id") %>%
  left_join(preseason_team_change_df, by = "yearly_id") %>%
  mutate(PA_HC_Change = Preseason_HC_Change) %>%
  mutate(PA_HC_Change = ifelse(is.na(PA_HC_Change),0,PA_HC_Change)) %>%
  mutate(PA_Team_Change = preseason_team_change) %>%
  mutate(PA_Team_Change = ifelse(is.na(PA_Team_Change),0,PA_Team_Change)) %>%
  mutate(PA_Preseason_Change = PA_HC_Change + PA_Team_Change) %>%
  left_join(league_winner_ever, by = "player_id") %>%
  mutate(PA_Season_Skipped = ifelse(is.na(PA_Breakout),1,0),
         PA_Season_Skipped = ifelse(Base_Prediction == "Rookie" & PA_Season_Skipped == 1,0,PA_Season_Skipped)) %>%
  mutate(PA_Breakout = ifelse(is.na(PA_Breakout),0,PA_Breakout)) %>%
  mutate(PA_Breakout_Older = ifelse(is.na(PA_Breakout_Older),0,PA_Breakout_Older)) %>%
  mutate(PA_SuccessRate = ifelse(is.na(PA_SuccessRate),0,PA_SuccessRate)) %>%
  left_join(preseason_teams, by = "yearly_id") %>%
  mutate(PA_Team_Drafted = ifelse(is.na(PA_Team_Drafted),0,PA_Team_Drafted)) %>%
  left_join(trades_df_season_team_pos, by = "yearly_id") %>%
  mutate(team_traded = ifelse(is.na(team_traded),0,team_traded)) %>%
  ## REMOVING DRAFT INVESTMENT FOR SOPHOMORES. XGBOOST SHOULD BE ABLE TO DEAL WITH IT. 
  #mutate(PA_Team_Drafted = ifelse(PA_Team_Drafted == 1 & season-draft_year < 2,0,PA_Team_Drafted)) %>%
  mutate(PA_Preseason_Change = PA_HC_Change + PA_Team_Change + PA_Season_Skipped + PA_Team_Drafted) %>%
  mutate(B2S = PA_Preseason_Change) %>%
  mutate(S2G = PA_Breakout + PA_Breakout_Older + PA_SuccessRate) %>%
  mutate(P2G = PA_Preseason_Change) %>%
  mutate(Prediction_Rating = ifelse(Base_Prediction == "Bronze" & B2S > 0, "Silver", Base_Prediction),
         Prediction_Rating = ifelse(Base_Prediction == "Bronze" & S2G > 0, "Gold", Prediction_Rating),
         Prediction_Rating = ifelse(Base_Prediction == "Silver" & S2G > 0, "Gold", Prediction_Rating),
         Prediction_Rating = ifelse(Base_Prediction == "Platinum" & P2G > 0, "Gold", Prediction_Rating),
  ) %>%
  mutate(Success_Rate = ifelse(Games_Away < 2,1,0)) %>%
  mutate(experience = season - draft_year) %>%
  select(yearly_id,player_id,season,full_name,fantasy_pos,Prediction_Rating,League_Winner,Success_Rate,Season_Valid,Games_Away,LWS,PPR,games30,games25,games20,gamesT10,Base_Prediction,lw_ever,PA_Breakout,PA_Breakout_Older,PA_SuccessRate,PA_HC_Change,PA_Team_Change,PA_Season_Skipped,PA_Team_Drafted,team_traded,lws_lastyear,ppr_lastyear,g30_lastyear,g25_lastyear,g20_lastyear,gT10_lastyear,experience) %>%
  arrange(-League_Winner,Games_Away,-LWS,-games30,-games25,-games20,-gamesT10,-PPR) %>%
  mutate(Rank = row_number()) %>%
  select(Rank,yearly_id,player_id,season,full_name,fantasy_pos,Prediction_Rating,League_Winner,Success_Rate,Season_Valid,Games_Away,LWS,PPR,games30,games25,games20,gamesT10,Base_Prediction,lw_ever,PA_Breakout,PA_Breakout_Older,PA_SuccessRate,PA_HC_Change,PA_Team_Change,PA_Season_Skipped,PA_Team_Drafted,team_traded,lws_lastyear,ppr_lastyear,g30_lastyear,g25_lastyear,g20_lastyear,gT10_lastyear,experience)

Prediction_Rating3a <- Prediction_Rating2 %>%
  select(season,player_id,Prediction_Rating,Season_Valid) %>%
  mutate(PA_CarriedOver = ifelse(Prediction_Rating == "Platinum" & Season_Valid == 0,1,0),
         PA_CarriedOver = ifelse(Prediction_Rating == "Gold" & Season_Valid == 0,1,PA_CarriedOver)) %>%
  mutate(yearly_id = paste0(season+1,"_",player_id)) %>%
  select(yearly_id,PA_CarriedOver)

Prediction_Rating3b <- Prediction_Rating2 %>%
  select(season,player_id,Prediction_Rating,Season_Valid) %>%
  mutate(PA_CarriedOver = ifelse(Prediction_Rating == "Platinum" & Season_Valid == 0,1,0),
         PA_CarriedOver = ifelse(Prediction_Rating == "Gold" & Season_Valid == 0,1,PA_CarriedOver)) %>%
  mutate(yearly_id = paste0(season+2,"_",player_id)) %>%
  select(yearly_id,PA_CarriedOver) 

Prediction_Rating3c <- Prediction_Rating2 %>%
  select(season,player_id,Prediction_Rating,Season_Valid) %>%
  mutate(PA_CarriedOver = ifelse(Prediction_Rating == "Platinum" & Season_Valid == 0,1,0),
         PA_CarriedOver = ifelse(Prediction_Rating == "Gold" & Season_Valid == 0,1,PA_CarriedOver)) %>%
  mutate(yearly_id = paste0(season+3,"_",player_id)) %>%
  select(yearly_id,PA_CarriedOver) 

Prediction_Rating3_merged <- bind_rows(Prediction_Rating3a,Prediction_Rating3b,Prediction_Rating3c) %>%
  group_by(yearly_id) %>%
  summarise(PA_CarriedOver = PA_CarriedOver[1])

### ADP AND FANTASYPROS ECR 

mfl_gsis_mapping <- load_ff_playerids() %>%
  select(mfl_id,gsis_id) %>%
  print()

adp2021 <- read.csv("C:\\Users\\zacro\\Documents\\Apprenticeship Work\\League Winner Predicts\\LWP4.0\\mfl_adp\\2021_adp.csv") %>%
  rename(mfl_id = "X_id") %>%
  mutate(mfl_id = as.character(mfl_id)) %>%
  left_join(mfl_gsis_mapping, by = "mfl_id") %>%
  left_join(name_data2, by = "gsis_id") %>%
  rename(adp_averagePick = "X_averagePick") %>%
  mutate(adp_averagePick = as.numeric(adp_averagePick)) %>%
  mutate(yearly_id = paste0(season,"_",gsis_id)) %>%
  select(yearly_id,adp_averagePick) 

adp2020 <- read.csv("C:\\Users\\zacro\\Documents\\Apprenticeship Work\\League Winner Predicts\\LWP4.0\\mfl_adp\\2020_adp.csv") %>%
  rename(mfl_id = "X_id") %>%
  mutate(mfl_id = as.character(mfl_id)) %>%
  left_join(mfl_gsis_mapping, by = "mfl_id") %>%
  left_join(name_data2, by = "gsis_id") %>%
  rename(adp_averagePick = "X_averagePick") %>%
  mutate(adp_averagePick = as.numeric(adp_averagePick)) %>%
  mutate(yearly_id = paste0(season,"_",gsis_id)) %>%
  select(yearly_id,adp_averagePick)

adp2019 <- read.csv("C:\\Users\\zacro\\Documents\\Apprenticeship Work\\League Winner Predicts\\LWP4.0\\mfl_adp\\2019_adp.csv") %>%
  rename(mfl_id = "X_id") %>%
  mutate(mfl_id = as.character(mfl_id)) %>%
  left_join(mfl_gsis_mapping, by = "mfl_id") %>%
  left_join(name_data2, by = "gsis_id") %>%
  rename(adp_averagePick = "X_averagePick") %>%
  mutate(adp_averagePick = as.numeric(adp_averagePick)) %>%
  mutate(yearly_id = paste0(season,"_",gsis_id)) %>%
  select(yearly_id,adp_averagePick)

adp2018 <- read.csv("C:\\Users\\zacro\\Documents\\Apprenticeship Work\\League Winner Predicts\\LWP4.0\\mfl_adp\\2018_adp.csv") %>%
  rename(mfl_id = "X_id") %>%
  mutate(mfl_id = as.character(mfl_id)) %>%
  left_join(mfl_gsis_mapping, by = "mfl_id") %>%
  left_join(name_data2, by = "gsis_id") %>%
  rename(adp_averagePick = "X_averagePick") %>%
  mutate(adp_averagePick = as.numeric(adp_averagePick)) %>%
  mutate(yearly_id = paste0(season,"_",gsis_id)) %>%
  select(yearly_id,adp_averagePick)

adp2017 <- read.csv("C:\\Users\\zacro\\Documents\\Apprenticeship Work\\League Winner Predicts\\LWP4.0\\mfl_adp\\2017_adp.csv") %>%
  rename(mfl_id = "X_id") %>%
  mutate(mfl_id = as.character(mfl_id)) %>%
  left_join(mfl_gsis_mapping, by = "mfl_id") %>%
  left_join(name_data2, by = "gsis_id") %>%
  rename(adp_averagePick = "X_averagePick") %>%
  mutate(adp_averagePick = as.numeric(adp_averagePick)) %>%
  mutate(yearly_id = paste0(season,"_",gsis_id)) %>%
  select(yearly_id,adp_averagePick)

adp2016 <- read.csv("C:\\Users\\zacro\\Documents\\Apprenticeship Work\\League Winner Predicts\\LWP4.0\\mfl_adp\\2016_adp.csv") %>%
  rename(mfl_id = "X_id") %>%
  mutate(mfl_id = as.character(mfl_id)) %>%
  left_join(mfl_gsis_mapping, by = "mfl_id") %>%
  left_join(name_data2, by = "gsis_id") %>%
  rename(adp_averagePick = "X_averagePick") %>%
  mutate(adp_averagePick = as.numeric(adp_averagePick)) %>%
  mutate(yearly_id = paste0(season,"_",gsis_id)) %>%
  select(yearly_id,adp_averagePick)

adp2015 <- read.csv("C:\\Users\\zacro\\Documents\\Apprenticeship Work\\League Winner Predicts\\LWP4.0\\mfl_adp\\2015_adp.csv") %>%
  rename(mfl_id = "X_id") %>%
  mutate(mfl_id = as.character(mfl_id)) %>%
  left_join(mfl_gsis_mapping, by = "mfl_id") %>%
  left_join(name_data2, by = "gsis_id") %>%
  rename(adp_averagePick = "X_averagePick") %>%
  mutate(adp_averagePick = as.numeric(adp_averagePick)) %>%
  mutate(yearly_id = paste0(season,"_",gsis_id)) %>%
  select(yearly_id,adp_averagePick)

adp2014 <- read.csv("C:\\Users\\zacro\\Documents\\Apprenticeship Work\\League Winner Predicts\\LWP4.0\\mfl_adp\\2014_adp.csv") %>%
  rename(mfl_id = "X_id") %>%
  mutate(mfl_id = as.character(mfl_id)) %>%
  left_join(mfl_gsis_mapping, by = "mfl_id") %>%
  left_join(name_data2, by = "gsis_id") %>%
  rename(adp_averagePick = "X_averagePick") %>%
  mutate(adp_averagePick = as.numeric(adp_averagePick)) %>%
  mutate(yearly_id = paste0(season,"_",gsis_id)) %>%
  select(yearly_id,adp_averagePick)

adp2013 <- read.csv("C:\\Users\\zacro\\Documents\\Apprenticeship Work\\League Winner Predicts\\LWP4.0\\mfl_adp\\2013_adp.csv") %>%
  rename(mfl_id = "X_id") %>%
  mutate(mfl_id = as.character(mfl_id)) %>%
  left_join(mfl_gsis_mapping, by = "mfl_id") %>%
  left_join(name_data2, by = "gsis_id") %>%
  rename(adp_averagePick = "X_averagePick") %>%
  mutate(adp_averagePick = as.numeric(adp_averagePick)) %>%
  mutate(yearly_id = paste0(season,"_",gsis_id)) %>%
  select(yearly_id,adp_averagePick) 

adp2012 <- read.csv("C:\\Users\\zacro\\Documents\\Apprenticeship Work\\League Winner Predicts\\LWP4.0\\mfl_adp\\2012_adp.csv") %>%
  rename(mfl_id = "X_id") %>%
  mutate(mfl_id = as.character(mfl_id)) %>%
  left_join(mfl_gsis_mapping, by = "mfl_id") %>%
  left_join(name_data2, by = "gsis_id") %>%
  rename(adp_averagePick = "X_averagePick") %>%
  mutate(adp_averagePick = as.numeric(adp_averagePick)) %>%
  mutate(yearly_id = paste0(season,"_",gsis_id)) %>%
  select(yearly_id,adp_averagePick)

adp2011 <- read.csv("C:\\Users\\zacro\\Documents\\Apprenticeship Work\\League Winner Predicts\\LWP4.0\\mfl_adp\\2011_adp.csv") %>%
  rename(mfl_id = "X_id") %>%
  mutate(mfl_id = as.character(mfl_id)) %>%
  left_join(mfl_gsis_mapping, by = "mfl_id") %>%
  left_join(name_data2, by = "gsis_id") %>%
  rename(adp_averagePick = "X_averagePick") %>%
  mutate(adp_averagePick = as.numeric(adp_averagePick)) %>%
  mutate(yearly_id = paste0(season,"_",gsis_id)) %>%
  select(yearly_id,adp_averagePick)

adp2022_underdog <- read.csv("C:\\Users\\zacro\\Documents\\Apprenticeship Work\\League Winner Predicts\\LWP4.0\\mfl_adp\\2022_adp_underdogs.csv") %>%
  mutate(full_name = ifelse(full_name == "DK Metcalf","D.K. Metcalf",full_name)) %>%
  print()

adp2022 <- read.csv("C:\\Users\\zacro\\Documents\\Apprenticeship Work\\League Winner Predicts\\LWP4.0\\mfl_adp\\2022_adp.csv") %>%
  rename(mfl_id = "X_id") %>%
  mutate(mfl_id = as.character(mfl_id)) %>%
  left_join(mfl_gsis_mapping, by = "mfl_id") %>%
  left_join(name_data2, by = "gsis_id") %>%
  rename(adp_averagePick = "X_averagePick") %>%
  mutate(adp_averagePick = as.numeric(adp_averagePick)) %>%
  mutate(yearly_id = paste0(season,"_",gsis_id)) %>%
  left_join(adp2022_underdog, by = "full_name") %>%
  arrange(Rank) %>%
  mutate(rank_copy = Rank) %>%
  mutate(rank_test = row_number()) %>%
  mutate(ADP = ifelse(is.na(ADP),240.0,ADP)) %>%
  select(yearly_id,ADP) %>%
  rename(adp_averagePick = ADP)

adp2022_ffpc <- read.csv("C:\\Users\\zacro\\Documents\\Apprenticeship Work\\League Winner Predicts\\LWP4.0\\mfl_adp\\2022_adp_ffpc.csv") %>%
  left_join(name_data2, by = "full_name") %>%
  mutate(full_name = ifelse(full_name == "Kenneth Walker III","Kenneth Walker",full_name)) %>%
  mutate(full_name = ifelse(full_name == "John Metchie III","John Metchie",full_name)) %>%
  mutate(full_name = ifelse(full_name == "Pierre Strong Jr.","Pierre Strong",full_name)) %>%
  mutate(full_name = ifelse(full_name == "Calvin Austin III","Calvin Austin",full_name)) %>%
  #mutate(full_name = ifelse(full_name == "Kenneth Walker III","Dai'Jean Dixon",full_name)) %>%
  mutate(full_name = ifelse(full_name == "C.J. Verdell","CJ Verdell",full_name)) %>%
  mutate(full_name = ifelse(full_name == "Master Teague III","Master Teague",full_name)) %>%
  #mutate(full_name = ifelse(full_name == "Kenneth Walker III","Max Borghi",full_name)) %>%
  mutate(gsis_id = ifelse(is.na(gsis_id),full_name,gsis_id)) %>%
  mutate(yearly_id = paste0("2022_",gsis_id)) %>%
  select(yearly_id,ADP) %>%
  rename(adp_averagePick = ADP) %>%
  view()

adp_2011_2022 <- bind_rows(adp2011,adp2012,adp2013,adp2014,adp2015,adp2016,adp2017,adp2018,adp2019,adp2020,adp2021,adp2022_ffpc) %>%
  view()

adp_2011_2022_undrafted <- adp_2011_2022 %>%
  separate(yearly_id, into = c("season","player_id",sep = "_")) %>%
  group_by(season) %>%
  summarise(max_adp = max(adp_averagePick)) %>%
  ungroup() %>%
  mutate(adp_undrafted = max_adp+1) %>%
  mutate(adp_undrafted = ifelse(season == 2022, 350,adp_undrafted)) %>%
  select(season,adp_undrafted) %>%
  mutate(season = as.numeric(season)) %>%
  print()

### fp_historic_averagePicks 

fp_gsis_mapping <- load_ff_playerids() %>%
  select(fantasypros_id,gsis_id) %>%
  print()

fp_fpros_name_mapping <- load_ff_rankings() %>%
  head(10) %>%
  print()

fp_2011 <- ffpros::fp_rankings(page = "consensus-cheatsheets", year = c(2011)) %>%
  mutate(season = 2011) %>%
  left_join(fp_gsis_mapping, by = "fantasypros_id") %>%
  mutate(yearly_id = paste0(season,"_",gsis_id)) %>%
  select(yearly_id,rank,ecr)

fp_2012 <- ffpros::fp_rankings(page = "consensus-cheatsheets", year = c(2012)) %>%
  mutate(season = 2012) %>%
  left_join(fp_gsis_mapping, by = "fantasypros_id") %>%
  mutate(yearly_id = paste0(season,"_",gsis_id)) %>%
  select(yearly_id,rank,ecr)

fp_2013 <- ffpros::fp_rankings(page = "consensus-cheatsheets", year = c(2013)) %>%
  mutate(season = 2013) %>%
  left_join(fp_gsis_mapping, by = "fantasypros_id") %>%
  mutate(yearly_id = paste0(season,"_",gsis_id)) %>%
  select(yearly_id,rank,ecr)

fp_2014 <- ffpros::fp_rankings(page = "consensus-cheatsheets", year = c(2014)) %>%
  mutate(season = 2014) %>%
  left_join(fp_gsis_mapping, by = "fantasypros_id") %>%
  mutate(yearly_id = paste0(season,"_",gsis_id)) %>%
  select(yearly_id,rank,ecr)

fp_2015 <- ffpros::fp_rankings(page = "consensus-cheatsheets", year = c(2015)) %>%
  mutate(season = 2015) %>%
  left_join(fp_gsis_mapping, by = "fantasypros_id") %>%
  mutate(yearly_id = paste0(season,"_",gsis_id)) %>%
  select(yearly_id,rank,ecr)

fp_2016 <- ffpros::fp_rankings(page = "consensus-cheatsheets", year = c(2016)) %>%
  mutate(season = 2016) %>%
  left_join(fp_gsis_mapping, by = "fantasypros_id") %>%
  mutate(yearly_id = paste0(season,"_",gsis_id)) %>%
  select(yearly_id,rank,ecr)

fp_2017 <- ffpros::fp_rankings(page = "consensus-cheatsheets", year = c(2017)) %>%
  mutate(season = 2017) %>%
  left_join(fp_gsis_mapping, by = "fantasypros_id") %>%
  mutate(yearly_id = paste0(season,"_",gsis_id)) %>%
  select(yearly_id,rank,ecr)

fp_2018 <- ffpros::fp_rankings(page = "consensus-cheatsheets", year = c(2018)) %>%
  mutate(season = 2018) %>%
  left_join(fp_gsis_mapping, by = "fantasypros_id") %>%
  mutate(yearly_id = paste0(season,"_",gsis_id)) %>%
  select(yearly_id,rank,ecr)

fp_2019 <- ffpros::fp_rankings(page = "consensus-cheatsheets", year = c(2019)) %>%
  mutate(season = 2019) %>%
  left_join(fp_gsis_mapping, by = "fantasypros_id") %>%
  mutate(yearly_id = paste0(season,"_",gsis_id)) %>%
  select(yearly_id,rank,ecr)

fp_2020 <- ffpros::fp_rankings(page = "consensus-cheatsheets", year = c(2020)) %>%
  mutate(season = 2020) %>%
  left_join(fp_gsis_mapping, by = "fantasypros_id") %>%
  mutate(yearly_id = paste0(season,"_",gsis_id)) %>%
  select(yearly_id,rank,ecr)

fp_2021 <- ffpros::fp_rankings(page = "consensus-cheatsheets", year = c(2021)) %>%
  mutate(season = 2021) %>%
  left_join(fp_gsis_mapping, by = "fantasypros_id") %>%
  mutate(yearly_id = paste0(season,"_",gsis_id)) %>%
  select(yearly_id,rank,ecr)

fp_2022 <- load_ff_rankings() %>%
  filter(page_type == "redraft-overall") %>%
  mutate(season = 2022) %>%
  rename(fantasypros_id = id) %>%
  left_join(fp_gsis_mapping, by = "fantasypros_id") %>%
  mutate(yearly_id = paste0(season,"_",gsis_id)) %>%
  mutate(rank = row_number()) %>%
  select(yearly_id,rank,ecr) %>%
  print()

#fp_2022 <- read.csv("C:\\Users\\zacro\\Documents\\Apprenticeship Work\\League Winner Predicts\\LWP4.0\\mfl_adp\\2022_ecr_21_march.csv") %>%
#  rename(player_name = "PLAYER.NAME") %>%
#  rename(rank = RK) %>%
#  rename(ecr = AVG.) %>%
#  head(5) %>%
#  select(player_name,rank,ecr) %>%
#  print()

fp_ranks <- bind_rows(fp_2011,fp_2012,fp_2013,fp_2014,fp_2015,fp_2016,fp_2017,fp_2018,fp_2019,fp_2020,fp_2021,fp_2022) %>%
  print()

fp_ranks_unranked <- fp_ranks %>%
  separate(yearly_id, into = c("season","player_id",sep = "_")) %>%
  group_by(season) %>%
  summarise(max_averagePicked = max(ecr)) %>%
  ungroup() %>%
  mutate(ecr_unranked = max_averagePicked+1) %>%
  select(season,ecr_unranked) %>%
  mutate(season = as.numeric(season)) %>%
  print()

rookies_22_df <- read.csv("C:\\Users\\zacro\\Documents\\Apprenticeship Work\\League Winner Predicts\\LWP4.0\\df_rookie.csv") %>%
  select(-adp_averagePick) %>%
  left_join(adp2022_ffpc, by = "yearly_id") %>%
  view()

rookies_22_names <- dplyr::pull(rookies_22_df,name)

rookies_22_fp <- load_ff_rankings() %>%
  filter(page_type == "redraft-overall") %>%
  mutate(season = 2022) %>%
  rename(fantasypros_id = id) %>%
  left_join(fp_gsis_mapping, by = "fantasypros_id") %>%
  mutate(yearly_id = paste0(season,"_",gsis_id)) %>%
  mutate(rank = row_number()) %>%
  rename(name = player) %>%
  filter(name %in% rookies_22_names) %>%
  select(name,ecr) %>%
  print()

rookies_22_df_all <- rookies_22_df %>%
  left_join(rookies_22_fp, by = "name") %>%
  mutate(season = 2022) %>%
  left_join(adp_2011_2022_undrafted, by = "season") %>%
  left_join(fp_ranks_unranked, by = "season") %>%
  mutate(ecr = ifelse(is.na(ecr),ecr_unranked,ecr)) %>%
  mutate(Position_order = ifelse(position == "QB",1,""),
         Position_order = ifelse(position == "RB",2,Position_order),
         Position_order = ifelse(position == "WR",3,Position_order),
         Position_order = ifelse(position == "TE",4,Position_order),) %>%
  mutate(adp_averagePick = ifelse(is.na(adp_averagePick),350,adp_averagePick)) %>%
  view()

### ADDING EVERYTHING TOGETHER

Prediction_Rating4 <- Prediction_Rating2 %>%
  mutate(lw_ever = ifelse(is.na(lw_ever),0,lw_ever)) %>%
  left_join(Prediction_Rating3_merged, by = "yearly_id") %>%
  mutate(PA_CarriedOver = ifelse(is.na(PA_CarriedOver) & Prediction_Rating == "Rookie",0,PA_CarriedOver)) %>%
  mutate(PA_CarriedOver = ifelse(is.na(PA_CarriedOver),0,PA_CarriedOver)) %>%
  mutate(Prediction_Rating = ifelse(PA_CarriedOver == 1 & Prediction_Rating != "Platinum","Gold",Prediction_Rating)) %>%
  mutate(Prediction_Rating = ifelse(lw_ever == 1 & Prediction_Rating == "Bronze","Silver",Prediction_Rating)) %>%
  mutate(graph_col_lw = ifelse(Games_Away == 0,"#03C04A",""),
         graph_col_lw = ifelse(Games_Away == 1,"#FC6A03",graph_col_lw),
         graph_col_lw = ifelse(Games_Away > 1,"#900D09",graph_col_lw),) %>%
  mutate(League_Winner_Status = ifelse(League_Winner == 1,"Yes","No")) %>%
  mutate(Season_Valid_Status = ifelse(Season_Valid == 1,"Valid","Invalid")) %>%
  left_join(position_data_non_fantasy_2022_merged, by = "yearly_id") %>%
  mutate(LWP_rating_num = ifelse(Prediction_Rating == "Platinum",1,""),
         LWP_rating_num = ifelse(Prediction_Rating == "Gold",2,LWP_rating_num),
         LWP_rating_num = ifelse(Prediction_Rating == "Silver",3,LWP_rating_num),
         LWP_rating_num = ifelse(Prediction_Rating == "Bronze",4,LWP_rating_num),
         LWP_rating_num = ifelse(Prediction_Rating == "Rookie",5,LWP_rating_num),) %>%
  mutate(Position_order = ifelse(position == "QB",1,""),
         Position_order = ifelse(position == "RB",2,Position_order),
         Position_order = ifelse(position == "WR",3,Position_order),
         Position_order = ifelse(position == "TE",4,Position_order),) %>%
  mutate(lw_lastyear = ifelse(Base_Prediction == "Platinum" | Base_Prediction == "Gold",1,0)) %>%
  mutate(sv_lastyear = ifelse(Base_Prediction == "Platinum" | Base_Prediction == "Bronze",1,0)) %>%
  filter(season > 2001) %>%
  left_join(adp_2011_2022, by = "yearly_id") %>%
  left_join(adp_2011_2022_undrafted, by = "season") %>%
  mutate(adp_averagePick = ifelse(is.na(adp_averagePick),adp_undrafted,adp_averagePick)) %>%
  left_join(fp_ranks, by = "yearly_id") %>%
  left_join(fp_ranks_unranked, by = "season") %>%
  mutate(ecr = ifelse(is.na(ecr),ecr_unranked,ecr)) %>%
  mutate(LWS = ifelse(season == 2022,0,LWS)) %>%
  select(-Rank) %>%
  unique() %>%
  rename(fantasy_pos_correct = fantasy_pos) %>%
  mutate(yearly_id_year_before = paste0(season-1,"_",player_id)) %>%
  left_join(position_data, by = c("yearly_id_year_before" = "yearly_id")) %>%
  mutate(fantasy_pos_correct = ifelse(is.na(fantasy_pos_correct),fantasy_pos,fantasy_pos_correct)) %>%
  select(-fantasy_pos) %>%
  mutate(yearly_id_2years_before = paste0(season-2,"_",player_id)) %>%
  left_join(position_data, by = c("yearly_id_2years_before" = "yearly_id")) %>%
  mutate(fantasy_pos_correct = ifelse(is.na(fantasy_pos_correct),fantasy_pos,fantasy_pos_correct)) %>%
  mutate(fantasy_pos_correct = ifelse(is.na(fantasy_pos_correct),fantasy_pos,fantasy_pos_correct)) %>%
  select(-fantasy_pos) %>%
  rename(fantasy_pos = fantasy_pos_correct) %>%
  unique() %>%
  view()

missing_records <- Prediction_Rating4 %>%
  filter(PA_Season_Skipped == 1) %>%
  mutate(season = season - 1) %>%
  mutate(yearly_id = paste0(season,"_",player_id)) %>%
  select(yearly_id,season,player_id) %>%
  print()


write.csv(base_df_2022_merged,"C:\\Users\\zacro\\Documents\\Apprenticeship Work\\League Winner Predicts\\LWP4.0\\base_df_2022.csv")
write.csv(Prediction_Rating4,"C:\\Users\\zacro\\Documents\\Apprenticeship Work\\League Winner Predicts\\LWP4.0\\Prediction_Rating4_15th_March_1.csv")
write.csv(fp_ranks_unranked,"C:\\Users\\zacro\\Documents\\Apprenticeship Work\\League Winner Predicts\\LWP4.0\\fp_ranks_unranked.csv")
write.csv(adp_2011_2022_undrafted,"C:\\Users\\zacro\\Documents\\Apprenticeship Work\\League Winner Predicts\\LWP4.0\\adp_2011_2022_undrafted.csv")
write.csv(rookies_22_df_all,"C:\\Users\\zacro\\Documents\\Apprenticeship Work\\League Winner Predicts\\LWP4.0\\rookies_22_df_all.csv")
write.csv(missing_records,"C:\\Users\\zacro\\Documents\\Apprenticeship Work\\League Winner Predicts\\LWP4.0\\missing_records_vector_pre2022_2.csv")


