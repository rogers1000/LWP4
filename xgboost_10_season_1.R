

# TO DO LIST:
## FP POSITIONAL ECR
### 2021 DATA NOT FOUND, ASKED TAN
## Positional Value for Rankings
## Dynasty? 
### Predict 2 years ahead?


#install.packages("splitTools")
#install.packages("dials")
#install.packages("xgboost")
#install.packages("Ckmeans.1d.dp")
#install.packages("MLmetrics")

library(nflverse)
library(splitTools)
library(dials)
library(xgboost)
library(ggplot2)
library(tidyverse)
library(Ckmeans.1d.dp)
library(MLmetrics)

options(scipen = 9999)

fp_ranks_unranked <- read.csv("C:\\Users\\zacro\\Documents\\Apprenticeship Work\\League Winner Predicts\\LWP4.0\\fp_ranks_unranked.csv")

adp_2011_2022_undrafted <- read.csv("C:\\Users\\zacro\\Documents\\Apprenticeship Work\\League Winner Predicts\\LWP4.0\\adp_2011_2022_undrafted.csv")

base_df_xgboost <- read.csv("C:\\Users\\zacro\\Documents\\Apprenticeship Work\\League Winner Predicts\\LWP4.0\\base_df_2022.csv") %>%
  print()

rookies_22_df_all <- read.csv("C:\\Users\\zacro\\Documents\\Apprenticeship Work\\League Winner Predicts\\LWP4.0\\rookies_22_df_all.csv")

base_df_2022_rookies <- rookies_22_df_all

base_df_2022_with_rookies <- bind_rows(base_df_2022_rookies,base_df_xgboost)

position_data_non_fantasy_2022_merged_xgboost <- read.csv("C:\\Users\\zacro\\Documents\\Apprenticeship Work\\League Winner Predicts\\LWP4.0\\position_data_2022.csv")

positions_2022_rookies <- rookies_22_df_all %>%
  select(yearly_id,position)

position_data_non_fantasy_2022_merged_rookies <- bind_rows(position_data_non_fantasy_2022_merged_xgboost,positions_2022_rookies)

df_all <- read.csv("C:\\Users\\zacro\\Documents\\Apprenticeship Work\\League Winner Predicts\\LWP4.0\\Prediction_Rating4_15th_March_1.csv") %>%
  rename(label = League_Winner) %>%
  select(season,yearly_id,label,Position_order,lws_lastyear,ppr_lastyear,lw_lastyear,sv_lastyear,lw_ever,PA_SuccessRate,PA_HC_Change,PA_Team_Change,PA_Season_Skipped,PA_Team_Drafted,team_traded,g30_lastyear,g25_lastyear,g20_lastyear,gT10_lastyear,adp_averagePick,ecr,experience) %>%
  mutate(pos_QB = ifelse(Position_order == 1,1,0)) %>%
  mutate(pos_RB = ifelse(Position_order == 2,1,0)) %>%
  mutate(pos_WR = ifelse(Position_order == 3,1,0)) %>%
  mutate(pos_TE = ifelse(Position_order == 4,1,0)) %>%
  select(-Position_order) %>%
  unique() %>%
  print()

df_all_year_before <- read.csv("C:\\Users\\zacro\\Documents\\Apprenticeship Work\\League Winner Predicts\\LWP4.0\\Prediction_Rating4_15th_March_1.csv") %>%
  select(season,yearly_id,Position_order,lws_lastyear,ppr_lastyear,lw_lastyear,sv_lastyear,lw_ever,PA_SuccessRate,PA_HC_Change,PA_Team_Change,PA_Season_Skipped,PA_Team_Drafted,team_traded,g30_lastyear,g25_lastyear,g20_lastyear,gT10_lastyear,adp_averagePick,ecr,experience) %>%
  separate(yearly_id,into = c("year","player_id"), sep = "_") %>%
  mutate(yearly_id = paste0(season+1,"_",player_id)) %>%
  rename(lws_2yrs_ago = lws_lastyear) %>%
  rename(ppr_2yrs_ago = ppr_lastyear) %>%
  rename(lw_2yrs_ago = lw_lastyear) %>%
  rename(sv_2yrs_ago = sv_lastyear) %>%
  rename(PA_SuccessRate_lastyear = PA_SuccessRate) %>%
  rename(PA_HC_Change_lastyear = PA_HC_Change) %>%
  rename(PA_Team_Change_lastyear = PA_Team_Change) %>%
  rename(PA_Season_Skipped_lastyear = PA_Season_Skipped) %>%
  rename(PA_Team_Drafted_lastyear = PA_Team_Drafted) %>%
  rename(adp_averagePick_lastyear = adp_averagePick) %>%
  rename(ecr_lastyear = ecr) %>%
  rename(g30_lastyear_2yrs_ago = g30_lastyear) %>%
  rename(g25_lastyear_2yrs_ago = g25_lastyear) %>%
  rename(g20_lastyear_2yrs_ago = g20_lastyear) %>%
  rename(gT10_lastyear_2yrs_ago = gT10_lastyear) %>%
  select(-season,-year,-Position_order,-experience) %>%
  #select(yearly_id,lws_lastyear,ppr_lastyear,lw_lastyear,sv_lastyear,lw_ever,PA_SuccessRate,PA_HC_Change,PA_Team_Change,PA_Season_Skipped,PA_Team_Drafted,g30_lastyear,g25_lastyear,g20_lastyear,gT10_lastyear,adp_averagePick,ecr,experience) %>%
  select(-player_id) %>%
  print()

df_all_moredetails <- df_all %>%
  left_join(df_all_year_before, by = "yearly_id") %>%
  select(-lw_ever.y) %>%
  rename(lw_ever = lw_ever.x) %>%
  rename(team_traded_lastyear = team_traded.y) %>%
  rename(team_traded = team_traded.x) %>%
  print()


df_rookie <- df_all_moredetails %>%
  filter(experience == 0) %>%
  head(10) %>%
  print()

rookies_22_df_extra_detail <- read.csv("C:\\Users\\zacro\\Documents\\Apprenticeship Work\\League Winner Predicts\\LWP4.0\\rookies_2022_extradetail.csv") %>%
  left_join(positions_2022_rookies, by = "yearly_id") %>%
  mutate(pos_QB = ifelse(position == "QB",1,0)) %>%
  mutate(pos_RB = ifelse(position == "RB",1,0)) %>%
  mutate(pos_WR = ifelse(position == "WR",1,0)) %>%
  mutate(pos_TE = ifelse(position == "TE",1,0)) %>%
  select(-position,-lw_ever.y,-Position_order) %>%
  rename(lw_ever = lw_ever.x) %>%
  print()

base_df_2022_rookies_adp_ecr_only <- base_df_2022_rookies %>%
  select(yearly_id,adp_averagePick,ecr)

rookies_22_df_all_selected_columns <- rookies_22_df_extra_detail %>%
  #select(season,yearly_id,label,Position_order,lws_lastyear,ppr_lastyear,lw_lastyear,sv_lastyear,lw_ever,PA_SuccessRate,PA_HC_Change,PA_Team_Change,PA_Season_Skipped,PA_Team_Drafted,g30_lastyear,g25_lastyear,g20_lastyear,gT10_lastyear,adp_averagePick,ecr,experience) %>%
  mutate(lws_lastyear = NA) %>%
  mutate(ppr_lastyear = NA) %>%
  mutate(g30_lastyear = NA) %>%
  mutate(g25_lastyear = NA) %>%
  mutate(g20_lastyear = NA) %>%
  mutate(gT10_lastyear = NA) %>%
  mutate(season = as.integer(season)) %>%
  #select(-adp_averagePick,-ecr,-X) %>%
  left_join(base_df_2022_rookies_adp_ecr_only, by = "yearly_id") %>%
  mutate(adp_averagePick_placeholder = adp_averagePick) %>%
  mutate(ecr_placeholder = ecr) %>%
  select(-adp_averagePick,-ecr,-X) %>%
  rename(adp_averagePick = adp_averagePick_placeholder) %>%
  rename(ecr = ecr_placeholder) %>%
  mutate(PA_HC_Change = 0) %>%
  mutate(team_traded = 0) %>%
  mutate(team_traded_lastyear = 0) %>%
  print()

df <- df_all_moredetails %>%
  filter(season < 2022, season > 2011) %>%
  print()

df_2022 <- df_all_moredetails %>%
  filter(season == 2022) %>%
  #filter(!is.na(Position_order)) %>%
  unique() %>%
  print()


df_2022_with_rookies <- bind_rows(rookies_22_df_all_selected_columns,df_2022) %>%
  unique() %>%
  print()


#tibble(df)
#####

test_data <- df %>%
  filter(season >= 2012)

train_data <- df %>%
  filter(season < 2019) %>%
  view()

#xgb.save(lw_model_season1,"lwp4_season1.model")

lw_model_season1 <- xgb.load("lw_model_season1.model")

importance <- xgboost::xgb.importance(
  feature_names = colnames(lw_model_season1),
  model = lw_model_season1
)
xgboost::xgb.ggplot.importance(importance_matrix = importance)

preds <- stats::predict(
  lw_model_season1,
  # get rid of the things not needed for prediction here
  as.matrix(test_data %>% select(-label, -yearly_id, -season))
) %>%
  tibble::as_tibble() %>%
  dplyr::rename(lw = value) %>%
  dplyr::bind_cols(test_data)

preds2 <- preds %>%
  right_join(base_df_xgboost, by = "yearly_id") %>%
  select(-season.y) %>%
  rename(season = season.x) %>%
  filter(!is.na(lw)) %>%
  filter(Season_Valid == 1 | Games_Away <2 ) %>%
  print()

  plot <- preds2 %>%
  # Create BINS for lw:
  dplyr::mutate(bin_pred_prob = round(lw / 0.05) * .05) %>%
  dplyr::group_by(bin_pred_prob) %>%
  # Calculate the calibration results:
  dplyr::summarize(
    n_plays = n(),
    n_wins = length(which(label == 1)),
    bin_actual_prob = n_wins / n_plays
  ) %>%
  dplyr::ungroup()

ann_text <- data.frame(
  x = c(.25, 0.75), y = c(0.75, 0.25),
  lab = c("More times\nthan expected", "Fewer times\nthan expected")
)

plot %>%
  ggplot() +
  geom_point(aes(x = bin_pred_prob, y = bin_actual_prob, size = n_plays)) +
  geom_smooth(aes(x = bin_pred_prob, y = bin_actual_prob), method = "loess") +
  geom_abline(slope = 1, intercept = 0, color = "black", lty = 2) +
  coord_equal() +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    size = "Number of plays",
    x = "Estimated LW probability",
    y = "Observed LW probability",
    title = "LW prob calibration plot"
  ) +
  geom_text(data = ann_text, aes(x = x, y = y, label = lab), size = 4) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.background = element_blank(),
    strip.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 10, angle = 90),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.position = "bottom"
  )

#####
### ADDING 2022 PREDICTION TO THE DATA

# STEP 1: REMOVE DATA NOT ASSOCIATED WITH PREDICTION
## SEASON, YEARLY_ID, LABEL

col_names <- colnames(df)

df_2022_with_rookies

df_noIds <- df %>%
  select(-yearly_id,-season,-label) %>%
  head(1) %>%
  print()

df_2022_noID <- df_2022_with_rookies %>%
  select(col_names) %>%
  select(-season,-yearly_id,-label) %>%
  print()

df_2022_noID_matrix <- as.matrix(df_2022_noID)

tibble(df_2022_noID_matrix)

## DO PREDICT(MODEL, NEW_DATA)

df_2022_prediction <- predict(lw_model_season1,df_2022_noID_matrix) %>%
  print()

## ADDING IDS TO MATCH DATA FRAMES TOGETHER

tibble(df_2022_prediction)

df_2022_prediction_data <- data.frame(df_2022_prediction) %>%
  mutate(number_row = row_number()) %>%
  rename(lw = df_2022_prediction) %>%
  print()

## ADDING ROW NUMBER ID AND JOINING DATA

df_2022_row_number_id <- df_2022_with_rookies %>%
  mutate(number_row = row_number()) %>%
  left_join(df_2022_prediction_data, by = "number_row") %>%
  select(-number_row) %>%
  print()

## MERGING 2022 PREDICTION WITH ALL OTHER DATA

base_df_2022_with_rookies_selected_columns <- base_df_2022_with_rookies %>%
  select(yearly_id,season,player_id,full_name,League_Winner,Season_Valid,Games_Away,LWS,PPR,games30,games25,games20,gamesT10) %>%
  print()

df_prediction_all <- bind_rows(preds,df_2022_row_number_id) %>%
  unique() %>%
  right_join(base_df_2022_with_rookies_selected_columns, by = "yearly_id") %>%
  unique() %>%
  rename(gT10_2yrs_ago = gT10_lastyear_2yrs_ago) %>%
  arrange(-lw,adp_averagePick,ecr,-ppr_lastyear,-gT10_lastyear,-lws_lastyear,-g20_lastyear,-ppr_2yrs_ago,ecr_lastyear,adp_averagePick_lastyear,-PA_SuccessRate,-lws_2yrs_ago,-gT10_2yrs_ago) %>%
  left_join(position_data_non_fantasy_2022_merged_rookies, by = "yearly_id") %>%
  select(-X) %>%
  mutate(LWP4_Ranking = row_number()) %>%
  arrange(-League_Winner,-LWS,Games_Away,-games30,-games25,-games20,-gamesT10,-PPR) %>%
  mutate(EndofSeason_Rankings = row_number()) %>%
  arrange(LWP4_Ranking) %>%
  mutate(graph_col_lw = ifelse(Games_Away == 0,"#03C04A",""),
         graph_col_lw = ifelse(Games_Away == 1,"#FC6A03",graph_col_lw),
         graph_col_lw = ifelse(Games_Away > 1,"#900D09",graph_col_lw),) %>%
  select(-season.y) %>%
  rename(season = season.x) %>%
  mutate(rookie_names = yearly_id) %>%
  separate(rookie_names, into = c("year_remove","name_rookie"), sep = "_") %>%
  mutate(full_name = ifelse(is.na(full_name),name_rookie,full_name)) %>%
  select(-year_remove,name_rookie) %>%
  print()
  
  
## ADD 2022 PREDICTION DATA TO SHINY APP

## GETTING DF READY FOR SHINYAPP

lwp4_base_df_p1 <- df_prediction_all %>%
  ### ECR ERROR ON FANTASYPROS SIDE
  filter(yearly_id != "2022_Brian Robinson Jr.") %>%
  ### RIDLEY SUSPENDED FOR THE 2022 SEASON
  filter(yearly_id != "2022_00-0034837") %>%
  print()
  
  
lwp4_base_df_p2 <- df_prediction_all %>%
  filter(yearly_id == "2022_Brian Robinson Jr.") %>%
  head(1) %>%
  print()


lwp4_base_df <- bind_rows(lwp4_base_df_p1,lwp4_base_df_p2) %>%
  unique() %>%
  arrange(LWP4_Ranking) %>%
  arrange(-LWS) %>%
  mutate(LWS_rank = row_number()) %>%
  arrange(LWP4_Ranking) %>%
  arrange(-PPR) %>%
  mutate(PPR_rank = row_number()) %>%
  arrange(LWP4_Ranking) %>%
  arrange(-games30) %>%
  mutate(g30_rank = row_number()) %>%
  arrange(LWP4_Ranking) %>%
  arrange(-games25) %>%
  mutate(g25_rank = row_number()) %>%
  arrange(LWP4_Ranking) %>%
  arrange(-games20) %>%
  mutate(g20_rank = row_number()) %>%
  arrange(LWP4_Ranking) %>%
  arrange(-gamesT10) %>%
  mutate(gT10_rank = row_number()) %>%
  arrange(LWP4_Ranking) %>%
  #filter(is.na(season)) %>%
  separate(yearly_id, into = c("year","player_id"), sep = "_") %>%
  mutate(season = year) %>%
  mutate(yearly_id = paste0(season,"_",player_id)) %>%
  mutate(LWP4_Ranking = ifelse(is.na(adp_averagePick),NA,LWP4_Ranking)) %>%
  #select(-year,-player_id) %>%
  mutate(graph_col_pos = ifelse(position == "QB","#FFC0CB",""),
         graph_col_pos = ifelse(position == "RB","#228C22",graph_col_pos),
         graph_col_pos = ifelse(position == "WR","#03254C",graph_col_pos),
         graph_col_pos = ifelse(position == "TE","#C95B0C",graph_col_pos),) %>%
  mutate(season = as.numeric(season)) %>%
  left_join(fp_ranks_unranked, by = "season") %>%
  left_join(adp_2011_2022_undrafted, by = "season") %>%
  mutate(comparison_valid = ifelse(Games_Away < 2 | Season_Valid == 1,1,0)) %>%
  #select(-X.x,-X.y) %>%
  print()

lwp4_df_med_mean <- lwp4_base_df %>%
  select(season,yearly_id,position,ecr,adp_averagePick) %>%
  filter(position != "QB") %>%
  group_by(season) %>%
  mutate(yearly_rank = row_number()) %>%
  ungroup() %>%
  group_by(yearly_id) %>%
  mutate(median = median(c(yearly_rank,ecr,adp_averagePick))) %>%
  mutate(mean = (yearly_rank+ecr+adp_averagePick)/3) %>%
  ungroup() %>%
  #select(yearly_id,yearly_rank,mean,median) %>%
  mutate(med_mean_sqrt = sqrt(mean*median)) %>%
  mutate(yearly_rank_per = yearly_rank / med_mean_sqrt) %>%
  mutate(compare_p1 = ifelse(med_mean_sqrt < 25,6,"")) %>%
  mutate(compare_p1_b = ifelse(med_mean_sqrt < 49 & med_mean_sqrt > 24,8,compare_p1)) %>%
  mutate(compare_p1_c = ifelse(med_mean_sqrt < 61 & med_mean_sqrt > 48,10,compare_p1_b)) %>%
  mutate(compare_p1_d = ifelse(med_mean_sqrt > 60,12,compare_p1_c)) %>%
  mutate(compare_p1_d = as.double(compare_p1_d)) %>%
  mutate(compare_p2 = ifelse(yearly_rank - med_mean_sqrt > compare_p1_d | yearly_rank - med_mean_sqrt < compare_p1_d*-1,1,0)) %>%
  mutate(compare_p3 = ifelse(yearly_rank == median,0,compare_p2)) %>%
  mutate(compare_p4 = ifelse(adp_averagePick > 240 | ecr > 240,0,compare_p3)) %>%
  mutate(compare_p5 = ifelse(yearly_rank_per < 1 & (adp_averagePick > 240 | ecr > 240),0,compare_p3)) %>%
  select(yearly_id,yearly_rank,yearly_rank_per,med_mean_sqrt,compare_p5) %>%
  rename(compare_p2 = compare_p5) %>%
  arrange(-compare_p2,yearly_rank_per) %>%
  mutate(lwp4_comp_rank = row_number()) %>%
  view()

lwp4_EOS_yearly <- lwp4_base_df %>%
  filter(position != "QB") %>%
  select(season,yearly_id,EndofSeason_Rankings) %>%
  arrange(EndofSeason_Rankings) %>%
  group_by(season) %>%
  mutate(EOS_yearly = row_number()) %>%
  ungroup() %>%
  select(yearly_id,EOS_yearly) %>%
  print()

tibble(lwp4_EOS_yearly)
  

lwp4_df_pre_zr_rankings <- lwp4_base_df %>%
  left_join(lwp4_df_med_mean, by = "yearly_id") %>%
  select(-name_rookie,-year) %>%
  left_join(lwp4_EOS_yearly, by = "yearly_id") %>%
  mutate(Position_order = ifelse(position == "QB",1,""),
         Position_order = ifelse(position == "RB",2,Position_order),
         Position_order = ifelse(position == "WR",3,Position_order),
         Position_order = ifelse(position == "TE",4,Position_order),) %>%
  #filter(position != "QB") %>%
  print()

lwp4_df_zr_rankings <- lwp4_df_pre_zr_rankings %>%
  #filter(season == 2022) %>%
  filter(!is.na(yearly_rank)) %>%
  select(season,yearly_id,full_name,position,adp_averagePick,ecr,yearly_rank,lw) %>%
  mutate(prem_pos = ifelse(position == "RB" | position == "TE",1,0)) %>%
  mutate(lw_rankings = ifelse(prem_pos == 1,lw*1.1,lw)) %>%
  group_by(season,yearly_id,full_name,position) %>%
  mutate(min_pick = min(adp_averagePick,ecr)) %>%
  mutate(min_pick_mod12 = min_pick %% 12) %>%
  mutate(round_expectation = floor(min_pick / 12)) %>%
  mutate(round_expectation_adjustment = ifelse(min_pick_mod12 < 6,round_expectation-1,round_expectation)) %>%
  mutate(round_expectation_adjustment = ifelse(round_expectation_adjustment < 0, 0, round_expectation_adjustment)) %>%
  mutate(min_pick_adjusted = round_expectation_adjustment*12+1) %>%
  ungroup() %>%
  arrange(-lw_rankings) %>%
  group_by(season) %>%
  mutate(zr_rankings = row_number()) %>%
  mutate(zr_rankings_adjusted = ifelse(zr_rankings < min_pick_adjusted,min_pick_adjusted,zr_rankings)) %>%
  mutate(zr_rankings_adjusted_tickbox = ifelse(zr_rankings < min_pick_adjusted,1,0)) %>%
  arrange(zr_rankings_adjusted,zr_rankings) %>%
  mutate(zr_rankings_adjusted = row_number()) %>%
  ### LOOP TEST
  mutate(zr_rankings_adjusted = ifelse(zr_rankings_adjusted < min_pick_adjusted,min_pick_adjusted,zr_rankings_adjusted)) %>%
  arrange(zr_rankings_adjusted,zr_rankings) %>%
  mutate(zr_rankings_adjusted = row_number()) %>%
  ungroup() %>%
  select(yearly_id,zr_rankings_adjusted)

lwp4_df <- lwp4_df_pre_zr_rankings %>%
  left_join(lwp4_df_zr_rankings, by = "yearly_id") %>%
  mutate(compare_p2 = ifelse(is.na(compare_p2),0,compare_p2)) %>%
  mutate(yearly_rank = ifelse(is.na(yearly_rank),650,yearly_rank)) %>%
  mutate(zr_rankings_adjusted = ifelse(is.na(zr_rankings_adjusted),650,zr_rankings_adjusted)) %>%
  view()


write.csv(lwp4_df,"C:\\Users\\zacro\\Documents\\Apprenticeship Work\\League Winner Predicts\\LWP4.0\\shinydata_05_04_1.csv")
#write.csv(df_rookie,"C:\\Users\\zacro\\Documents\\Apprenticeship Work\\League Winner Predicts\\LWP4.0\\base_df_rookies.csv")


lwp4_df_graph <- lwp4_df %>%
  filter(position != "qb")

lwp4_df_max <- lwp4_df_graph %>%
  filter(season != 2022) %>%
  filter(!is.na(yearly_rank),!is.na(lw)) %>%
  group_by(yearly_rank) %>%
  summarise(lw = max(lw)) %>%
  print()

lwp4_df_min <- lwp4_df_graph %>%
  filter(season != 2022) %>%
  filter(!is.na(yearly_rank),!is.na(lw)) %>%
  group_by(yearly_rank) %>%
  summarise(lw = min(lw)) %>%
  print()

lwp4_df_med <- lwp4_df_graph %>%
  filter(season != 2022) %>%
  filter(!is.na(yearly_rank),!is.na(lw)) %>%
  group_by(yearly_rank) %>%
  summarise(lw = median(lw)) %>%
  print()



lwp4_df_graph %>%
  ggplot(aes(x = yearly_rank, y = lw)) +
  geom_smooth() +
  #geom_point(data = lwp4_df_graph %>% filter(season == 2012), size = 3, col = "red") +
  #geom_point(data = lwp4_df_graph %>% filter(season == 2019), size = 3, col = "forestgreen") +
  geom_point(data = lwp4_df_graph %>% filter(season == 2022), size = 3, col = "black") +
  geom_line(data = lwp4_df_max) +
  geom_line(data = lwp4_df_min) +
  theme_bw() +
  xlim(0,100)


#####

grid_size <- 400

test_data <- df %>%
  filter(season >= 2012)

train_data <- df %>%
  filter(season < 2019)

folds <- splitTools::create_folds(
  y = train_data$yearly_id,
  k = 10,
  type = "grouped",
  invert = TRUE
)

train_labels <- train_data %>%
  dplyr::select(label)

train_data <- train_data %>%
  select(-season,-yearly_id,-label)

str(folds)

grid <- dials::grid_latin_hypercube(
  dials::finalize(dials::mtry(),train_data),
  dials::min_n(),
  dials::tree_depth(),
  dials::learn_rate(range = c(-1.5,-0.5),trans = scales::log10_trans()),
  dials::loss_reduction(),
  sample_size = dials::sample_prop(),
  size = grid_size
) %>%
  dplyr::mutate(
    mtry = mtry/ length(train_data),
    monotone_constraints = "(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)"
  ) %>%
  dplyr::rename(
    eta = learn_rate,
    gamma = loss_reduction,
    subsample = sample_size,
    colsample_bytree = mtry,
    max_depth = tree_depth,
    min_child_weight = min_n
  )

grid

get_row <- function(row) {
  params <-
    list(
      booster = "gbtree",
      objective = "binary:logistic",
      eval_metric = c("logloss"),
      eta = row$eta,
      gamma = row$gamma,
      subsample = row$subsample,
      colsample_bytree = row$colsample_bytree,
      max_depth = row$max_depth,
      min_child_weight = row$min_child_weight,
      monotone_constraints = row$monotone_constraints
    )
  
  # do the cross validation
  lw_cv_model <- xgboost::xgb.cv(
    data = as.matrix(train_data),
    label = train_labels$label,
    params = params,
    # this doesn't matter with early stopping in xgb.cv, just set a big number
    # the actual optimal rounds will be found in this tuning process
    nrounds = 15000,
    # created above
    folds = folds,
    metrics = list("logloss"),
    early_stopping_rounds = 50,
    print_every_n = 50
  )
  
  # bundle up the results together for returning
  output <- params
  output$iter <- lw_cv_model$best_iteration
  output$logloss <- lw_cv_model$evaluation_log[output$iter]$test_logloss_mean
  
  row_result <- bind_rows(output)
  
  return(row_result)
}

results <- purrr::map_df(1:nrow(grid), function(x) {
  get_row(grid %>% dplyr::slice(x))
})

results %>%
  dplyr::select(logloss, eta, gamma, subsample, colsample_bytree, max_depth, min_child_weight) %>%
  tidyr::pivot_longer(eta:min_child_weight,
                      values_to = "value",
                      names_to = "parameter"
  ) %>%
  ggplot(aes(value, logloss, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE, size = 3) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "logloss") +
  theme_minimal()

grid <- dials::grid_latin_hypercube(
  # don't need the finalize business since we're using length in here
  dials::mtry(range = c(length(train_data) / 4, length(train_data))),
  dials::min_n(),
  # force tree depth to be between 3 and 5
  dials::tree_depth(range = c(4L, 8L)),
  # to force learn_rate to not be crazy small like dials defaults to
  dials::learn_rate(range = c(-1.5, -1), trans = scales::log10_trans()),
  dials::loss_reduction(),
  sample_size = dials::sample_prop(),
  size = grid_size
) %>%
  dplyr::mutate(
    # has to be between 0 and 1 for xgb
    # for some reason mtry gives the number of columns rather than proportion
    mtry = mtry / length(train_data),
    monotone_constraints = "(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)"
  ) %>%
  # make these the right names for xgb
  dplyr::rename(
    eta = learn_rate,
    gamma = loss_reduction,
    subsample = sample_size,
    colsample_bytree = mtry,
    max_depth = tree_depth,
    min_child_weight = min_n
  )

grid

results %>%
  dplyr::select(logloss, eta, gamma, subsample, colsample_bytree, max_depth, min_child_weight) %>%
  tidyr::pivot_longer(eta:min_child_weight,
                      values_to = "value",
                      names_to = "parameter"
  ) %>%
  ggplot(aes(value, logloss, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE, size = 3) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "logloss") +
  theme_minimal()

best_model <- results

params <-
  list(
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = c("logloss"),
    eta = best_model$eta,
    gamma = best_model$gamma,
    subsample = best_model$subsample,
    colsample_bytree = best_model$colsample_bytree,
    max_depth = best_model$max_depth,
    min_child_weight = best_model$min_child_weight,
    monotone_constraints = best_model$monotone_constraints
  )

nrounds <- best_model$iter

params

lw_model_season1 <- xgboost::xgboost(
  params = params,
  data = as.matrix(train_data),
  label = train_labels$label,
  nrounds = nrounds,
  verbose = 2
)

importance <- xgboost::xgb.importance(
  feature_names = colnames(lw_model_season1),
  model = lw_model_season1
)
xgboost::xgb.ggplot.importance(importance_matrix = importance)

preds <- stats::predict(
  lw_model_season1,
  # get rid of the things not needed for prediction here
  as.matrix(test_data %>% select(-label, -yearly_id, -season))
) %>%
  tibble::as_tibble() %>%
  dplyr::rename(lw = value) %>%
  dplyr::bind_cols(test_data)

preds2 <- preds %>%
  right_join(base_df_xgboost, by = "yearly_id") %>%
  select(-season.y) %>%
  rename(season = season.x) %>%
  #filter(!is.na(lw)) %>%
  #filter(Season_Valid == 1 | Games_Away <2 ) %>%
  print()

MLmetrics::LogLoss(preds$lw, preds$label)

MLmetrics::Accuracy(
  # say a team is predicted to win if they have win prob > .5
  preds2 %>%
    dplyr::mutate(pred = ifelse(lw > .5, 1, 0)) %>%
    dplyr::pull(pred),
  # compare to whether they actually won
  preds2$label
)

plot <- preds2 %>%
  # Create BINS for lw:
  dplyr::mutate(bin_pred_prob = round(lw / 0.05) * .05) %>%
  dplyr::group_by(bin_pred_prob) %>%
  # Calculate the calibration results:
  dplyr::summarize(
    n_plays = n(),
    n_wins = length(which(label == 1)),
    bin_actual_prob = n_wins / n_plays
  ) %>%
  dplyr::ungroup()

ann_text <- data.frame(
  x = c(.25, 0.75), y = c(0.75, 0.25),
  lab = c("More times\nthan expected", "Fewer times\nthan expected")
)

plot %>%
  ggplot() +
  geom_point(aes(x = bin_pred_prob, y = bin_actual_prob, size = n_plays)) +
  geom_smooth(aes(x = bin_pred_prob, y = bin_actual_prob), method = "loess") +
  geom_abline(slope = 1, intercept = 0, color = "black", lty = 2) +
  coord_equal() +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    size = "Number of plays",
    x = "Estimated LW probability",
    y = "Observed LW probability",
    title = "LW prob calibration plot"
  ) +
  geom_text(data = ann_text, aes(x = x, y = y, label = lab), size = 4) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.background = element_blank(),
    strip.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 10, angle = 90),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.position = "bottom"
  )

xgb.save(lw_model_season1,"lw_model_season1.model")

test <- xgb.load("lw_model_season1.model")

xgb.loa


























