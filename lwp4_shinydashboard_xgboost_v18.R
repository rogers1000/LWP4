

library(tidyverse)
library(shinydashboard)
library(shiny)
library(gt)
library(remotes)
library(gtExtras)

name_data_shiny <- read.csv(url("https://raw.githubusercontent.com/rogers1000/LWP4/main/name_data.csv")) %>%
  select(gsis_id,full_name)

position_data_shiny <- read.csv(url("https://raw.githubusercontent.com/rogers1000/LWP4/main/position_data.csv")) %>%
  select(yearly_id,position)

fp_ranks_unranked <- read.csv(url("https://raw.githubusercontent.com/rogers1000/LWP4/main/fp_ranks_unranked.csv"))

adp_2011_2022_undrafted <- read.csv(url("https://raw.githubusercontent.com/rogers1000/LWP4/main/adp_2011_2022_undrafted.csv"))

lwp4_df <- read.csv(url("https://raw.githubusercontent.com/rogers1000/LWP4/main/shinydata_05_04_1.csv"))


name_data_vector1 <- lwp4_df %>%
  arrange(-season,Position_order,LWP4_Ranking) %>%
  select(player_id,full_name) %>%
  unique()

name_data_vector <- dplyr::pull(name_data_vector1,full_name)

graph_axis_limits <- data.frame(names = c("LWS","PPR","games30","games25","games20","gamesT10","League_Winner","lw","LWP4_Ranking","adp_averagePick","ecr","EndofSeason_Rankings","Games_Away","Season_Valid"),min = c(0,0,0,0,0,0,0,0,1,1,1,1,0,0), max = c(60,500,10,12,15,15,1,1,6000,275,650,12000,3,1))


ui <- dashboardPage(title = "LWP4.0",
                    dashboardHeader(title = "LWP4.0"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Prediction Data", tabName = "prediction_data"),
                        menuItem("Single Season Data", tabName = "single_season_data"),
                        menuItem("Dynasty Data", tabName = "dynasty_data"),
                        menuItem("Player Focus", tabName = "player_focus")
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "prediction_data",
                                box(width = 3, height = 1160,
                                    selectInput(inputId = "prediction_data_season_slider",
                                                label = "Season:",
                                                choices = c(2012:2022),
                                                selected = 2022),
                                    selectInput(inputId = "prediction_data_position",
                                                       label = "Positions:",
                                                       choices = c("QB","RB","WR","TE"),
                                                       selected = c("RB","WR","TE"),
                                                multiple = TRUE),
                                    selectInput(inputId = "prediction_data_sort",
                                                label = "Sort By:",
                                                choices = c("LW Percentage" = "LWP4_Ranking",
                                                            "Position" = "Position_order",
                                                            "ZR Rankings" = "zr_rankings_adjusted",
                                                            "ADP" = "adp_averagePick",
                                                            "FantasyPros ECR" = "ecr",
                                                            "LWP4 Comparison" = "lwp4_comp_rank"),
                                                selected = "LWP4_Ranking"),
                                    fixedRow(width = 3,
                                             column(width = 6,
                                    numericInput(inputId = "prediction_data_lwpercent_min",
                                                 label = "LW Percentage Min:",
                                                 min = 0,
                                                 max = 100,
                                                 value = 0,
                                                 width = NA)),
                                    column(width = 6,
                                    numericInput(inputId = "prediction_data_lwpercent_max",
                                                 label = "LW Percentage Max:",
                                                 min = 0,
                                                 max = 100,
                                                 value = 100,
                                                 width = NA))),
                                    fixedRow(width = 3,
                                             column(width = 6,
                                                    numericInput(inputId = "prediction_data_lwp4ranking_min",
                                                                 label = "LWP4 Ranking - Overall (Min):",
                                                                 min = 1,
                                                                 max = 12000,
                                                                 value = 1)),
                                             column(width = 6,
                                                    numericInput(inputId = "prediction_data_lwp4ranking_max",
                                                                 label = "LWP4 Ranking - Overall (Max):",
                                                                 min = 1,
                                                                 max = 12000,
                                                                 value = 12000)),),
                                    fixedRow(width = 3,
                                             column(width = 6,
                                                    numericInput(inputId = "prediction_data_lwp4ranking_filtered_min",
                                                                 label = "LWP4 Ranking - Filtered (Min):",
                                                                 min = 1,
                                                                 max = 12000,
                                                                 value = 1)),
                                             column(width = 6,
                                                    numericInput(inputId = "prediction_data_lwp4ranking_filtered_max",
                                                                 label = "LWP4 Ranking - Filtered (Max):",
                                                                 min = 1,
                                                                 max = 12000,
                                                                 value = 12000)),),
                                    fixedRow(width = 3,
                                             column(width = 6,
                                                    numericInput(inputId = "prediction_data_zr_rankings_min",
                                                                 label = "ZR Rankings (Min):",
                                                                 min = 1,
                                                                 max = 650,
                                                                 value = 1)),
                                             column(width = 6,
                                                    numericInput(inputId = "prediction_data_zr_rankings_max",
                                                                 label = "ZR Rankings (Max):",
                                                                 min = 1,
                                                                 max = 650,
                                                                 value = 650)),),
                                    fixedRow(width = 3,
                                             column(width = 6,
                                                    numericInput(inputId = "prediction_data_adp_min",
                                                                 label = "Season ADP by MFL (Min):",
                                                                 min = 1,
                                                                 max = 275,
                                                                 value = 1)),
                                             column(width = 6,
                                                    numericInput(inputId = "prediction_data_adp_max",
                                                                 label = "Season ADP by MFL (Max):",
                                                                 min = 1,
                                                                 max = 275,
                                                                 value = 275)),),
                                    fixedRow(width = 3,
                                             column(width = 6,
                                                    numericInput(inputId = "prediction_data_ecr_min",
                                                                 label = "Season ECR by FantasyPros (Min):",
                                                                 min = 1,
                                                                 max = 650,
                                                                 value = 1)),
                                             column(width = 6,
                                                    numericInput(inputId = "prediction_data_ecr_max",
                                                                 label = "Season ECR by FantasyPros (Max):",
                                                                 min = 1,
                                                                 max = 650,
                                                                 value = 650)),),
                                    selectInput(inputId = "prediction_data_lw_lastyear",
                                                label = "LW Last Year:",
                                                choices = c("Unfiltered" = 2,
                                                            "Yes" = 0,
                                                            "No" = 1),
                                                selected = 2),
                                    selectInput(inputId = "prediction_data_successful",
                                                label = "Successful Last Year:",
                                                choices = c("Unfiltered" = 2,
                                                            "Yes" = 0,
                                                            "No" = 1),
                                                selected = 2),
                                    selectInput(inputId = "prediction_data_season_valid",
                                                label = "Season Valid Last Year:",
                                                choices = c("Unfiltered" = 2,
                                                            "Yes" = 0,
                                                            "No" = 1),
                                                selected = 2),
                                    fixedRow(width = 3,
                                             column(width = 6,
                                                    numericInput(inputId = "prediction_data_lws_lastyear_min",
                                                                 label = "LWS Last Year (Min):",
                                                                 min = 0,
                                                                 max = 60,
                                                                 value = 0)),
                                             column(width = 6,
                                                    numericInput(inputId = "prediction_data_lws_lastyear_max",
                                                                 label = "LWS Last Year (Max):",
                                                                 min = 1,
                                                                 max = 60,
                                                                 value = 60)),),
                                    fixedRow(width = 3,
                                             column(width = 6,
                                                    numericInput(inputId = "prediction_data_ppr_lastyear_min",
                                                                 label = "PPR Last Year (Min):",
                                                                 min = 0,
                                                                 max = 500,
                                                                 value = 0)),
                                             column(width = 6,
                                                    numericInput(inputId = "prediction_data_ppr_lastyear_max",
                                                                 label = "PPR Last Year (Max):",
                                                                 min = 0,
                                                                 max = 500,
                                                                 value = 500)),),
                                    selectInput(inputId = "prediction_data_lw_ever",
                                                label = "League Winner Ever:",
                                                choices = c("Unfiltered" = 2,
                                                            "Yes" = 0,
                                                            "No" = 1),
                                                selected = 2),
                                    selectInput(inputId = "prediction_data_team_drafted",
                                                label = "Team Drafted:",
                                                choices = c("Unfiltered" = 2,
                                                            "Yes" = 0,
                                                            "No" = 1),
                                                selected = 2),
                                    selectInput(inputId = "prediction_data_team_traded",
                                                label = "Team Traded:",
                                                choices = c("Unfiltered" = 2,
                                                            "Yes" = 0,
                                                            "No" = 1),
                                                selected = 2),
                                    selectInput(inputId = "prediction_data_changed_team",
                                                label = "Changed Team:",
                                                choices = c("Unfiltered" = 2,
                                                            "Yes" = 0,
                                                            "No" = 1),
                                                selected = 2),
                                    selectInput(inputId = "prediction_data_changed_hc",
                                                label = "Changed HC:",
                                                choices = c("Unfiltered" = 2,
                                                            "Yes" = 0,
                                                            "No" = 1),
                                                selected = 2),
                                    selectInput(inputId = "prediction_data_skipped_season",
                                                label = "Skipped Season Last Year:",
                                                choices = c("Unfiltered" = 2,
                                                            "Yes" = 0,
                                                            "No" = 1),
                                                selected = 2),
                                    fixedRow(width = 3,
                                             column(width = 6,
                                                    numericInput(inputId = "prediction_data_experience_min",
                                                                 label = "Experience (Min):",
                                                                 min = 0,
                                                                 max = 20,
                                                                 value = 0)),
                                             column(width = 6,
                                                    numericInput(inputId = "prediction_data_experience_max",
                                                                 label = "Experience (Max):",
                                                                 min = 0,
                                                                 max = 20,
                                                                 value = 20)),),
                                ),
                                box(width = 9, height = 1160,
                                    gt_output(outputId = "prediction_data_table"))
                        ),
                        tabItem(tabName = "single_season_data",
                                box(width = 3, height = 1160,
                                    fixedRow(width = 3,
                                             column(width = 6,
                                                    selectInput(inputId = "single_season_data_season_min",
                                                                 label = "Season (Min):",
                                                                 choices = c(2000:2022),
                                                                 selected = 2021)),
                                             column(width = 6,
                                                    selectInput(inputId = "single_season_data_season_max",
                                                                 label = "Season (Max):",
                                                                 choices = c(2000:2022),
                                                                 selected = 2021)),),
                                    selectInput(inputId = "single_season_data_positions",
                                                       label = "Positions:",
                                                       choices = c("QB","RB","WR","TE"),
                                                       selected = c("RB","WR","TE"),
                                                multiple = TRUE),
                                    selectInput(inputId = "single_season_data_x_axis",
                                                label = "X Axis:",
                                                choices = c("PPR" = "PPR",
                                                            "League Winner Score" = "LWS",
                                                            "30+ Games" = "games30",
                                                            "25+ Games" = "games25",
                                                            "20+ Games" = "games20",
                                                            "Top 10 Games" = "gamesT10",
                                                            "LW Percent" = "lw",
                                                            "LWP4 Ranking - Overall" = "LWP4_Ranking",
                                                            "ZR Rankings" = "zr_rankings_adjusted",
                                                            "ADP (by MFL)" = "adp_averagePick",
                                                            "ECR (by FantasyPros)" = "ecr",
                                                            "LW Rank" = "EndofSeason_Rankings",
                                                            "Games Away" = "Games_Away",
                                                            "Season Valid" = "Season_Valid"),
                                                selected = "PPR"),
                                    selectInput(inputId = "single_season_data_y_axis",
                                                label = "Y Axis:",
                                                choices = c("League Winner Score" = "LWS",
                                                            "PPR" = "PPR",
                                                            "30+ Games" = "games30",
                                                            "25+ Games" = "games25",
                                                            "20+ Games" = "games20",
                                                            "Top 10 Games" = "gamesT10",
                                                            "League Winner" = "League_Winner",
                                                            "LW Percent" = "lw",
                                                            "LWP4 Ranking - Overall" = "LWP4_Ranking",
                                                            "ZR Rankings" = "zr_rankings_adjusted",
                                                            "ADP (by MFL)" = "adp_averagePick",
                                                            "ECR (by FantasyPros)" = "ecr",
                                                            "LW Rank" = "EndofSeason_Rankings",
                                                            "Games Away" = "Games_Away",
                                                            "Season Valid" = "Season_Valid"),
                                                selected = "League Winner Score"),
                                    selectInput(inputId = "single_season_data_sort",
                                                label = "Sort Table By:",
                                                choices = c("End of Season Rank" = "EndofSeason_Rankings",
                                                            "LW Percent" = "LWP4_Ranking",
                                                            "LWP4 Comparison" = "lwp4_comp_rank",
                                                            "ZR Rankings" = "zr_rankings_adjusted",
                                                            "ADP" = "adp_averagePick",
                                                            "FantasyPros ECR" = "ecr",
                                                            "LW Score" = "LWS_rank",
                                                            "PPR" = "PPR_rank",
                                                            "30+" = "g30_rank",
                                                            "25+" = "g25_rank",
                                                            "20+" = "g20_rank",
                                                            "Top 10" = "gT10_rank"),
                                                selected = "EndOfSeason_Rankings"),
                                    selectInput(inputId = "single_season_data_graph_col",
                                                label = "Graph Highlights:",
                                                choices = c("League Winners" = "graph_col_lw",
                                                            "Position" = "graph_col_pos"),
                                                selected = "League Winners"),
                                    fixedRow(width = 3, 
                                             column(width = 6,
                                                    numericInput(inputId = "single_season_data_lwpercent_min",
                                                                 label = "LW Percentage Min:",
                                                                 min = 0,
                                                                 max = 100,
                                                                 value = 0,
                                                                 width = NA)),
                                             column(width = 6,
                                                    numericInput(inputId = "single_season_data_lwpercent_max",
                                                                 label = "LW Percentage Max:",
                                                                 min = 0,
                                                                 max = 100,
                                                                 value = 100,
                                                                 width = NA))),
                                    fixedRow(width = 3,
                                             column(width = 6,
                                                    numericInput(inputId = "single_season_data_lwp4ranking_min",
                                                                 label = "LWP4 Ranking - Overall (Min):",
                                                                 min = 1,
                                                                 max = 12000,
                                                                 value = 1)),
                                             column(width = 6,
                                                    numericInput(inputId = "single_season_data_lwp4ranking_max",
                                                                 label = "LWP4 Ranking - Overall (Max):",
                                                                 min = 1,
                                                                 max = 12000,
                                                                 value = 12000)),),
                                    fixedRow(width = 3,
                                             column(width = 6,
                                                    numericInput(inputId = "single_season_data_lwp4ranking_filtered_min",
                                                                 label = "LWP4 Ranking - Filtered (Min):",
                                                                 min = 1,
                                                                 max = 12000,
                                                                 value = 1)),
                                             column(width = 6,
                                                    numericInput(inputId = "single_season_data_lwp4ranking_filtered_max",
                                                                 label = "LWP4 Ranking - Filtered (Max):",
                                                                 min = 1,
                                                                 max = 12000,
                                                                 value = 12000)),),
                                    fixedRow(width = 3,
                                             column(width = 6,
                                                    numericInput(inputId = "single_season_data_zr_rankings_min",
                                                                 label = "ZR Rankings (Min):",
                                                                 min = 1,
                                                                 max = 650,
                                                                 value = 1)),
                                             column(width = 6,
                                                    numericInput(inputId = "single_season_data_zr_rankings_max",
                                                                 label = "ZR Rankings (Max):",
                                                                 min = 1,
                                                                 max = 650,
                                                                 value = 650)),),
                                    fixedRow(width = 3,
                                             column(width = 6,
                                                    numericInput(inputId = "single_season_data_adp_min",
                                                                 label = "Season ADP by MFL (Min):",
                                                                 min = 1,
                                                                 max = 275,
                                                                 value = 1)),
                                             column(width = 6,
                                                    numericInput(inputId = "single_season_data_adp_max",
                                                                 label = "Season ADP by MFL (Max):",
                                                                 min = 1,
                                                                 max = 275,
                                                                 value = 275)),),
                                    fixedRow(width = 3,
                                             column(width = 6,
                                                    numericInput(inputId = "single_season_data_ecr_min",
                                                                 label = "Season ECR by FantasyPros (Min):",
                                                                 min = 1,
                                                                 max = 650,
                                                                 value = 1)),
                                             column(width = 6,
                                                    numericInput(inputId = "single_season_data_ecr_max",
                                                                 label = "Season ECR by FantasyPros (Max):",
                                                                 min = 1,
                                                                 max = 650,
                                                                 value = 650)),),
                                    selectInput(inputId = "single_season_data_lw",
                                                label = "League Winner:",
                                                choices = c("Unfiltered" = 2,
                                                            "Yes" = 0,
                                                            "No" = 1),
                                                selected = 2),
                                    fixedRow(width = 3, 
                                             column(width = 6,
                                                    numericInput(inputId = "single_season_data_gamesaway_min",
                                                                 label = "Games Away (Min):",
                                                                 min = 0,
                                                                 max = 3,
                                                                 value = 0)),
                                             column(width = 6,
                                                    numericInput(inputId = "single_season_data_gamesaway_max",
                                                                 label = "Games Away (Max):",
                                                                 min = 0,
                                                                 max = 3,
                                                                 value = 3)),),
                                    selectInput(inputId = "single_season_data_sv",
                                                label = "Season Valid:",
                                                choices = c("Unfiltered" = 2,
                                                            "Yes" = 0,
                                                            "No" = 1),
                                                selected = 2),
                                    selectInput(inputId = "single_season_data_comp_valid",
                                                label = "Comparison Valid:",
                                                choices = c("Unfiltered" = 2,
                                                            "Yes" = 0,
                                                            "No" = 1),
                                                selected = 2),
                                    fixedRow(width = 3,
                                             column(width = 6,
                                                    numericInput(inputId = "single_season_data_lws_min",
                                                                 label = "LWS (Min):",
                                                                 min = 0,
                                                                 max = 60,
                                                                 value = 0)),
                                             column(width = 6,
                                                    numericInput(inputId = "single_season_data_lws_max",
                                                                 label = "LWS (Max):",
                                                                 min = 1,
                                                                 max = 60,
                                                                 value = 60)),),
                                    fixedRow(width = 3,
                                             column(width = 6,
                                                    numericInput(inputId = "single_season_data_ppr_min",
                                                                 label = "PPR (Min):",
                                                                 min = 0,
                                                                 max = 500,
                                                                 value = 0)),
                                             column(width = 6,
                                                    numericInput(inputId = "single_season_data_ppr_max",
                                                                 label = "PPR Max):",
                                                                 min = 0,
                                                                 max = 500,
                                                                 value = 500)),),
                                    fixedRow(width = 3,
                                             column(width = 6,
                                                    numericInput(inputId = "single_season_data_30_min",
                                                                 label = "30+ Games (Min):",
                                                                 min = 1,
                                                                 max = 10,
                                                                 value = 0)),
                                             column(width = 6,
                                                    numericInput(inputId = "single_season_data_30_max",
                                                                 label = "30+ Games (Max):",
                                                                 min = 1,
                                                                 max = 10,
                                                                 value = 10)),),
                                    fixedRow(width = 3,
                                             column(width = 6,
                                                    numericInput(inputId = "single_season_data_25_min",
                                                                 label = "25+ Games (Min):",
                                                                 min = 0,
                                                                 max = 12,
                                                                 value = 0)),
                                             column(width = 6,
                                                    numericInput(inputId = "single_season_data_25_max",
                                                                 label = "25+ Games (Max):",
                                                                 min = 0,
                                                                 max = 12,
                                                                 value = 12)),),
                                    fixedRow(width = 3,
                                             column(width = 6,
                                                    numericInput(inputId = "single_season_data_20_min",
                                                                 label = "20+ Games (Min):",
                                                                 min = 1,
                                                                 max = 15,
                                                                 value = 0)),
                                             column(width = 6,
                                                    numericInput(inputId = "single_season_data_20_max",
                                                                 label = "20+ Games (Max):",
                                                                 min = 1,
                                                                 max = 15,
                                                                 value = 15)),),
                                    fixedRow(width = 3,
                                             column(width = 6,
                                                    numericInput(inputId = "single_season_data_T10_min",
                                                                 label = "Top 10 Games (Min):",
                                                                 min = 0,
                                                                 max = 15,
                                                                 value = 0)),
                                             column(width = 6,
                                                    numericInput(inputId = "single_season_data_T10_max",
                                                                 label = "Top 10 Games (Max):",
                                                                 min = 0,
                                                                 max = 15,
                                                                 value = 15)),),
                                    selectInput(inputId = "single_season_data_lw_lastyear",
                                                label = "LW Last Year:",
                                                choices = c("Unfiltered" = 2,
                                                            "Yes" = 0,
                                                            "No" = 1),
                                                selected = 2),
                                    selectInput(inputId = "single_season_data_successful",
                                                label = "Successful Last Year:",
                                                choices = c("Unfiltered" = 2,
                                                            "Yes" = 0,
                                                            "No" = 1),
                                                selected = 2),
                                    selectInput(inputId = "single_season_data_season_valid",
                                                label = "Season Valid Last Year:",
                                                choices = c("Unfiltered" = 2,
                                                            "Yes" = 0,
                                                            "No" = 1),
                                                selected = 2),
                                    fixedRow(width = 3,
                                             column(width = 6,
                                                    numericInput(inputId = "single_season_data_lws_lastyear_min",
                                                                 label = "LWS Last Year (Min):",
                                                                 min = 0,
                                                                 max = 60,
                                                                 value = 0)),
                                             column(width = 6,
                                                    numericInput(inputId = "single_season_data_lws_lastyear_max",
                                                                 label = "LWS Last Year (Max):",
                                                                 min = 1,
                                                                 max = 60,
                                                                 value = 60)),),
                                    fixedRow(width = 3,
                                             column(width = 6,
                                                    numericInput(inputId = "single_season_data_ppr_lastyear_min",
                                                                 label = "PPR Last Year (Min):",
                                                                 min = 0,
                                                                 max = 500,
                                                                 value = 0)),
                                             column(width = 6,
                                                    numericInput(inputId = "single_season_data_ppr_lastyear_max",
                                                                 label = "PPR Last Year (Max):",
                                                                 min = 0,
                                                                 max = 500,
                                                                 value = 500)),),
                                    selectInput(inputId = "single_season_data_lw_ever",
                                                label = "League Winner Ever:",
                                                choices = c("Unfiltered" = 2,
                                                            "Yes" = 0,
                                                            "No" = 1),
                                                selected = 2),
                                    selectInput(inputId = "single_season_data_team_drafted",
                                                label = "Team Drafted:",
                                                choices = c("Unfiltered" = 2,
                                                            "Yes" = 0,
                                                            "No" = 1),
                                                selected = 2),
                                    selectInput(inputId = "single_season_data_team_traded",
                                                label = "Team Traded:",
                                                choices = c("Unfiltered" = 2,
                                                            "Yes" = 0,
                                                            "No" = 1),
                                                selected = 2),
                                    selectInput(inputId = "single_season_data_changed_team",
                                                label = "Changed Team:",
                                                choices = c("Unfiltered" = 2,
                                                            "Yes" = 0,
                                                            "No" = 1),
                                                selected = 2),
                                    selectInput(inputId = "single_season_data_changed_hc",
                                                label = "Changed HC:",
                                                choices = c("Unfiltered" = 2,
                                                            "Yes" = 0,
                                                            "No" = 1),
                                                selected = 2),
                                    selectInput(inputId = "single_season_data_skipped_season",
                                                label = "Skipped Season Last Year:",
                                                choices = c("Unfiltered" = 2,
                                                            "Yes" = 0,
                                                            "No" = 1),
                                                selected = 2),
                                    fixedRow(width = 3,
                                             column(width = 6,
                                                    numericInput(inputId = "single_season_data_experience_min",
                                                                 label = "Experience (Min):",
                                                                 min = 0,
                                                                 max = 20,
                                                                 value = 0)),
                                             column(width = 6,
                                                    numericInput(inputId = "single_season_data_experience_max",
                                                                 label = "Experience (Max):",
                                                                 min = 0,
                                                                 max = 20,
                                                                 value = 20)),),
                                    ),
                                box(width = 9,
                                    plotOutput(outputId = "single_season_data_graph")),
                                box(width = 9, offset = 3,
                                    gt_output(outputId = "single_season_data_table")
                                    )
                      ),
                      tabItem(tabName = "dynasty_data",
                              box(width = 3, height = 1160,
                                      fixedRow(width = 3,
                                               column(width = 6,
                                                      selectInput(inputId = "dynasty_data_season_min",
                                                                   label = "Season (Min):",
                                                                   choices = c(2000:2022),
                                                                   selected = 2019)),
                                               column(width = 6,
                                                      selectInput(inputId = "dynasty_data_season_max",
                                                                   label = "Season (Max):",
                                                                   choices = c(2000:2022),
                                                                   selected = 2021)),),
                                  selectInput(inputId = "dynasty_data_position",
                                                     label = "Positions",
                                                     choices = c("QB","RB","WR","TE"),
                                                     selected = c("RB","WR","TE"),
                                              multiple = TRUE),
                                  selectInput(inputId = "dynasty_data_sort",
                                              label = "Sort By:",
                                              choices = c("Rank" = "dynasty_rank",
                                                          "LW Percentage (Mean)" = "rankLWMean",
                                                          "LW Percentage (Median)" = "rankLWMed",
                                                          "LW Percentage (Mean x Median)" = "medxmedian_rank",
                                                          "League Winner (Count)" = "rankLWC",
                                                          "Successful (Count)" = "successful_rank",
                                                          "Season Valid (Count)" = "rankSVC",
                                                          "LWS (Total)" = "LWS_rank",
                                                          "PPR (Total)" = "PPR_rank",
                                                          "30+ Games (Total)" = "rank30",
                                                          "25+ Games (Total" = "rank25",
                                                          "20+ Games (Total)" = "rank20",
                                                          "Top 10 Games (Total)" = "rankT10"),
                                              selected = "Rank"),
                                  selectInput(inputId = "dynasty_data_graph_colour",
                                              label = "Graph Highlights:",
                                              choices = c("League Winner" = "graph_col_lw",
                                                          "Position" = "Position"),
                                              selected = c("League Winner")),
                                  fixedRow(width = 3,
                                           column(width = 6,
                                                  numericInput(inputId = "dynasty_data_seasons_min",
                                                               label = "Seasons Played (Min):",
                                                               min = 1,
                                                               max = 22,
                                                               value = 1)),
                                           column(width = 6,
                                                  numericInput(inputId = "dynasty_data_seasons_max",
                                                               label = "Seasons Played (Max):",
                                                               min = 1,
                                                               max = 22,
                                                               value = 22)),),
                                  fixedRow(width = 3,
                                           column(width = 6,
                                                  numericInput(inputId = "dynasty_data_lw_count_min",
                                                               label = "LW Count (Min):",
                                                               min = 0,
                                                               max = 22,
                                                               value = 0)),
                                           column(width = 6,
                                                  numericInput(inputId = "dynasty_data_lw_count_max",
                                                               label = "LW Count (Max):",
                                                               min = 0,
                                                               max = 22,
                                                               value = 22)),),
                                  fixedRow(width = 3,
                                           column(width = 6,
                                                  numericInput(inputId = "dynasty_data_success_count_min",
                                                               label = "Success Count (Min):",
                                                               min = 0,
                                                               max = 22,
                                                               value = 0)),
                                           column(width = 6,
                                                  numericInput(inputId = "dynasty_data_success_count_max",
                                                               label = "Success Count (Max):",
                                                               min = 0,
                                                               max = 22,
                                                               value = 22)),),
                                  fixedRow(width = 3,
                                           column(width = 6,
                                                  numericInput(inputId = "dynasty_data_sv_count_min",
                                                               label = "Seasons Valid (Min):",
                                                               min = 0,
                                                               max = 22,
                                                               value = 0)),
                                           column(width = 6,
                                                  numericInput(inputId = "dynasty_data_sv_count_max",
                                                               label = "Seasons Valid (Max):",
                                                               min = 0,
                                                               max = 22,
                                                               value = 22)),),
                                  fixedRow(width = 3,
                                           column(width = 6,
                                                  numericInput(inputId = "dynasty_data_lws_min",
                                                               label = "LWS (Min):",
                                                               min = 0,
                                                               max = 425,
                                                               value = 0)),
                                           column(width = 6,
                                                  numericInput(inputId = "dynasty_data_lws_max",
                                                               label = "PPR (Max):",
                                                               min = 0,
                                                               max = 425,
                                                               value = 425)),),
                                  fixedRow(width = 3,
                                           column(width = 6,
                                                  numericInput(inputId = "dynasty_data_ppr_min",
                                                               label = "PPR (Min):",
                                                               min = 0,
                                                               max = 6000,
                                                               value = 0)),
                                           column(width = 6,
                                                  numericInput(inputId = "dynasty_data_ppr_max",
                                                               label = "PPR (Max):",
                                                               min = 0,
                                                               max = 6000,
                                                               value = 6000)),),
                                  fixedRow(width = 3,
                                           column(width = 6,
                                                  numericInput(inputId = "dynasty_data_g30_min",
                                                               label = "30+ Games (Min):",
                                                               min = 0,
                                                               max = 30,
                                                               value = 0)),
                                           column(width = 6,
                                                  numericInput(inputId = "dynasty_data_g30_max",
                                                               label = "30+ Games (Max):",
                                                               min = 0,
                                                               max = 40,
                                                               value = 40)),),
                                  fixedRow(width = 3,
                                           column(width = 6,
                                                  numericInput(inputId = "dynasty_data_g25_min",
                                                               label = "25+ Games (Min):",
                                                               min = 0,
                                                               max = 75,
                                                               value = 0)),
                                           column(width = 6,
                                                  numericInput(inputId = "dynasty_data_g25_max",
                                                               label = "25+ Games (Max):",
                                                               min = 0,
                                                               max = 75,
                                                               value = 75)),),
                                  fixedRow(width = 3,
                                           column(width = 6,
                                                  numericInput(inputId = "dynasty_data_g20_min",
                                                               label = "20+ Games (Min):",
                                                               min = 0,
                                                               max = 150,
                                                               value = 0)),
                                           column(width = 6,
                                                  numericInput(inputId = "dynasty_data_g20_max",
                                                               label = "20+ Games (Max):",
                                                               min = 0,
                                                               max = 150,
                                                               value = 150)),),
                                  fixedRow(width = 3,
                                           column(width = 6,
                                                  numericInput(inputId = "dynasty_data_gt10_min",
                                                               label = "Top 10 Games (Min):",
                                                               min = 0,
                                                               max = 175,
                                                               value = 0)),
                                           column(width = 6,
                                                  numericInput(inputId = "dynasty_data_gt10_max",
                                                               label = "Top 10 Games (Max):",
                                                               min = 0,
                                                               max = 175,
                                                               value = 175)),),
                              ),
                              box(width = 9,
                                  plotOutput(outputId = "dynasty_data_graph")),
                              box(width = 9, offset = 3,
                                  gt_output(outputId = "dynasty_data_table"))),
                      tabItem(tabName = "player_focus",
                              box(width = 3, height = 1160,
                                  fixedRow(width = 3,
                                           column(width = 6,
                                                  selectInput(inputId = "player_focus_data_season_min",
                                                               label = "Season (Min):",
                                                               choices = c(2000:2022),
                                                               selected = 2000)),
                                           column(width = 6,
                                                  selectInput(inputId = "player_focus_data_season_max",
                                                               label = "Season (Max):",
                                                               choices = c(2000:2022),
                                                               selected = 2022)),),
                                  selectInput(inputId = "player_focus_data_player1",
                                              label = "Player 1:",
                                              choices = name_data_vector,
                                              selected = c("Davante Adams")),
                                  selectInput(inputId = "player_focus_data_x_axis",
                                              label = "X Axis:",
                                              choices = c("Season" = "season",
                                                          "League Winner Score" = "LWS",
                                                          "PPR" = "PPR",
                                                          "30+ Games" = "games30",
                                                          "25+ Games" = "games25",
                                                          "20+ Games" = "games20",
                                                          "Top 10 Games" = "gamesT10",
                                                          "LW Percent" = "lw",
                                                          "LWP4 Ranking - Overall" = "LWP4_Ranking",
                                                          "ZR Rankings" = "zr_rankings_adjusted",
                                                          "ADP (by MFL)" = "adp_averagePick",
                                                          "ECR (by FantasyPros)" = "ecr",
                                                          "LW Rank" = "EndofSeason_Rankings",
                                                          "Games Away" = "Games_Away",
                                                          "Season Valid" = "Season_Valid"),
                                              selected = "Season"),
                                  selectInput(inputId = "player_focus_data_y_axis",
                                              label = "Y Axis:",
                                              choices = c("League Winner Score" = "LWS",
                                                          "PPR" = "PPR",
                                                          "30+ Games" = "games30",
                                                          "25+ Games" = "games25",
                                                          "20+ Games" = "games20",
                                                          "Top 10 Games" = "gamesT10",
                                                          "League Winner" = "League_Winner",
                                                          "LW Percent" = "lw",
                                                          "LWP4 Ranking - Overall" = "LWP4_Ranking",
                                                          "ZR Rankings" = "zr_rankings_adjusted",
                                                          "ADP (by MFL)" = "adp_averagePick",
                                                          "ECR (by FantasyPros)" = "ecr",
                                                          "LW Rank" = "EndofSeason_Rankings",
                                                          "Games Away" = "Games_Away",
                                                          "Season Valid" = "Season_Valid"),
                                              selected = "League Winner Score"),
                                  selectInput(inputId = "player_focus_data_positions",
                                              label = "Positions:",
                                              choices = c("QB","RB","WR","TE"),
                                              selected = c("QB", "RB","WR","TE"),
                                              multiple = TRUE),
                                  selectInput(inputId = "player_focus_data_sort",
                                              label = "Sort Table By:",
                                              choices = c("Season" = "season",
                                                          "End of Season Rank" = "EndofSeason_Rankings",
                                                          "LW Percent" = "LWP4_Ranking",
                                                          "ZR Rankings" = "zr_rankings_adjusted",
                                                          "ADP" = "adp_averagePick",
                                                          "FantasyPros ECR" = "ecr",
                                                          "LW Score" = "LWS_rank",
                                                          "PPR" = "PPR_rank",
                                                          "30+" = "g30_rank",
                                                          "25+" = "g25_rank",
                                                          "20+" = "g20_rank",
                                                          "Top 10" = "gT10_rank"),
                                              selected = "Season"),
                                  selectInput(inputId = "player_focus_data_graph_col",
                                              label = "Graph Highlights:",
                                              choices = c("League Winners" = "graph_col_lw",
                                                          "Position" = "graph_col_pos"),
                                              selected = "League Winners"),
                                  fixedRow(width = 3,
                                           column(width = 6,
                                                  numericInput(inputId = "player_focus_data_lwpercent_min",
                                                               label = "LW Percentage Min:",
                                                               min = 0,
                                                               max = 100,
                                                               value = 0,
                                                               width = NA)),
                                           column(width = 6,
                                                  numericInput(inputId = "player_focus_data_lwpercent_max",
                                                               label = "LW Percentage Max:",
                                                               min = 0,
                                                               max = 100,
                                                               value = 100,
                                                               width = NA))),
                                  fixedRow(width = 3,
                                           column(width = 6,
                                                  numericInput(inputId = "player_focus_data_lwp4ranking_min",
                                                               label = "LWP4 Ranking - Overall (Min):",
                                                               min = 1,
                                                               max = 12000,
                                                               value = 1)),
                                           column(width = 6,
                                                  numericInput(inputId = "player_focus_data_lwp4ranking_max",
                                                               label = "LWP4 Ranking - Overall (Max):",
                                                               min = 1,
                                                               max = 12000,
                                                               value = 12000)),),
                                  fixedRow(width = 3,
                                           column(width = 6,
                                                  numericInput(inputId = "player_focus_data_lwp4ranking_filtered_min",
                                                               label = "LWP4 Ranking - Filtered (Min):",
                                                               min = 1,
                                                               max = 12000,
                                                               value = 1)),
                                           column(width = 6,
                                                  numericInput(inputId = "player_focus_data_lwp4ranking_filtered_max",
                                                               label = "LWP4 Ranking - Filtered (Max):",
                                                               min = 1,
                                                               max = 12000,
                                                               value = 12000)),),
                                  fixedRow(width = 3,
                                           column(width = 6,
                                                  numericInput(inputId = "player_focus_data_zr_rankings_min",
                                                               label = "ZR Rankings (Min):",
                                                               min = 1,
                                                               max = 650,
                                                               value = 1)),
                                           column(width = 6,
                                                  numericInput(inputId = "player_focus_data_zr_rankings_max",
                                                               label = "ZR Rankings (Max):",
                                                               min = 1,
                                                               max = 650,
                                                               value = 650)),),
                                  fixedRow(width = 3,
                                           column(width = 6,
                                                  numericInput(inputId = "player_focus_data_adp_min",
                                                               label = "Season ADP by MFL (Min):",
                                                               min = 1,
                                                               max = 275,
                                                               value = 1)),
                                           column(width = 6,
                                                  numericInput(inputId = "player_focus_data_adp_max",
                                                               label = "Season ADP by MFL (Max):",
                                                               min = 1,
                                                               max = 275,
                                                               value = 275)),),
                                  fixedRow(width = 3,
                                           column(width = 6,
                                                  numericInput(inputId = "player_focus_data_ecr_min",
                                                               label = "Season ECR by FantasyPros (Min):",
                                                               min = 1,
                                                               max = 650,
                                                               value = 1)),
                                           column(width = 6,
                                                  numericInput(inputId = "player_focus_data_ecr_max",
                                                               label = "Season ECR by FantasyPros (Max):",
                                                               min = 1,
                                                               max = 650,
                                                               value = 650)),),
                                  selectInput(inputId = "player_focus_data_lw",
                                              label = "League Winner:",
                                              choices = c("Unfiltered" = 2,
                                                          "Yes" = 0,
                                                          "No" = 1),
                                              selected = 2),
                                  fixedRow(width = 3,
                                           column(width = 6,
                                                  numericInput(inputId = "player_focus_data_gamesaway_min",
                                                               label = "Games Away (Min):",
                                                               min = 0,
                                                               max = 3,
                                                               value = 0)),
                                           column(width = 6,
                                                  numericInput(inputId = "player_focus_data_gamesaway_max",
                                                               label = "Games Away (Max):",
                                                               min = 0,
                                                               max = 3,
                                                               value = 3)),),
                                  selectInput(inputId = "player_focus_data_sv",
                                              label = "Season Valid:",
                                              choices = c("Unfiltered" = 2,
                                                          "Yes" = 0,
                                                          "No" = 1),
                                              selected = 2),
                                  fixedRow(width = 3,
                                           column(width = 6,
                                                  numericInput(inputId = "player_focus_data_lws_min",
                                                               label = "LWS (Min):",
                                                               min = 0,
                                                               max = 60,
                                                               value = 0)),
                                           column(width = 6,
                                                  numericInput(inputId = "player_focus_data_lws_max",
                                                               label = "LWS (Max):",
                                                               min = 1,
                                                               max = 60,
                                                               value = 60)),),
                                  fixedRow(width = 3,
                                           column(width = 6,
                                                  numericInput(inputId = "player_focus_data_ppr_min",
                                                               label = "PPR (Min):",
                                                               min = 0,
                                                               max = 500,
                                                               value = 0)),
                                           column(width = 6,
                                                  numericInput(inputId = "player_focus_data_ppr_max",
                                                               label = "PPR Max):",
                                                               min = 0,
                                                               max = 500,
                                                               value = 500)),),
                                  fixedRow(width = 3,
                                           column(width = 6,
                                                  numericInput(inputId = "player_focus_data_30_min",
                                                               label = "30+ Games (Min):",
                                                               min = 1,
                                                               max = 10,
                                                               value = 0)),
                                           column(width = 6,
                                                  numericInput(inputId = "player_focus_data_30_max",
                                                               label = "30+ Games (Max):",
                                                               min = 1,
                                                               max = 10,
                                                               value = 10)),),
                                  fixedRow(width = 3,
                                           column(width = 6,
                                                  numericInput(inputId = "player_focus_data_25_min",
                                                               label = "25+ Games (Min):",
                                                               min = 0,
                                                               max = 12,
                                                               value = 0)),
                                           column(width = 6,
                                                  numericInput(inputId = "player_focus_data_25_max",
                                                               label = "25+ Games (Max):",
                                                               min = 0,
                                                               max = 12,
                                                               value = 12)),),
                                  fixedRow(width = 3,
                                           column(width = 6,
                                                  numericInput(inputId = "player_focus_data_20_min",
                                                               label = "20+ Games (Min):",
                                                               min = 1,
                                                               max = 15,
                                                               value = 0)),
                                           column(width = 6,
                                                  numericInput(inputId = "player_focus_data_20_max",
                                                               label = "20+ Games (Max):",
                                                               min = 1,
                                                               max = 15,
                                                               value = 15)),),
                                  fixedRow(width = 3,
                                           column(width = 6,
                                                  numericInput(inputId = "player_focus_data_T10_min",
                                                               label = "Top 10 Games (Min):",
                                                               min = 0,
                                                               max = 15,
                                                               value = 0)),
                                           column(width = 6,
                                                  numericInput(inputId = "player_focus_data_T10_max",
                                                               label = "Top 10 Games (Max):",
                                                               min = 0,
                                                               max = 15,
                                                               value = 15)),),
                                  ),
                              box(width = 9,
                                  plotOutput(outputId = "player_focus_data_graph")),
                              box(width = 9, offset =3,
                                  gt_output(outputId = "player_focus_data_table"))
                              )
                      )
                    )
)


server <- function(input,output,session) {
  output$prediction_data_table <- render_gt(
    lwp4_df %>%
      separate(yearly_id, into = c("year","player_id"), sep = "_") %>%
      mutate(lw_lastyear = ifelse(is.na(gT10_lastyear),NA,lw_lastyear)) %>%
      mutate(PA_SuccessRate = ifelse(is.na(gT10_lastyear),NA,PA_SuccessRate)) %>%
      mutate(sv_lastyear = ifelse(is.na(gT10_lastyear),NA,sv_lastyear)) %>%
      mutate(lw_lastyear_incNA = ifelse(is.na(lw_lastyear),0,lw_lastyear)) %>%
      mutate(PA_SuccessRate_incNA = ifelse(is.na(PA_SuccessRate),0,PA_SuccessRate)) %>%
      mutate(sv_lastyear_incNA = ifelse(is.na(sv_lastyear),0,sv_lastyear)) %>%
      mutate(lw_ever_incNA = ifelse(is.na(lw_ever),0,lw_ever)) %>%
      mutate(lws_lastyear_incNA = ifelse(is.na(lws_lastyear),0,lws_lastyear)) %>%
      mutate(ppr_lastyear_incNA = ifelse(is.na(ppr_lastyear),0,ppr_lastyear)) %>%
      ### FILTER BLACK COUNTER FOR NUMERIC INPUT
      mutate(prediction_data_lwp4ranking_min = ifelse(is.na(input$prediction_data_lwp4ranking_min),1,input$prediction_data_lwp4ranking_min)) %>%
      mutate(prediction_data_lwp4ranking_max = ifelse(is.na(input$prediction_data_lwp4ranking_max),12000,input$prediction_data_lwp4ranking_max)) %>%
      mutate(prediction_data_lwpercent_min = ifelse(is.na(input$prediction_data_lwpercent_min),0/100,input$prediction_data_lwpercent_min/100)) %>%
      mutate(prediction_data_lwpercent_max = ifelse(is.na(input$prediction_data_lwpercent_max),100/100,input$prediction_data_lwpercent_max/100)) %>%
      mutate(prediction_data_lws_lastyear_min = ifelse(is.na(input$prediction_data_lws_lastyear_min),0,input$prediction_data_lws_lastyear_min)) %>%
      mutate(prediction_data_lws_lastyear_max = ifelse(is.na(input$prediction_data_lws_lastyear_max),60,input$prediction_data_lws_lastyear_max)) %>%
      mutate(prediction_data_ppr_lastyear_min = ifelse(is.na(input$prediction_data_ppr_lastyear_min),0,input$prediction_data_ppr_lastyear_min)) %>%
      mutate(prediction_data_ppr_lastyear_max = ifelse(is.na(input$prediction_data_ppr_lastyear_max),60,input$prediction_data_ppr_lastyear_max)) %>%
      mutate(prediction_data_ecr_min = ifelse(is.na(input$prediction_data_ecr_min),1,input$prediction_data_ecr_min)) %>%
      mutate(prediction_data_ecr_max = ifelse(is.na(input$prediction_data_ecr_max),650,input$prediction_data_ecr_max)) %>%
      mutate(prediction_data_adp_min = ifelse(is.na(input$prediction_data_adp_min),1,input$prediction_data_adp_min)) %>%
      mutate(prediction_data_adp_max = ifelse(is.na(input$prediction_data_adp_max),275,input$prediction_data_adp_max)) %>%
      mutate(prediction_data_zr_rankings_min = ifelse(is.na(input$prediction_data_zr_rankings_min),1,input$prediction_data_zr_rankings_min)) %>%
      mutate(prediction_data_zr_rankings_max = ifelse(is.na(input$prediction_data_zr_rankings_max),650,input$prediction_data_zr_rankings_max)) %>%
      mutate(prediction_data_experience_min = ifelse(is.na(input$prediction_data_experience_min),0,input$prediction_data_experience_min)) %>%
      mutate(prediction_data_experience_max = ifelse(is.na(input$prediction_data_experience_max),20,input$prediction_data_experience_max)) %>%
      filter(season %in% input$prediction_data_season_slider,position %in% input$prediction_data_position,
             !(lw_lastyear_incNA %in% input$prediction_data_lw_lastyear),!(PA_SuccessRate_incNA %in% input$prediction_data_successful),
             !(sv_lastyear_incNA %in% input$prediction_data_season_valid),!(lw_ever_incNA %in% input$prediction_data_lw_ever),!(PA_Team_Drafted %in% input$prediction_data_team_drafted),!(team_traded %in% input$prediction_data_team_traded),!(PA_Team_Change %in% input$prediction_data_changed_team),
             !(PA_HC_Change %in% input$prediction_data_changed_hc),!(PA_Season_Skipped %in% input$prediction_data_skipped_season),
             LWP4_Ranking >= prediction_data_lwp4ranking_min, LWP4_Ranking <= prediction_data_lwp4ranking_max,
             lw >= prediction_data_lwpercent_min, lw <= prediction_data_lwpercent_max,
             lws_lastyear_incNA >= prediction_data_lws_lastyear_min, lws_lastyear_incNA <= prediction_data_lws_lastyear_max,
             ppr_lastyear_incNA >= prediction_data_ppr_lastyear_min, ppr_lastyear_incNA <= prediction_data_ppr_lastyear_max,
             adp_averagePick >= prediction_data_adp_min, adp_averagePick <= prediction_data_adp_max,
             ecr >= prediction_data_ecr_min, ecr <= prediction_data_ecr_max,
             zr_rankings_adjusted >= prediction_data_zr_rankings_min, zr_rankings_adjusted <= prediction_data_zr_rankings_max,
             experience >= prediction_data_experience_min, experience <= prediction_data_experience_max
             ) %>%
      arrange(LWP4_Ranking) %>%
      mutate(LWP4_Ranking_filtered = row_number()) %>%
      mutate(prediction_data_lwp4ranking_filtered_min = ifelse(is.na(input$prediction_data_lwp4ranking_filtered_min),1,input$prediction_data_lwp4ranking_filtered_min)) %>%
      mutate(prediction_data_lwp4ranking_filtered_max = ifelse(is.na(input$prediction_data_lwp4ranking_filtered_max),12000,input$prediction_data_lwp4ranking_filtered_max)) %>%
      filter(LWP4_Ranking_filtered >= prediction_data_lwp4ranking_filtered_min, LWP4_Ranking_filtered <= prediction_data_lwp4ranking_filtered_max) %>%
      mutate(LWP4_Ranking_filtered = ifelse(is.na(adp_averagePick),NA,LWP4_Ranking_filtered)) %>%
      mutate(adp_averagePick = ifelse(adp_averagePick == adp_undrafted,NA,adp_averagePick)) %>%
      mutate(ecr = ifelse(ecr == ecr_unranked,NA,ecr)) %>%
      ### OFFSEASON NAs
      mutate(League_Winner = ifelse(season == 2022, NA, League_Winner)) %>%
      mutate(Season_Valid = ifelse(season == 2022, NA, Season_Valid)) %>%
      mutate(Games_Away = ifelse(season == 2022, NA, Games_Away)) %>%
      mutate(LWS = ifelse(season == 2022, NA, LWS)) %>%
      mutate(PPR = ifelse(season == 2022, NA, PPR)) %>%
      mutate(games30 = ifelse(season == 2022, NA, games30)) %>%
      mutate(games25 = ifelse(season == 2022, NA, games25)) %>%
      mutate(games20 = ifelse(season == 2022, NA, games20)) %>%
      mutate(gamesT10 = ifelse(season == 2022, NA, gamesT10)) %>%
      mutate(EndofSeason_Rankings = ifelse(season == 2022, NA, EndofSeason_Rankings)) %>%
      mutate(League_Winner = ifelse(season == 2022, NA, League_Winner)) %>%
      mutate(team_investment = PA_Team_Drafted + team_traded) %>%
      mutate(team_investment = ifelse(team_investment >1,1,0)) %>%
      select(player_id,season,full_name,position,lw,LWP4_Ranking,yearly_rank,LWP4_Ranking_filtered,adp_averagePick,ecr,zr_rankings_adjusted,compare_p2,lw_lastyear,PA_SuccessRate,sv_lastyear,lws_lastyear,ppr_lastyear,g30_lastyear,g25_lastyear,g20_lastyear,gT10_lastyear,lw_ever,team_investment,PA_Team_Drafted,PA_Team_Change,team_traded,PA_HC_Change,PA_Season_Skipped,Position_order,g30_rank,g25_rank,g20_rank,gT10_rank,PPR_rank,LWS_rank,yearly_rank_per,lwp4_comp_rank) %>%
      arrange(.data[[input$prediction_data_sort]],LWP4_Ranking) %>%
      gt() %>%
      gt_highlight_rows(rows = yearly_rank_per < 0.75 & compare_p2 == 1, fill = "lightcoral") %>%
      gt_highlight_rows(rows = yearly_rank_per < 0.5 & compare_p2 == 1, fill = "Firebrick") %>%
      gt_highlight_rows(rows = yearly_rank_per > 1.25 & compare_p2 == 1) %>%
      gt_highlight_rows(rows = yearly_rank_per > 1.5 & compare_p2 == 1, fill = "royalblue") %>%
      cols_align(align = c("center")) %>%
      cols_hide(c(Position_order,g30_rank,g25_rank,g20_rank,gT10_rank,PPR_rank,LWS_rank,compare_p2,yearly_rank_per,lwp4_comp_rank,team_traded,PA_Team_Drafted)) %>%
      cols_label(player_id = "Player ID",
                 season = "Season",
                 full_name = "Player Name",
                 position = "Position",
                 lw = "LW Percentage",
                 LWP4_Ranking = "LWP4 Ranking - Overall",
                 adp_averagePick = "ADP (MFL)",
                 ecr = "FantasyPros Ranking (ECR)",
                 lws_lastyear = "LWS (Last Year)",
                 ppr_lastyear = "PPR (LY)",
                 lw_lastyear = "League Winner (LY)",
                 gT10_lastyear = "Top 10 (LY)",
                 g20_lastyear = "20+ (LY)",
                 g30_lastyear = "30+ (LY)",
                 g25_lastyear = "25+ (LY)",
                 lw_ever = "League Winner Ever",
                 PA_SuccessRate = "Successful (LY)",
                 team_investment = "Team Invested",
                 sv_lastyear = "Season Valid (LY)",
                 PA_Season_Skipped = "Skipped Season (LY)",
                 PA_HC_Change = "Changed HC",
                 PA_Team_Change = "Changed Team",
                 LWP4_Ranking_filtered = "LWP4 Ranking - Filtered",
                 yearly_rank = "LWP4 Ranking - Yearly",
                 zr_rankings_adjusted = "ZR Rankings") %>%
      cols_move(zr_rankings_adjusted,LWP4_Ranking_filtered) %>%
      fmt_percent(lw) %>%
      tab_style(
        style = list(
          cell_borders(
            sides = "bottom",
            color = "black",
            weight = px(3)
          )
        ),
        locations = list(
          cells_column_labels())) %>%
      tab_style(
        style = list(
          cell_borders(
            sides = "right",
            color = "black",
            weight = px(3)
          )
        ),
        locations = list(
          cells_body(
            columns = c(position)))) %>%
      tab_style(
        style = list(
          cell_borders(
            sides = "right",
            color = "black",
            weight = px(3)
          )
        ),
        locations = list(
          cells_body(
            columns = c(LWP4_Ranking_filtered)))) %>%
      tab_style(
        style = list(
          cell_borders(
            sides = "left",
            color = "black",
            weight = px(3)
          )
        ),
        locations = list(
          cells_body(
            columns = c(lw_ever)))) %>%
      tab_style(
        style = list(
          cell_borders(
            sides = "right",
            color = "black",
            weight = px(3)
          )
        ),
        locations = list(
          cells_body(
            columns = c(ecr))))
  )
#### ABCDEF
  output$single_season_data_graph <- renderPlot(
    lwp4_df %>%
      separate(yearly_id, into = c("year","player_id"), sep = "_") %>%
      mutate(lw_lastyear = ifelse(is.na(gT10_lastyear),NA,lw_lastyear)) %>%
      mutate(PA_SuccessRate = ifelse(is.na(gT10_lastyear),NA,PA_SuccessRate)) %>%
      mutate(sv_lastyear = ifelse(is.na(gT10_lastyear),NA,sv_lastyear)) %>%
      mutate(lw_lastyear_incNA = ifelse(is.na(lw_lastyear),0,lw_lastyear)) %>%
      mutate(PA_SuccessRate_incNA = ifelse(is.na(PA_SuccessRate),0,PA_SuccessRate)) %>%
      mutate(sv_lastyear_incNA = ifelse(is.na(sv_lastyear),0,sv_lastyear)) %>%
      mutate(lw_ever_incNA = ifelse(is.na(lw_ever),0,lw_ever)) %>%
      mutate(lws_lastyear_incNA = ifelse(is.na(lws_lastyear),0,lws_lastyear)) %>%
      mutate(ppr_lastyear_incNA = ifelse(is.na(ppr_lastyear),0,ppr_lastyear)) %>%
      mutate(single_season_data_season_min = ifelse(is.na(input$single_season_data_season_min),2000,input$single_season_data_season_min)) %>%
      mutate(single_season_data_season_max = ifelse(is.na(input$single_season_data_season_max),2022,input$single_season_data_season_max)) %>%
      mutate(single_season_data_lwpercent_min = ifelse(is.na(input$single_season_data_lwpercent_min),0/100,input$single_season_data_lwpercent_min/100)) %>%
      mutate(single_season_data_lwpercent_max = ifelse(is.na(input$single_season_data_lwpercent_max),100/100,input$single_season_data_lwpercent_max/100)) %>%
      mutate(single_season_data_lwp4ranking_min = ifelse(is.na(input$single_season_data_lwp4ranking_min),1,input$single_season_data_lwp4ranking_min)) %>%
      mutate(single_season_data_lwp4ranking_max = ifelse(is.na(input$single_season_data_lwp4ranking_max),12000,input$single_season_data_lwp4ranking_max)) %>%
      mutate(single_season_data_adp_min = ifelse(is.na(input$single_season_data_adp_min),1,input$single_season_data_adp_min)) %>%
      mutate(single_season_data_adp_max = ifelse(is.na(input$single_season_data_adp_max),275,input$single_season_data_adp_max)) %>%
      mutate(single_season_data_ecr_min = ifelse(is.na(input$single_season_data_ecr_min),1,input$single_season_data_ecr_min)) %>%
      mutate(single_season_data_ecr_max = ifelse(is.na(input$single_season_data_ecr_max),650,input$single_season_data_ecr_max)) %>%
      mutate(single_season_data_gamesaway_min = ifelse(is.na(input$single_season_data_gamesaway_min),0,input$single_season_data_gamesaway_min)) %>%
      mutate(single_season_data_gamesaway_max = ifelse(is.na(input$single_season_data_gamesaway_max),3,input$single_season_data_gamesaway_max)) %>%
      mutate(single_season_data_lws_min = ifelse(is.na(input$single_season_data_lws_min),0,input$single_season_data_lws_min)) %>%
      mutate(single_season_data_lws_max = ifelse(is.na(input$single_season_data_lws_max),60,input$single_season_data_lws_max)) %>%
      mutate(single_season_data_ppr_min = ifelse(is.na(input$single_season_data_ppr_min),0,input$single_season_data_ppr_min)) %>%
      mutate(single_season_data_ppr_max = ifelse(is.na(input$single_season_data_ppr_max),60,input$single_season_data_ppr_max)) %>%
      mutate(single_season_data_30_min = ifelse(is.na(input$single_season_data_30_min),0,input$single_season_data_30_min)) %>%
      mutate(single_season_data_30_max = ifelse(is.na(input$single_season_data_30_max),10,input$single_season_data_30_max)) %>%
      mutate(single_season_data_25_min = ifelse(is.na(input$single_season_data_25_min),0,input$single_season_data_25_min)) %>%
      mutate(single_season_data_25_max = ifelse(is.na(input$single_season_data_25_max),12,input$single_season_data_25_max)) %>%
      mutate(single_season_data_20_min = ifelse(is.na(input$single_season_data_20_min),0,input$single_season_data_20_min)) %>%
      mutate(single_season_data_20_max = ifelse(is.na(input$single_season_data_20_max),15,input$single_season_data_20_max)) %>%
      mutate(single_season_data_T10_min = ifelse(is.na(input$single_season_data_T10_min),0,input$single_season_data_T10_min)) %>%
      mutate(single_season_data_T10_max = ifelse(is.na(input$single_season_data_T10_max),15,input$single_season_data_T10_max)) %>%
      mutate(single_season_data_zr_rankings_min = ifelse(is.na(input$single_season_data_zr_rankings_min),1,input$single_season_data_zr_rankings_min)) %>%
      mutate(single_season_data_zr_rankings_max = ifelse(is.na(input$single_season_data_zr_rankings_max),650,input$single_season_data_zr_rankings_max)) %>%
      mutate(single_season_data_experience_min = ifelse(is.na(input$single_season_data_experience_min),0,input$single_season_data_experience_min)) %>%
      mutate(single_season_data_experience_max = ifelse(is.na(input$single_season_data_experience_max),20,input$single_season_data_experience_max)) %>%
      mutate(single_season_data_lws_lastyear_min = ifelse(is.na(input$single_season_data_lws_lastyear_min),0,input$single_season_data_lws_lastyear_min)) %>%
      mutate(single_season_data_lws_lastyear_max = ifelse(is.na(input$single_season_data_lws_lastyear_max),60,input$single_season_data_lws_lastyear_max)) %>%
      mutate(single_season_data_ppr_lastyear_min = ifelse(is.na(input$single_season_data_ppr_lastyear_min),0,input$single_season_data_ppr_lastyear_min)) %>%
      mutate(single_season_data_ppr_lastyear_max = ifelse(is.na(input$single_season_data_ppr_lastyear_max),60,input$single_season_data_ppr_lastyear_max)) %>%
      mutate(lw_incNA = ifelse(is.na(lw),0,lw)) %>%
      mutate(LWP4_Ranking_incNA = ifelse(is.na(LWP4_Ranking),12000,LWP4_Ranking)) %>%
      mutate(adp_averagePick_incNA = ifelse(is.na(lw),275,adp_averagePick)) %>%
      mutate(ecr_incNA = ifelse(is.na(lw),650,ecr)) %>%
      filter(season >= input$single_season_data_season_min, season <= input$single_season_data_season_max,
             position %in% input$single_season_data_positions,
             lw_incNA >= single_season_data_lwpercent_min, lw_incNA <= single_season_data_lwpercent_max,
             LWP4_Ranking_incNA >= single_season_data_lwp4ranking_min, LWP4_Ranking_incNA <= single_season_data_lwp4ranking_max,
             adp_averagePick_incNA >= single_season_data_adp_min, adp_averagePick_incNA <= single_season_data_adp_max,
             ecr_incNA >= single_season_data_ecr_min, ecr_incNA <= single_season_data_ecr_max,
             !(League_Winner %in% input$single_season_data_lw),
             Games_Away >= single_season_data_gamesaway_min, Games_Away <= single_season_data_gamesaway_max,
             !(Season_Valid %in% input$single_season_data_sv),
             !(comparison_valid %in% input$single_season_data_comp_valid),
             LWS >= single_season_data_lws_min, LWS <= single_season_data_lws_max,
             PPR >= single_season_data_ppr_min, PPR <= single_season_data_ppr_max,
             games30 >= single_season_data_30_min, games30 <= single_season_data_30_max,
             games25 >= single_season_data_25_min, games25 <= single_season_data_25_max,
             games20 >= single_season_data_20_min, games20 <= single_season_data_20_max,
             gamesT10 >= single_season_data_T10_min, gamesT10 <= single_season_data_T10_max,
             zr_rankings_adjusted >= single_season_data_zr_rankings_min, zr_rankings_adjusted <= single_season_data_zr_rankings_max,
             experience >= single_season_data_experience_min, experience <= single_season_data_experience_max,
             lws_lastyear_incNA >= single_season_data_lws_lastyear_min, lws_lastyear_incNA <= single_season_data_lws_lastyear_max,
             ppr_lastyear_incNA >= single_season_data_ppr_lastyear_min, ppr_lastyear_incNA <= single_season_data_ppr_lastyear_max,
             !(lw_lastyear_incNA %in% input$single_season_data_lw_lastyear),!(PA_SuccessRate_incNA %in% input$single_season_data_successful),
             !(sv_lastyear_incNA %in% input$single_season_data_season_valid),!(lw_ever_incNA %in% input$single_season_data_lw_ever),
             !(lw_ever_incNA %in% input$single_season_data_lw_ever),!(PA_Team_Drafted %in% input$single_season_data_team_drafted),!(team_traded %in% input$single_season_data_team_traded),!(PA_Team_Change %in% input$single_season_data_changed_team),
             !(PA_HC_Change %in% input$single_season_data_changed_hc),!(PA_Season_Skipped %in% input$single_season_data_skipped_season),
      ) %>%
      arrange(LWP4_Ranking_incNA) %>%
      mutate(LWP4_Ranking_filtered = row_number()) %>%
      mutate(single_season_data_lwp4ranking_filtered_min = ifelse(is.na(input$single_season_data_lwp4ranking_filtered_min),1,input$single_season_data_lwp4ranking_filtered_min)) %>%
      mutate(single_season_data_lwp4ranking_filtered_max = ifelse(is.na(input$single_season_data_lwp4ranking_filtered_max),12000,input$single_season_data_lwp4ranking_filtered_max)) %>%
      filter(LWP4_Ranking_filtered >= single_season_data_lwp4ranking_filtered_min, LWP4_Ranking_filtered <= single_season_data_lwp4ranking_filtered_max) %>%
      ggplot(aes(x = .data[[input$single_season_data_x_axis]], y = .data[[input$single_season_data_y_axis]], col = graph_col_lw)) +
      geom_point(size = 5) +
      scale_colour_identity() +
      #xlim(single_season_graph_x_axis_limits_min,single_season_graph_x_axis_limits_max) +
      #ylim(single_season_graph_y_axis_limits_min,single_season_graph_y_axis_limits_max) +
      theme_bw() +
      theme(axis.title = element_blank())
  )
  output$single_season_data_table <- render_gt(
    lwp4_df %>%
      separate(yearly_id, into = c("year","player_id"), sep = "_") %>%
      mutate(lw_lastyear = ifelse(is.na(gT10_lastyear),NA,lw_lastyear)) %>%
      mutate(PA_SuccessRate = ifelse(is.na(gT10_lastyear),NA,PA_SuccessRate)) %>%
      mutate(sv_lastyear = ifelse(is.na(gT10_lastyear),NA,sv_lastyear)) %>%
      mutate(lw_lastyear_incNA = ifelse(is.na(lw_lastyear),0,lw_lastyear)) %>%
      mutate(PA_SuccessRate_incNA = ifelse(is.na(PA_SuccessRate),0,PA_SuccessRate)) %>%
      mutate(sv_lastyear_incNA = ifelse(is.na(sv_lastyear),0,sv_lastyear)) %>%
      mutate(lw_ever_incNA = ifelse(is.na(lw_ever),0,lw_ever)) %>%
      mutate(lws_lastyear_incNA = ifelse(is.na(lws_lastyear),0,lws_lastyear)) %>%
      mutate(ppr_lastyear_incNA = ifelse(is.na(ppr_lastyear),0,ppr_lastyear)) %>%
      mutate(single_season_data_season_min = ifelse(is.na(input$single_season_data_season_min),2000,input$single_season_data_season_min)) %>%
      mutate(single_season_data_season_max = ifelse(is.na(input$single_season_data_season_max),2022,input$single_season_data_season_max)) %>%
      mutate(single_season_data_lwpercent_min = ifelse(is.na(input$single_season_data_lwpercent_min),0/100,input$single_season_data_lwpercent_min/100)) %>%
      mutate(single_season_data_lwpercent_max = ifelse(is.na(input$single_season_data_lwpercent_max),100/100,input$single_season_data_lwpercent_max/100)) %>%
      mutate(single_season_data_lwp4ranking_min = ifelse(is.na(input$single_season_data_lwp4ranking_min),1,input$single_season_data_lwp4ranking_min)) %>%
      mutate(single_season_data_lwp4ranking_max = ifelse(is.na(input$single_season_data_lwp4ranking_max),12000,input$single_season_data_lwp4ranking_max)) %>%
      mutate(single_season_data_adp_min = ifelse(is.na(input$single_season_data_adp_min),1,input$single_season_data_adp_min)) %>%
      mutate(single_season_data_adp_max = ifelse(is.na(input$single_season_data_adp_max),275,input$single_season_data_adp_max)) %>%
      mutate(single_season_data_ecr_min = ifelse(is.na(input$single_season_data_ecr_min),1,input$single_season_data_ecr_min)) %>%
      mutate(single_season_data_ecr_max = ifelse(is.na(input$single_season_data_ecr_max),650,input$single_season_data_ecr_max)) %>%
      mutate(single_season_data_gamesaway_min = ifelse(is.na(input$single_season_data_gamesaway_min),0,input$single_season_data_gamesaway_min)) %>%
      mutate(single_season_data_gamesaway_max = ifelse(is.na(input$single_season_data_gamesaway_max),3,input$single_season_data_gamesaway_max)) %>%
      mutate(single_season_data_lws_min = ifelse(is.na(input$single_season_data_lws_min),0,input$single_season_data_lws_min)) %>%
      mutate(single_season_data_lws_max = ifelse(is.na(input$single_season_data_lws_max),60,input$single_season_data_lws_max)) %>%
      mutate(single_season_data_ppr_min = ifelse(is.na(input$single_season_data_ppr_min),0,input$single_season_data_ppr_min)) %>%
      mutate(single_season_data_ppr_max = ifelse(is.na(input$single_season_data_ppr_max),60,input$single_season_data_ppr_max)) %>%
      mutate(single_season_data_30_min = ifelse(is.na(input$single_season_data_30_min),0,input$single_season_data_30_min)) %>%
      mutate(single_season_data_30_max = ifelse(is.na(input$single_season_data_30_max),10,input$single_season_data_30_max)) %>%
      mutate(single_season_data_25_min = ifelse(is.na(input$single_season_data_25_min),0,input$single_season_data_25_min)) %>%
      mutate(single_season_data_25_max = ifelse(is.na(input$single_season_data_25_max),12,input$single_season_data_25_max)) %>%
      mutate(single_season_data_20_min = ifelse(is.na(input$single_season_data_20_min),0,input$single_season_data_20_min)) %>%
      mutate(single_season_data_20_max = ifelse(is.na(input$single_season_data_20_max),15,input$single_season_data_20_max)) %>%
      mutate(single_season_data_T10_min = ifelse(is.na(input$single_season_data_T10_min),0,input$single_season_data_T10_min)) %>%
      mutate(single_season_data_T10_max = ifelse(is.na(input$single_season_data_T10_max),15,input$single_season_data_T10_max)) %>%
      mutate(single_season_data_zr_rankings_min = ifelse(is.na(input$single_season_data_zr_rankings_min),1,input$single_season_data_zr_rankings_min)) %>%
      mutate(single_season_data_zr_rankings_max = ifelse(is.na(input$single_season_data_zr_rankings_max),650,input$single_season_data_zr_rankings_max)) %>%
      mutate(single_season_data_experience_min = ifelse(is.na(input$single_season_data_experience_min),0,input$single_season_data_experience_min)) %>%
      mutate(single_season_data_experience_max = ifelse(is.na(input$single_season_data_experience_max),20,input$single_season_data_experience_max)) %>%
      mutate(single_season_data_lws_lastyear_min = ifelse(is.na(input$single_season_data_lws_lastyear_min),0,input$single_season_data_lws_lastyear_min)) %>%
      mutate(single_season_data_lws_lastyear_max = ifelse(is.na(input$single_season_data_lws_lastyear_max),60,input$single_season_data_lws_lastyear_max)) %>%
      mutate(single_season_data_ppr_lastyear_min = ifelse(is.na(input$single_season_data_ppr_lastyear_min),0,input$single_season_data_ppr_lastyear_min)) %>%
      mutate(single_season_data_ppr_lastyear_max = ifelse(is.na(input$single_season_data_ppr_lastyear_max),60,input$single_season_data_ppr_lastyear_max)) %>%
      mutate(lw_incNA = ifelse(is.na(lw),0,lw)) %>%
      mutate(LWP4_Ranking_incNA = ifelse(is.na(LWP4_Ranking),12000,LWP4_Ranking)) %>%
      mutate(adp_averagePick_incNA = ifelse(is.na(lw),275,adp_averagePick)) %>%
      mutate(ecr_incNA = ifelse(is.na(lw),650,ecr)) %>%
      filter(season >= input$single_season_data_season_min, season <= input$single_season_data_season_max,
             position %in% input$single_season_data_positions,
             lw_incNA >= single_season_data_lwpercent_min, lw_incNA <= single_season_data_lwpercent_max,
             LWP4_Ranking_incNA >= single_season_data_lwp4ranking_min, LWP4_Ranking_incNA <= single_season_data_lwp4ranking_max,
             adp_averagePick_incNA >= single_season_data_adp_min, adp_averagePick_incNA <= single_season_data_adp_max,
             ecr_incNA >= single_season_data_ecr_min, ecr_incNA <= single_season_data_ecr_max,
             !(League_Winner %in% input$single_season_data_lw),
             Games_Away >= single_season_data_gamesaway_min, Games_Away <= single_season_data_gamesaway_max,
             !(Season_Valid %in% input$single_season_data_sv),
             !(comparison_valid %in% input$single_season_data_comp_valid),
             LWS >= single_season_data_lws_min, LWS <= single_season_data_lws_max,
             PPR >= single_season_data_ppr_min, PPR <= single_season_data_ppr_max,
             games30 >= single_season_data_30_min, games30 <= single_season_data_30_max,
             games25 >= single_season_data_25_min, games25 <= single_season_data_25_max,
             games20 >= single_season_data_20_min, games20 <= single_season_data_20_max,
             gamesT10 >= single_season_data_T10_min, gamesT10 <= single_season_data_T10_max,
             zr_rankings_adjusted >= single_season_data_zr_rankings_min, zr_rankings_adjusted <= single_season_data_zr_rankings_max,
             experience >= single_season_data_experience_min, experience <= single_season_data_experience_max,
             lws_lastyear_incNA >= single_season_data_lws_lastyear_min, lws_lastyear_incNA <= single_season_data_lws_lastyear_max,
             ppr_lastyear_incNA >= single_season_data_ppr_lastyear_min, ppr_lastyear_incNA <= single_season_data_ppr_lastyear_max,
             !(lw_lastyear_incNA %in% input$single_season_data_lw_lastyear),!(PA_SuccessRate_incNA %in% input$single_season_data_successful),
             !(sv_lastyear_incNA %in% input$single_season_data_season_valid),!(lw_ever_incNA %in% input$single_season_data_lw_ever),
             !(lw_ever_incNA %in% input$single_season_data_lw_ever),!(PA_Team_Drafted %in% input$single_season_data_team_drafted),!(team_traded %in% input$single_season_data_team_traded),!(PA_Team_Change %in% input$single_season_data_changed_team),
             !(PA_HC_Change %in% input$single_season_data_changed_hc),!(PA_Season_Skipped %in% input$single_season_data_skipped_season),
      ) %>%
      arrange(LWP4_Ranking_incNA) %>%
      mutate(LWP4_Ranking_filtered = row_number()) %>%
      mutate(single_season_data_lwp4ranking_filtered_min = ifelse(is.na(input$single_season_data_lwp4ranking_filtered_min),1,input$single_season_data_lwp4ranking_filtered_min)) %>%
      mutate(single_season_data_lwp4ranking_filtered_max = ifelse(is.na(input$single_season_data_lwp4ranking_filtered_max),12000,input$single_season_data_lwp4ranking_filtered_max)) %>%
      filter(LWP4_Ranking_filtered >= single_season_data_lwp4ranking_filtered_min, LWP4_Ranking_filtered <= single_season_data_lwp4ranking_filtered_max) %>%
      arrange(EndofSeason_Rankings) %>%
      mutate(EoS_Rankings_Filtered = row_number()) %>%
      mutate(LWP4_Ranking_filtered = ifelse(is.na(adp_averagePick),NA,LWP4_Ranking_filtered)) %>%
      mutate(adp_averagePick = ifelse(adp_averagePick == adp_undrafted,NA,adp_averagePick)) %>%
      mutate(ecr = ifelse(ecr == ecr_unranked,NA,ecr)) %>%
      mutate(EndofSeason_Rankings = ifelse(season == 2022,NA,EndofSeason_Rankings)) %>%
      mutate(EOS_yearly = ifelse(season == 2022,NA,EOS_yearly)) %>%
      mutate(EoS_Rankings_Filtered = ifelse(season == 2022,NA,EoS_Rankings_Filtered)) %>%
      mutate(League_Winner = ifelse(season == 2022,NA,League_Winner)) %>%
      mutate(Games_Away = ifelse(season == 2022,NA,Games_Away)) %>%
      mutate(Season_Valid = ifelse(season == 2022,NA,Season_Valid)) %>%
      mutate(games30 = ifelse(season == 2022,NA,games30)) %>%
      mutate(games25 = ifelse(season == 2022,NA,games25)) %>%
      mutate(games20 = ifelse(season == 2022,NA,games20)) %>%
      mutate(gamesT10 = ifelse(season == 2022,NA,gamesT10)) %>%
      mutate(LWS = ifelse(season == 2022,NA,LWS)) %>%
      mutate(PPR = ifelse(season == 2022,NA,PPR)) %>%
      mutate(team_investment = PA_Team_Drafted + team_traded) %>%
      mutate(team_investment = ifelse(team_investment >1,1,0)) %>%
      select(player_id,season,full_name,position,Position_order,lw,LWP4_Ranking,yearly_rank,yearly_rank_per,LWP4_Ranking_filtered,zr_rankings_adjusted,adp_averagePick,ecr,compare_p2,EndofSeason_Rankings,EOS_yearly,EoS_Rankings_Filtered,League_Winner,Games_Away,Season_Valid,LWS,PPR,games30,games25,games20,gamesT10,g30_rank,g25_rank,g20_rank,gT10_rank,PPR_rank,LWS_rank,lwp4_comp_rank) %>%
      arrange(.data[[input$single_season_data_sort]],EndofSeason_Rankings) %>%
      gt() %>%
      gt_highlight_rows(rows = yearly_rank_per < 0.75 & compare_p2 == 1, fill = "lightcoral") %>%
      gt_highlight_rows(rows = yearly_rank_per < 0.5 & compare_p2 == 1, fill = "Firebrick") %>%
      gt_highlight_rows(rows = yearly_rank_per > 1.25 & compare_p2 == 1) %>%
      gt_highlight_rows(rows = yearly_rank_per > 1.5 & compare_p2 == 1, fill = "royalblue") %>%
      cols_hide(c(Position_order,g30_rank,g25_rank,g20_rank,gT10_rank,PPR_rank,LWS_rank,compare_p2,yearly_rank_per,lwp4_comp_rank)) %>%
      cols_label(player_id = "Player ID",
                 season = "Season",
                 full_name = "Player Name",
                 position = "Position",
                 lw = "LW Percentage",
                 LWP4_Ranking = "LWP4 Ranking - Overall",
                 LWP4_Ranking_filtered = "LWP4 Ranking - Filtered",
                 adp_averagePick = "ADP (MFL)",
                 ecr = "FantasyPros Ranking (ECR)",
                 EndofSeason_Rankings = "LW Rank - Overall",
                 EoS_Rankings_Filtered = "LW Rank - Filtered",
                 League_Winner = "League Winner",
                 Games_Away = "Games Away",
                 Season_Valid = "Season Valid",
                 games30 = "30+",
                 games25 = "25+",
                 games20 = "20+",
                 gamesT10 = "Top 10",
                 LWS = "LW Score",
                 yearly_rank = "LWP4 Ranking - Yearly",
                 EOS_yearly = "LW Rank - Yearly",
                 zr_rankings_adjusted = "ZR Rankings") %>%
      cols_align(align = ("center")) %>%
      fmt_percent(lw) %>%
      tab_style(
        style = list(
          cell_borders(
            sides = "right",
            color = "black",
            weight = px(3)
          )
        ),
        locations = list(
          cells_body(
            columns = c(position)))) %>%
      tab_style(
        style = list(
          cell_borders(
            sides = "right",
            color = "black",
            weight = px(3)
          )
        ),
        locations = list(
          cells_body(
            columns = c(EoS_Rankings_Filtered)))) %>%
      tab_style(
        style = list(
          cell_borders(
            sides = "right",
            color = "black",
            weight = px(3)
          )
        ),
        locations = list(
          cells_body(
            columns = c(LWP4_Ranking_filtered)))) %>%
      tab_style(
        style = list(
          cell_borders(
            sides = "left",
            color = "black",
            weight = px(3)
          )
        ),
        locations = list(
          cells_body(
            columns = c(EndofSeason_Rankings)))) %>%
      tab_style(
        style = list(
          cell_borders(
            sides = "bottom",
            color = "black",
            weight = px(3)
          )
        ),
        locations = list(
          cells_column_labels()))
  )
  ### XYZ
  output$dynasty_data_graph <- renderPlot(
    ggplot(data = lwp4_df %>%
             filter(season >= input$dynasty_data_season_min, season <= input$dynasty_data_season_max, 
                    position %in% input$dynasty_data_position,
                    ),
             #group_by(player_id) %>%
             #summarise(games30 = sum(games30), games25 = sum(games25), games20 = sum(games20), gamesT10 = sum(gamesT10), LWS = sum(LWS), PPR = sum(PPR)),
           aes(x = PPR, y = LWS, col = graph_col_lw)) +
      geom_point(size = 5) +
      scale_colour_identity() +
      theme_bw() +
      xlim(0,500) +
      ylim(0,60)
  )
  output$dynasty_data_table <- render_gt(
    lwp4_df %>%
      filter(season >= input$dynasty_data_season_min, season <= input$dynasty_data_season_max, 
             position %in% input$dynasty_data_position) %>%
      mutate(successful = ifelse(Games_Away < 2,1,0)) %>%
      group_by(player_id) %>%
      summarise(games30 = sum(games30), games25 = sum(games25), games20 = sum(games20), gamesT10 = sum(gamesT10), LWS = sum(LWS), PPR = sum(PPR), League_Winner_Count = sum(League_Winner), Success_Count = sum(successful),Season_Valid_Count = sum(Season_Valid), lw_mean = mean(lw), lw_med = median(lw), seasons_played = n()) %>%
      mutate(meanXmedian = sqrt(lw_mean*lw_med)) %>%
      ungroup() %>%
      mutate(dynasty_data_lw_count_min = ifelse(is.na(input$dynasty_data_lw_count_min),0,input$dynasty_data_lw_count_min)) %>%
      mutate(dynasty_data_lw_count_max = ifelse(is.na(input$dynasty_data_lw_count_max),22,input$dynasty_data_lw_count_max)) %>%
      mutate(dynasty_data_success_count_min = ifelse(is.na(input$dynasty_data_success_count_min),0,input$dynasty_data_success_count_min)) %>%
      mutate(dynasty_data_success_count_max = ifelse(is.na(input$dynasty_data_success_count_max),22,input$dynasty_data_success_count_max)) %>%
      mutate(dynasty_data_ppr_min = ifelse(is.na(input$dynasty_data_ppr_min),0,input$dynasty_data_ppr_min)) %>%
      mutate(dynasty_data_sv_count_min = ifelse(is.na(input$dynasty_data_sv_count_min),0,input$dynasty_data_sv_count_min)) %>%
      mutate(dynasty_data_sv_count_max = ifelse(is.na(input$dynasty_data_sv_count_max),22,input$dynasty_data_sv_count_max)) %>%
      mutate(dynasty_data_ppr_min = ifelse(is.na(input$dynasty_data_ppr_min),0,input$dynasty_data_ppr_min)) %>%
      mutate(dynasty_data_ppr_max = ifelse(is.na(input$dynasty_data_ppr_max),6000,input$dynasty_data_ppr_max)) %>%
      mutate(dynasty_data_lws_min = ifelse(is.na(input$dynasty_data_lws_min),0,input$dynasty_data_lws_min)) %>%
      mutate(dynasty_data_lws_max = ifelse(is.na(input$dynasty_data_lws_max),425,input$dynasty_data_lws_max)) %>%
      mutate(dynasty_data_g30_min = ifelse(is.na(input$dynasty_data_g30_min),0,input$dynasty_data_g30_min)) %>%
      mutate(dynasty_data_g30_max = ifelse(is.na(input$dynasty_data_g30_max),40,input$dynasty_data_g30_max)) %>%
      mutate(dynasty_data_g25_min = ifelse(is.na(input$dynasty_data_g25_min),0,input$dynasty_data_g25_min)) %>%
      mutate(dynasty_data_g25_max = ifelse(is.na(input$dynasty_data_g25_max),75,input$dynasty_data_g25_max)) %>%
      mutate(dynasty_data_g20_min = ifelse(is.na(input$dynasty_data_g20_min),0,input$dynasty_data_g20_min)) %>%
      mutate(dynasty_data_g20_max = ifelse(is.na(input$dynasty_data_g20_max),150,input$dynasty_data_g20_max)) %>%
      mutate(dynasty_data_gt10_min = ifelse(is.na(input$dynasty_data_gt10_min),0,input$dynasty_data_gt10_min)) %>%
      mutate(dynasty_data_gt10_max = ifelse(is.na(input$dynasty_data_gt10_max),150,input$dynasty_data_gt10_max)) %>%
      filter(
             League_Winner_Count >= dynasty_data_lw_count_min, League_Winner_Count <= dynasty_data_lw_count_max,
             Success_Count >= dynasty_data_success_count_min, Success_Count <= dynasty_data_success_count_max,
             Season_Valid_Count >= dynasty_data_sv_count_min, Season_Valid_Count <= dynasty_data_sv_count_max,
             PPR >= dynasty_data_ppr_min, PPR <= dynasty_data_ppr_max,
             LWS >= dynasty_data_lws_min, LWS <= dynasty_data_lws_max,
             games30 >= dynasty_data_g30_min, games30 <= dynasty_data_g30_max,
             games25 >= dynasty_data_g25_min, games25 <= dynasty_data_g25_max,
             games20 >= dynasty_data_g20_min, games20 <= dynasty_data_g20_max,
             gamesT10 >= dynasty_data_gt10_min, gamesT10 <= dynasty_data_gt10_max
      ) %>%
      arrange(-League_Winner_Count,-Success_Count,-LWS,-games30,-games25,-games20,-gamesT10,-PPR) %>%
      mutate(dynasty_rank = row_number()) %>%
      arrange(-games30) %>%
      mutate(rank30 = row_number()) %>%
      arrange(dynasty_rank) %>%
      arrange(-games25) %>%
      mutate(rank25 = row_number()) %>%
      arrange(dynasty_rank) %>%
      arrange(-games20) %>%
      mutate(rank20 = row_number()) %>%
      arrange(dynasty_rank) %>%
      arrange(-gamesT10) %>%
      mutate(rankT10 = row_number()) %>%
      arrange(dynasty_rank) %>%
      arrange(-League_Winner_Count) %>%
      mutate(rankLWC = row_number()) %>%
      arrange(dynasty_rank) %>%
      arrange(-Season_Valid_Count) %>%
      mutate(rankSVC = row_number()) %>%
      arrange(dynasty_rank) %>%
      arrange(-lw_mean) %>%
      mutate(rankLWMean = row_number()) %>%
      arrange(dynasty_rank) %>%
      arrange(-lw_med) %>%
      mutate(rankLWMed = row_number()) %>%
      arrange(dynasty_rank) %>%
      arrange(-Success_Count) %>%
      mutate(successful_rank = row_number()) %>%
      arrange(dynasty_rank) %>%
      arrange(-meanXmedian) %>%
      mutate(medxmedian_rank = row_number()) %>%
      arrange(dynasty_rank) %>%
      arrange(-LWS) %>%
      mutate(LWS_rank = row_number()) %>%
      arrange(dynasty_rank) %>%
      arrange(-PPR) %>%
      mutate(PPR_rank = row_number()) %>%
      arrange(dynasty_rank) %>%
      left_join(name_data_shiny, by = c("player_id" = "gsis_id")) %>%
      arrange(.data[[input$dynasty_data_sort]],dynasty_rank) %>%
      gt() %>%
      cols_hide(c(rank30,rank25,rank20,rankT10,rankLWC,rankSVC,rankLWMed,rankLWMean,successful_rank,LWS_rank,PPR_rank,medxmedian_rank,dynasty_data_lw_count_min,dynasty_data_lw_count_max,dynasty_data_success_count_min,dynasty_data_success_count_max,dynasty_data_ppr_min,dynasty_data_ppr_max,
                  dynasty_data_sv_count_min,dynasty_data_sv_count_max,dynasty_data_lws_min,dynasty_data_lws_max,dynasty_data_g30_min,dynasty_data_g30_max,dynasty_data_g25_min,dynasty_data_g25_max,dynasty_data_g20_min,dynasty_data_g20_max,dynasty_data_gt10_min,dynasty_data_gt10_max)) %>%
      cols_align(align = c("center")) %>%
      cols_move_to_start(dynasty_rank) %>%
      cols_move(full_name,dynasty_rank) %>%
      cols_move(League_Winner_Count,full_name) %>%
      cols_move(Season_Valid_Count,League_Winner_Count) %>%
      cols_move(Success_Count,League_Winner_Count) %>%
      cols_move(lw_mean,full_name) %>%
      cols_move(lw_med,lw_mean) %>%
      cols_move(LWS,Season_Valid_Count) %>%
      cols_move(PPR,LWS) %>%
      cols_move(seasons_played,lw_med) %>%
      cols_move_to_start(player_id) %>%
      cols_move(meanXmedian,lw_med) %>%
      cols_label(dynasty_rank = "Rank - Dynasty",
                 full_name = "Player Name",
                 games30 = "30+",
                 games25 = "25+",
                 games20 = "20+",
                 gamesT10 = "Top 10",
                 League_Winner_Count = "League Winner (Count)",
                 Season_Valid_Count = "Season Valid (Count)",
                 lw_mean = "LW Percent (Mean)",
                 lw_med = "LW Percent (Median)",
                 seasons_played = "Seasons Played",
                 LWS = "LW Score",
                 player_id = "Player ID",
                 Success_Count = "Successful (Count)",
                 meanXmedian = "Mean x Median") %>%
      fmt_percent(c(lw_mean,lw_med,meanXmedian)) %>%
      tab_style(
        style = list(
          cell_borders(
            sides = "right",
            color = "black",
            weight = px(3)
          )
        ),
        locations = list(
          cells_body(
            columns = c(dynasty_rank)))) %>%
      tab_style(
        style = list(
          cell_borders(
            sides = "right",
            color = "black",
            weight = px(3)
          )
        ),
        locations = list(
          cells_body(
            columns = c(meanXmedian)))) %>%
      tab_style(
        style = list(
          cell_borders(
            sides = "right",
            color = "black",
            weight = px(3)
          )
        ),
        locations = list(
          cells_body(
            columns = c(Season_Valid_Count)))) %>%
      tab_style(
        style = list(
          cell_borders(
            sides = "bottom",
            color = "black",
            weight = px(3)
          )
        ),
        locations = list(
          cells_column_labels()))
  )
  output$player_focus_data_graph <- renderPlot(
    lwp4_df %>%
      filter(season >= input$player_focus_data_season_min, season <= input$player_focus_data_season_max,
             full_name %in% input$player_focus_data_player1) %>%
      ggplot(aes(x = .data[[input$player_focus_data_x_axis]], y = .data[[input$player_focus_data_y_axis]])) +
      geom_point(size = 5) +
      theme_bw() +
      theme(axis.title = element_blank())
  )
  output$player_focus_data_table <- render_gt(
    lwp4_df %>%
      filter(season >= input$player_focus_data_season_min, season <= input$player_focus_data_season_max,
             full_name %in% input$player_focus_data_player1) %>%
      arrange(LWP4_Ranking) %>%
      mutate(LWP4_Ranking_filtered = row_number()) %>%
      mutate(LWP4_Ranking_filtered = ifelse(is.na(adp_averagePick),NA,LWP4_Ranking_filtered)) %>%
      mutate(adp_averagePick = ifelse(adp_averagePick == adp_undrafted,NA,adp_averagePick)) %>%
      mutate(ecr = ifelse(ecr == ecr_unranked,NA,ecr)) %>%
      arrange(EndofSeason_Rankings) %>%
      mutate(EoS_Rankings_Filtered = row_number()) %>%
      select(player_id,season,full_name,position,Position_order,lw,LWP4_Ranking,yearly_rank,LWP4_Ranking_filtered,zr_rankings_adjusted,adp_averagePick,ecr,EndofSeason_Rankings,EOS_yearly,EoS_Rankings_Filtered,League_Winner,Games_Away,Season_Valid,LWS,PPR,games30,games25,games20,gamesT10,g30_rank,g25_rank,g20_rank,gT10_rank,PPR_rank,LWS_rank,lwp4_comp_rank,yearly_rank_per,compare_p2) %>%
      arrange(.data[[input$player_focus_data_sort]],season,EndofSeason_Rankings) %>%
      mutate(EndofSeason_Rankings = ifelse(season == 2022,NA,EndofSeason_Rankings)) %>%
      mutate(EOS_yearly = ifelse(season == 2022,NA,EOS_yearly)) %>%
      mutate(EoS_Rankings_Filtered = ifelse(season == 2022,NA,EoS_Rankings_Filtered)) %>%
      mutate(League_Winner = ifelse(season == 2022,NA,League_Winner)) %>%
      mutate(Games_Away = ifelse(season == 2022,NA,Games_Away)) %>%
      mutate(Season_Valid = ifelse(season == 2022,NA,Season_Valid)) %>%
      mutate(games30 = ifelse(season == 2022,NA,games30)) %>%
      mutate(games25 = ifelse(season == 2022,NA,games25)) %>%
      mutate(games20 = ifelse(season == 2022,NA,games20)) %>%
      mutate(gamesT10 = ifelse(season == 2022,NA,gamesT10)) %>%
      mutate(LWS = ifelse(season == 2022,NA,LWS)) %>%
      mutate(PPR = ifelse(season == 2022,NA,PPR)) %>%
      gt() %>%
      gt_highlight_rows(rows = yearly_rank_per < 0.75 & compare_p2 == 1, fill = "lightcoral") %>%
      gt_highlight_rows(rows = yearly_rank_per < 0.5 & compare_p2 == 1, fill = "Firebrick") %>%
      gt_highlight_rows(rows = yearly_rank_per > 1.25 & compare_p2 == 1) %>%
      gt_highlight_rows(rows = yearly_rank_per > 1.5 & compare_p2 == 1, fill = "royalblue") %>%
      cols_hide(c(Position_order,g30_rank,g25_rank,g20_rank,gT10_rank,PPR_rank,LWS_rank,lwp4_comp_rank,yearly_rank_per,compare_p2)) %>%
      cols_label(player_id = "Player ID",
                 season = "Season",
                 full_name = "Player Name",
                 position = "Position",
                 lw = "LW Percentage",
                 LWP4_Ranking = "LWP4 Ranking - Overall",
                 LWP4_Ranking_filtered = "LWP4 Ranking - Filtered",
                 adp_averagePick = "ADP (MFL)",
                 ecr = "FantasyPros Ranking (ECR)",
                 EndofSeason_Rankings = "LW Rank - Overall",
                 yearly_rank = "LWP4 Ranking - Yearly",
                 EoS_Rankings_Filtered = "LW Rank - Filtered",
                 EOS_yearly = "LW Rank - Yearly",
                 League_Winner = "League Winner",
                 Games_Away = "Games Away",
                 Season_Valid = "Season Valid",
                 games30 = "30+",
                 games25 = "25+",
                 games20 = "20+",
                 gamesT10 = "Top 10",
                 LWS = "LW Score",
                 zr_rankings_adjusted = "ZR Rankings") %>%
      cols_align(align = ("center")) %>%
      fmt_percent(lw) %>%
      tab_style(
        style = list(
          cell_borders(
            sides = "right",
            color = "black",
            weight = px(3)
          )
        ),
        locations = list(
          cells_body(
            columns = c(position)))) %>%
      tab_style(
        style = list(
          cell_borders(
            sides = "right",
            color = "black",
            weight = px(3)
          )
        ),
        locations = list(
          cells_body(
            columns = c(EoS_Rankings_Filtered)))) %>%
      tab_style(
        style = list(
          cell_borders(
            sides = "right",
            color = "black",
            weight = px(3)
          )
        ),
        locations = list(
          cells_body(
            columns = c(LWP4_Ranking_filtered)))) %>%
      tab_style(
        style = list(
          cell_borders(
            sides = "left",
            color = "black",
            weight = px(3)
          )
        ),
        locations = list(
          cells_body(
            columns = c(EndofSeason_Rankings)))) %>%
      tab_style(
        style = list(
          cell_borders(
            sides = "bottom",
            color = "black",
            weight = px(3)
          )
        ),
        locations = list(
          cells_column_labels()))
  )
}

shinyApp(ui,server)

