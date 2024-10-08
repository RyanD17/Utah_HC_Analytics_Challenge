# Ensure all required libraries are loaded
library(tidyverse)
library(hockeyR)
library(rvest)
library(openxlsx)
library(readxl)
library(janitor)
library(anytime)
library(ggrepel)
library(car)
library(lattice)
library(ggplot2)
library(gt)
library(plotly)


#Load data
data <- load_pbp('2023-2024')


#Filtering Data
all_games <- unique(data$game_id)


# Filter data for shots and goals that exclude 
# empty net goals and empty net shot attempts
shot_goal_data <- data %>%
  filter(event_type %in% c("SHOT", "GOAL")) %>%
  filter(extra_attacker %in% c("FALSE"))

# Get unique goalie names
unique_goalie_names <- data %>%
  distinct(event_goalie_name) %>%
  filter(!is.na(event_goalie_name))


#General Statistics

#function to get the number of games that a goalie played
goalie_games_played <- function(goalie_name){
  goalie_data <- data %>% 
    filter(event_goalie_name == goalie_name)
  goalie_data <- unique(goalie_data$game_id)
  
  return(length(goalie_data))
}
#function that gets the distance of each shot that a goalie has faced
goalie_shot_distance <- function(goalie_name) {
  shot_distance_vector <- shot_goal_data %>%
    filter(event_goalie_name == goalie_name) %>%
    pull(shot_distance)
  return(shot_distance_vector)
}

#function to get the number of shots that a goaltender faced 
goalie_shots_against <- function(goalie_name) {
  shots <- shot_goal_data %>%
    filter(event_goalie_name == goalie_name) %>%
    nrow()
  return(shots)
}

#function to get the number of goals that a goaltender allowed
#in the 2023-2024 season
goalie_goals_allowed <- function(goalie_name) {
  goals <- shot_goal_data %>%
    filter(event_goalie_name == goalie_name, event_type == "GOAL") %>%
    nrow()
  return(goals)
}

#function to calculate the save percentage of a goaltender
goalie_save_percent <- function(goalie_name) {
  shots <- goalie_shots_against(goalie_name)
  goals <- goalie_goals_allowed(goalie_name)
  
  if (shots == 0) return(NA)
  
  save_percentage <- (shots - goals) / shots
  return(round(save_percentage, 3))
}


#Penalty Kill Stats


#function to calculate the number of shots that a goaltender who's 
#team is on the penalty kill faced
goalie_pk_shots_against <- function(goalie_name) {
  pk_shots <- shot_goal_data %>%
    filter(event_goalie_name == goalie_name, strength_code == "PP", event_type == "SHOT") %>%
    nrow()
  pk_goals <- shot_goal_data %>%
    filter(event_goalie_name == goalie_name, strength_code == "PP", event_type == "GOAL") %>%
    nrow()
  
  return(pk_shots + pk_goals)
}

#function that returns the number of goals that a goaltender allowed
#while their team was on the penalty kill 
goalie_pk_goals_against <- function(goalie_name) {
  pk_goals <- shot_goal_data %>%
    filter(event_goalie_name == goalie_name, strength_code == "PP", event_type == "GOAL") %>%
    nrow()
  return(pk_goals)
}

#function to get the save percentage of a goaltender who's team is on the penalty kill
goalie_pk_save_percent <- function(goalie_name) {
  pk_shots <- shot_goal_data %>%
    filter(event_goalie_name == goalie_name, strength_code == "PP", event_type == "SHOT") %>%
    nrow()
  pk_goals <- shot_goal_data %>%
    filter(event_goalie_name == goalie_name, strength_code == "PP", event_type == "GOAL") %>%
    nrow()
  
  if (pk_shots == 0) return(NA)
  
  pk_save_percentage <- (pk_shots - pk_goals) / pk_shots
  return(round(pk_save_percentage, digits = 3))
}


#High Danger Statistics (shots being \<= 29ft away from opposing teams' net)


#function that calculates the number of high danger shots that a goalie has faced
goalie_high_danger_shots_against <- function(goalie_name) {
  high_danger_shots <- shot_goal_data %>%
    filter(event_goalie_name == goalie_name, shot_distance <= 29, event_type == "SHOT") %>%
    nrow()
  high_danger_goals <- shot_goal_data %>%
    filter(event_goalie_name == goalie_name, shot_distance <= 29, event_type == "GOAL") %>%
    nrow()
  
  return(high_danger_shots + high_danger_goals)
}
#function that calculates the number of high danger shots that a goalie has allowed 
goalie_high_danger_goals_against <- function(goalie_name) {
  high_danger_goals <- shot_goal_data %>%
    filter(event_goalie_name == goalie_name, shot_distance <= 29, event_type == "GOAL") %>%
    nrow()
  return(high_danger_goals)
}

#function that calculates the high danger save percentage for a goalie.
goalie_high_danger_save_pct <- function(goalie_name) {
  high_danger_shots <- shot_goal_data %>%
    filter(event_goalie_name == goalie_name, shot_distance <= 29, event_type == "SHOT") %>%
    nrow()
  high_danger_goals <- shot_goal_data %>%
    filter(event_goalie_name == goalie_name, shot_distance <= 29, event_type == "GOAL") %>%
    nrow()
  
  if (high_danger_shots == 0) return(NA)
  
  high_danger_save_pct <- (high_danger_shots - high_danger_goals) / high_danger_shots
  return(round(high_danger_save_pct, digits = 3))
}


#Mid Range Statistics (shot being \>= 29ft and \<= 43 ft away from the opposing teams' net)


#function that calculates the number of shots that a goaltender has faced from mid range shots
goalie_mid_range_shots_against <- function(goalie_name) {
  mid_range_shots <- shot_goal_data %>%
    filter(event_goalie_name == goalie_name, shot_distance >= 29 & shot_distance <= 43, event_type == "SHOT") %>%
    nrow()
  mid_range_goals <- shot_goal_data %>%
    filter(event_goalie_name == goalie_name, shot_distance >= 29 & shot_distance <= 43, event_type == "GOAL") %>%
    nrow()
  
  return(mid_range_shots + mid_range_goals)
}
#function that calculates the number of goals that a goaltender has allowed from mid range shots
goalie_mid_range_goals_against <- function(goalie_name) {
  mid_range_goals <- shot_goal_data %>%
    filter(event_goalie_name == goalie_name, shot_distance >= 29 & shot_distance <= 43, event_type == "GOAL") %>%
    nrow()
  return(mid_range_goals)
}
#function that calculates the save percentage for a goaltender from mid range shots and goals
goalie_mid_range_save_pct <- function(goalie_name) {
  mid_range_shots <- shot_goal_data %>%
    filter(event_goalie_name == goalie_name, shot_distance >= 29 & shot_distance <= 43, event_type == "SHOT") %>%
    nrow()
  mid_range_goals <- shot_goal_data %>%
    filter(event_goalie_name == goalie_name, shot_distance >= 29 & shot_distance <= 43, event_type == "GOAL") %>%
    nrow()
  
  if (mid_range_shots == 0) return(NA)
  
  mid_range_save_pct <- (mid_range_shots - mid_range_goals) / mid_range_shots
  
  return(round(mid_range_save_pct, digits = 3))
}


#Long Range Shots (shot being \>= 43 ft away while shot is still located within the offensive zone)


#function that calculates the number of shots that a goaltender has faced from long range shots
goalie_long_range_shots_against <- function(goalie_name) {
  long_range_shots <- shot_goal_data %>%
    filter(event_goalie_name == goalie_name, shot_distance >= 43 & shot_distance <= 64, event_type == "SHOT") %>%
    nrow()
  long_range_goals <- shot_goal_data %>%
    filter(event_goalie_name == goalie_name, shot_distance >= 43, event_type == "GOAL") %>%
    nrow()
  
  return(long_range_shots + long_range_goals)
}
#function that calculates the number of goals that a goaltender has allowed from long range shots
goalie_long_range_goals_against <- function(goalie_name) {
  long_range_goals <- shot_goal_data %>%
    filter(event_goalie_name == goalie_name, shot_distance >= 43, event_type == "GOAL") %>%
    nrow()
  return(long_range_goals)
}
#function that calculates the save percentage of goaltender from long range shots and goals
goalie_long_range_save_pct <- function(goalie_name) {
  long_range_shots <- shot_goal_data %>%
    filter(event_goalie_name == goalie_name, shot_distance >= 43, event_type == "SHOT") %>%
    nrow()
  long_range_goals <- shot_goal_data %>%
    filter(event_goalie_name == goalie_name, shot_distance >= 43, event_type == "GOAL") %>%
    nrow()
  
  if (long_range_shots == 0) return(NA)
  
  long_range_save_pct <- (long_range_shots - long_range_goals) / long_range_shots
  return(round(long_range_save_pct, digits = 3))
}


#For loop and Dataframe creation


# Initialize vectors for storing data
goalie_name_vector <- c()
goalie_games_played_vector <- c()
goalie_shot_distance_vector <- c()
goalie_sa_vector <- c()
goalie_goals_allowed_vector <- c()
goalie_save_pct_vector <- c()

goalie_pk_shots_against_vector <- c()
goalie_pk_goals_allowed_vector <- c()
goalie_pk_save_pct_vector <- c()

goalie_high_danger_shots_against_vector <- c()
goalie_high_danger_goals_allowed_vector <- c()
goalie_high_danger_save_pct_vector <- c()

goalie_mid_range_shots_against_vector <- c()
goalie_mid_range_goals_allowed_vector <- c()
goalie_mid_range_save_pct_vector <- c()

goalie_long_range_shots_against_vector <- c()
goalie_long_range_goals_allowed_vector <- c()
goalie_long_range_save_pct_vector <- c()

# Process each goalie
for (i in 1:length(unique_goalie_names$event_goalie_name)) {
  tendy_name <- unique_goalie_names$event_goalie_name[i]
  
  shots_against <- goalie_shots_against(tendy_name)
  games_played <- goalie_games_played(tendy_name)
  shot_distance <- goalie_shot_distance(tendy_name)
  goals_allowed <- goalie_goals_allowed(tendy_name)
  save_pct <- goalie_save_percent(tendy_name)
  
  pk_shots_against <- goalie_pk_shots_against(tendy_name)
  pk_save_pct <- goalie_pk_save_percent(tendy_name)
  pk_goals_allowed <- goalie_pk_goals_against(tendy_name)
  
  high_danger_shots_against <- goalie_high_danger_shots_against(tendy_name)
  high_danger_save_pct <- goalie_high_danger_save_pct(tendy_name)
  high_danger_goals_allowed <- goalie_high_danger_goals_against(tendy_name)
  
  mid_range_shots_against <- goalie_mid_range_shots_against(tendy_name)
  mid_range_save_pct <- goalie_mid_range_save_pct(tendy_name)
  mid_range_goals_allowed <- goalie_mid_range_goals_against(tendy_name)
  
  long_range_shots_against <- goalie_long_range_shots_against(tendy_name)
  long_range_save_pct <- goalie_long_range_save_pct(tendy_name)
  long_range_goals_allowed <- goalie_long_range_goals_against(tendy_name)
  
  goalie_name_vector <- c(goalie_name_vector, tendy_name)
  goalie_games_played_vector <- c(goalie_games_played_vector, games_played)
  goalie_sa_vector <- c(goalie_sa_vector, shots_against)
  goalie_shot_distance_vector <- c(goalie_shot_distance_vector, shot_distance)
  goalie_goals_allowed_vector <- c(goalie_goals_allowed_vector, goals_allowed)
  goalie_save_pct_vector <- c(goalie_save_pct_vector, save_pct)
  
  goalie_pk_shots_against_vector <- c(goalie_pk_shots_against_vector, pk_shots_against)
  goalie_pk_goals_allowed_vector <- c(goalie_pk_goals_allowed_vector, pk_goals_allowed)
  goalie_pk_save_pct_vector <- c(goalie_pk_save_pct_vector, pk_save_pct)
  
  goalie_high_danger_shots_against_vector <- c(goalie_high_danger_shots_against_vector, high_danger_shots_against)
  goalie_high_danger_goals_allowed_vector <- c(goalie_high_danger_goals_allowed_vector, high_danger_goals_allowed)
  goalie_high_danger_save_pct_vector <- c(goalie_high_danger_save_pct_vector, high_danger_save_pct)
  
  goalie_mid_range_shots_against_vector <- c(goalie_mid_range_shots_against_vector, mid_range_shots_against)
  goalie_mid_range_goals_allowed_vector <- c(goalie_mid_range_goals_allowed_vector, mid_range_goals_allowed)
  goalie_mid_range_save_pct_vector <- c(goalie_mid_range_save_pct_vector, mid_range_save_pct)
  
  goalie_long_range_shots_against_vector <- c(goalie_long_range_shots_against_vector, long_range_shots_against)
  goalie_long_range_goals_allowed_vector <- c(goalie_long_range_goals_allowed_vector, long_range_goals_allowed)
  goalie_long_range_save_pct_vector <- c(goalie_long_range_save_pct_vector, long_range_save_pct)
}

# Create a data frame with the statistics
goalie_stats_df <- data.frame(
  "Goalie Name" = goalie_name_vector,
  "Goalie Games Played" = goalie_games_played_vector,
  "Goalie Shots Against" = goalie_sa_vector,
  "Goalie Goals Allowed" = goalie_goals_allowed_vector,
  "Goalie Save %" = goalie_save_pct_vector,
  "Goalie PK Shots Against" = goalie_pk_shots_against_vector,
  "Goalie PK Goals Allowed" = goalie_pk_goals_allowed_vector,
  "Goalie PK Save %" = goalie_pk_save_pct_vector,
  "Goalie High Danger Shots Against" = goalie_high_danger_shots_against_vector,
  "Goalie High Danger Goals Allowed" = goalie_high_danger_goals_allowed_vector,
  "Goalie High Danger Save %" = goalie_high_danger_save_pct_vector,
  "Goalie Mid Range Shots Against" = goalie_mid_range_shots_against_vector,
  "Goalie Mid Range Goals Allowed" = goalie_mid_range_goals_allowed_vector,
  "Goalie Mid Range Save %" = goalie_mid_range_save_pct_vector,
  "Goalie Long Range Shots Against" = goalie_long_range_shots_against_vector,
  "Goalie Long Range Goals Allowed" = goalie_long_range_goals_allowed_vector,
  "Goalie Long Range Save %" = goalie_long_range_save_pct_vector
)

# View the results
View(goalie_stats_df)


#CSV Writing



goalie_stats_csv <- write.csv(goalie_stats_df, "goalie_stats_csv", row.names = FALSE)