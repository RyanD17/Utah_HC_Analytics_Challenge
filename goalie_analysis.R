#Import hockey R library

#install.packages("ggrepel")
#install.packages("nhlapi")
# devtools::install_github("danmorse314/hockeyR")
#devtools::install_github("jozefhajnala/nhlapi")
#Load necessary libraries 
library(tidyverse)
library(hockeyR)
library(dplyr)
library(rvest)
library(openxlsx)
library(readxl)
library(janitor)
library(anytime)
library(ggrepel)
library(car)
library(lattice)
library(nhlapi)

#project ideas: 
# 1) how effective is the reverse VH for goaltenders
# 2) How valuble is winning faceoffs in hockey?

# 5a) Analyze ticket sales vs win percentage for NHL teams 
# 5b) Look at ticket prices throughout the league and suggest a price 
# that maxamizes profit, and # of tickets sold . 


#Main idea
# 3) Rebound control for goaltenders
# 3b) Analyzing goaltender preformance, and understanding why goalies cannot
# 3b) play as many games as they used to (in 2010s top-tier goalies played
# 60 + games, while in 2024 it's more common to see a 1A, 1B combo for goalies)
# 3c) Look at expected goals against for goalies and rank goalies base on that

data <- load_pbp('2023-2024')

shot_goal_data <- data %>%
  filter(event_type == "SHOT" | event_type == "GOAL")

unique_team_names <- data %>%
  distinct(event_team)
unique_team_names <- unique_team_names %>%
  filter(!is.na(event_team))

unique_goalie_names <- data %>%
  distinct(event_goalie_name)

unique_goalie_names <- unique_goalie_names %>%
  filter(!is.na(event_goalie_name))


goalie_shots_against <- function(goalie_name){
  shots <- shot_goal_data %>%
    filter(event_goalie_name == goalie_name)
  
  return(nrow(shots))
}

goalie_save_percent <- function(goalie_name){
  saves <- shot_goal_data %>%
    filter(event_goalie_name == goalie_name) %>%
    filter(event_type == "SHOT")

  goals_allowed <- shot_goal_data %>%
    filter(event_goalie_name == goalie_name) %>%
    filter(event_type == "GOAL") 
  
  num_saves <- nrow(saves)
  num_goals_allowed <- nrow(goals_allowed)
  
  save_percentage <- num_saves/(num_goals_allowed + num_saves)
  return(save_percentage)
}

goalie_save_percent("Jeremy Swayman")

goalie_pk_save_percent <- function(goalie_name){
  pk_saves <- shot_goal_data %>%
    filter(event_goalie_name == goalie_name) %>%
    filter(strength_code == "PP") %>%
    filter(event_type == "SHOT")
  
  pk_goals_allowed <- shot_goal_data %>%
    filter(event_goalie_name == goalie_name) %>%
    filter(strength_code == "PP") %>%
    filter(event_type == "GOAL") 
  
  num_pk_saves <- nrow(pk_saves)
  num_pk_goals_allowed <- nrow(pk_goals_allowed)
  
  pk_save_percentage <- num_pk_saves/(num_pk_goals_allowed + num_pk_saves)
  return(pk_save_percentage)
}

goalie_pk_save_percent("Jeremy Swayman")

goalie_high_danger_save_pct <- function(goalie_name){
  high_danger_saves <- shot_goal_data %>%
    filter(event_goalie_name == goalie_name) %>%
    filter(shot_distance <= 29) %>%
    filter(event_type == "SHOT")
  
  high_danger_goals_allowed <- shot_goal_data %>%
    filter(event_goalie_name == goalie_name) %>%
    filter(shot_distance <= 29) %>%
    filter(event_type == "GOAL")
  
  num_high_danger_saves <- nrow(high_danger_saves)
  num_high_danger_goals_allowed <- nrow(high_danger_goals_allowed)
  
  high_danger_save_pct <- num_high_danger_saves/(num_high_danger_goals_allowed+num_high_danger_saves)
  return(high_danger_save_pct)
}

goalie_mid_range_save_pct <- function(goalie_name){
  mid_range_saves <- shot_goal_data %>%
    filter(event_goalie_name == goalie_name) %>%
    filter(shot_distance >= 29 & shot_distance <= 43) %>%
    filter(event_type == "SHOT")
  
  mid_range_goals_allowed <- shot_goal_data %>%
    filter(event_goalie_name == goalie_name) %>%
    filter(shot_distance >= 29 & shot_distance <= 43) %>%
    filter(event_type == "GOAL")
  
  num_mid_range_saves <- nrow(mid_range_saves)
  num_mid_range_goals_allowed <- nrow(mid_range_goals_allowed)
  
  mid_range_save_pct <- num_mid_range_saves/(num_mid_range_goals_allowed+num_mid_range_saves)
  return(mid_range_save_pct)
}

goalie_high_danger_save_pct("Sergei Bobrovsky")

goalie_long_range_save_pct <- function(goalie_name){
  long_range_saves <- shot_goal_data %>%
    filter(event_goalie_name == goalie_name) %>%
    filter(shot_distance >= 43) %>%
    filter(event_type == "SHOT")
  
  long_range_goals_allowed <- shot_goal_data %>%
    filter(event_goalie_name == goalie_name) %>%
    filter(shot_distance >= 43) %>%
    filter(event_type == "GOAL")
  
  num_long_range_saves <- nrow(long_range_saves)
  num_long_range_goals_allowed <- nrow(long_range_goals_allowed)
  
  long_range_save_pct <- num_long_range_saves/(num_long_range_goals_allowed+num_long_range_saves)
  return(long_range_save_pct)
}


unique_goalie_names_vector <- unique_goalie_names$event_goalie_name
unique_team_names_vector <- unique_team_names$event_team
goalie_name_vector <- c()
goalie_team_vector <- c()
goalie_sa_vector <- c()
goalie_save_pct_vector <- c()
goalie_pk_save_pct_vector <- c()
goalie_high_danger_save_pct_vector <- c()
goalie_mid_range_save_pct_vector <- c()
goalie_long_range_save_pct_vector <- c()

for(i in 1:length(unique_goalie_names_vector)){
  tendy_name <- unique_goalie_names_vector[i]
  
  shots_against <- goalie_shots_against(tendy_name)
  save_pct <- goalie_save_percent(tendy_name)
  pk_save_pct <- goalie_pk_save_percent(tendy_name)
  high_danger_save_pct <- goalie_high_danger_save_pct(tendy_name)
  mid_range_save_pct <- goalie_mid_range_save_pct(tendy_name)
  long_range_save_pct <- goalie_long_range_save_pct(tendy_name)
  
  goalie_name_vector <- c(goalie_name_vector, tendy_name)
  goalie_team_vector <- c(goalie_team_vector, team_name)
  goalie_sa_vector <- c(goalie_sa_vector, shots_against)
  goalie_save_pct_vector <- c(goalie_save_pct_vector, save_pct)
  goalie_pk_save_pct_vector <- c(goalie_pk_save_pct_vector, pk_save_pct)
  goalie_high_danger_save_pct_vector <- c(goalie_high_danger_save_pct_vector, high_danger_save_pct)
  goalie_mid_range_save_pct_vector <- c(goalie_mid_range_save_pct_vector, mid_range_save_pct)
  goalie_long_range_save_pct_vector <- c(goalie_long_range_save_pct_vector, long_range_save_pct)
  
}

goalie_stats_df <- data.frame(
  "Goalie Name" = goalie_name_vector, 
  "Goalie Shots Against" = goalie_sa_vector, 
  "Goalie Save %" = goalie_save_pct_vector,
  "Goalie PK Save %" = goalie_pk_save_pct_vector,
  "Goalie High Danger Save %" = goalie_high_danger_save_pct_vector,
  "Goalie Mid Range Save %" = goalie_mid_range_save_pct_vector,
  "Goalie Long Range Save %" = goalie_long_range_save_pct_vector
)

View(goalie_stats_df)



