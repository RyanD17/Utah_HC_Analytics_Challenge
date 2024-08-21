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

# Load data
data <- load_pbp('2023-2024')

all_games <- unique(data$game_id)


# Filter data for shots and goals
shot_goal_data <- data %>%
  filter(event_type %in% c("SHOT", "GOAL"))

# Get unique goalie names
unique_goalie_names <- data %>%
  distinct(event_goalie_name) %>%
  filter(!is.na(event_goalie_name))

goalie_games_played <- function(goalie_name){
  goalie_data <- data %>% 
    filter(event_goalie_name == goalie_name)
  goalie_data <- unique(goalie_data$game_id)
  
  return(length(goalie_data))
}


goalie_shots_against <- function(goalie_name) {
  shots <- shot_goal_data %>%
    filter(event_goalie_name == goalie_name) %>%
    nrow()
  return(shots)
}


goalie_goals_allowed <- function(goalie_name) {
  goals <- shot_goal_data %>%
    filter(event_goalie_name == goalie_name, event_type == "GOAL") %>%
    nrow()
  return(goals)
}

goalie_save_percent <- function(goalie_name) {
  shots <- goalie_shots_against(goalie_name)
  goals <- goalie_goals_allowed(goalie_name)
  
  if (shots == 0) return(NA)
  
  save_percentage <- (shots - goals) / shots
  return(round(save_percentage, 3))
}

goalie_pk_shots_against <- function(goalie_name) {
  pk_shots <- shot_goal_data %>%
    filter(event_goalie_name == goalie_name, strength_code == "PP", event_type == "SHOT") %>%
    nrow()
  pk_goals <- shot_goal_data %>%
    filter(event_goalie_name == goalie_name, strength_code == "PP", event_type == "GOAL") %>%
    nrow()
  
  return(pk_shots + pk_goals)
}

goalie_pk_goals_against <- function(goalie_name) {
  pk_goals <- shot_goal_data %>%
    filter(event_goalie_name == goalie_name, strength_code == "PP", event_type == "GOAL") %>%
    nrow()
  return(pk_goals)
}

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

goalie_high_danger_shots_against <- function(goalie_name) {
  high_danger_shots <- shot_goal_data %>%
    filter(event_goalie_name == goalie_name, shot_distance <= 29, event_type == "SHOT") %>%
    nrow()
  high_danger_goals <- shot_goal_data %>%
    filter(event_goalie_name == goalie_name, shot_distance <= 29, event_type == "GOAL") %>%
    nrow()
  
  return(high_danger_shots + high_danger_goals)
}

goalie_shot_distance <- function(goalie_name) {
  shot_distance_vector <- shot_goal_data %>%
    filter(event_goalie_name == goalie_name) %>%
    pull(shot_distance)
  return(shot_distance_vector)
}

goalie_high_danger_goals_against <- function(goalie_name) {
  high_danger_goals <- shot_goal_data %>%
    filter(event_goalie_name == goalie_name, shot_distance <= 29, event_type == "GOAL") %>%
    nrow()
  return(high_danger_goals)
}

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

goalie_mid_range_shots_against <- function(goalie_name) {
  mid_range_shots <- shot_goal_data %>%
    filter(event_goalie_name == goalie_name, shot_distance >= 29 & shot_distance <= 43, event_type == "SHOT") %>%
    nrow()
  mid_range_goals <- shot_goal_data %>%
    filter(event_goalie_name == goalie_name, shot_distance >= 29 & shot_distance <= 43, event_type == "GOAL") %>%
    nrow()
  
  return(mid_range_shots + mid_range_goals)
}

goalie_mid_range_goals_against <- function(goalie_name) {
  mid_range_goals <- shot_goal_data %>%
    filter(event_goalie_name == goalie_name, shot_distance >= 29 & shot_distance <= 43, event_type == "GOAL") %>%
    nrow()
  return(mid_range_goals)
}

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

goalie_long_range_shots_against <- function(goalie_name) {
  long_range_shots <- shot_goal_data %>%
    filter(event_goalie_name == goalie_name, shot_distance >= 43, event_type == "SHOT") %>%
    nrow()
  long_range_goals <- shot_goal_data %>%
    filter(event_goalie_name == goalie_name, shot_distance >= 43, event_type == "GOAL") %>%
    nrow()
  
  return(long_range_shots + long_range_goals)
}

goalie_long_range_goals_against <- function(goalie_name) {
  long_range_goals <- shot_goal_data %>%
    filter(event_goalie_name == goalie_name, shot_distance >= 43, event_type == "GOAL") %>%
    nrow()
  return(long_range_goals)
}

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

goalie_stats_csv <- write.csv(goalie_stats_df, "goalie_stats_csv", row.names = FALSE)

#Data used to create graphs based on best overall save %
best_save_pct <- goalie_stats_df %>%
  group_by(Goalie.Name) %>%
  filter(Goalie.Shots.Against > 855) %>%
  summarize(
    games_played = max(Goalie.Games.Played),
    save_pct = max(Goalie.Save..),
    shots_against = max(Goalie.Shots.Against),
    goals_allowed = max(Goalie.Goals.Allowed)
  ) %>%
  ungroup() %>%
  arrange(desc(save_pct)) %>%
  mutate(Rank = row_number()) %>%
  slice_head(n = 10)

#Data used to create graphs based on best PK Save %
best_pk_save_pct <- goalie_stats_df %>%
  group_by(Goalie.Name) %>%
  filter(Goalie.Shots.Against > 855) %>%
  summarize(
    games_played = max(Goalie.Games.Played),
    pk_save_pct = max(Goalie.PK.Save..),
    pk_goals_allowed = max(Goalie.PK.Goals.Allowed),
    pk_shots_against = max(Goalie.PK.Shots.Against)
  ) %>%
  ungroup() %>%
  arrange(desc(pk_save_pct)) %>%
  mutate(Rank = row_number()) %>%
  slice_head(n = 10)

#Data filtered to create graphs based on High Danger Save %
 best_hd_save_pct <- goalie_stats_df %>%
   group_by(Goalie.Name) %>%
   filter(Goalie.Shots.Against > 855) %>%
   summarize(
     high_danger_shots_against = max(Goalie.High.Danger.Shots.Against),
     high_danger_goals_allowed = max(Goalie.High.Danger.Goals.Allowed),
     high_danger_save_pct = max(Goalie.High.Danger.Save..)
   ) %>%
   ungroup() %>%
   arrange(desc(high_danger_save_pct)) %>%
   mutate(Rank = row_number()) %>%
   slice_head(n = 10)
 
 #Filtered data that is being used to create graphs based on 
 #mid danger/range save %
 best_mid_range_save_pct <- goalie_stats_df %>%
   group_by(Goalie.Name) %>%
   filter(Goalie.Shots.Against > 855) %>%
   summarize(
     mid_danger_shots_against = max(Goalie.Mid.Range.Shots.Against),
     mid_danger_goals_allowed = max(Goalie.Mid.Range.Goals.Allowed),
     mid_range_save_pct = max(Goalie.Mid.Range.Save..)
   ) %>%
   ungroup() %>%
   arrange(desc(mid_range_save_pct)) %>%
   mutate(Rank = row_number()) %>%
   slice_head(n = 10)
 
 #Filtered data used to create graphs based on long range save %
 best_long_range_save_pct <- goalie_stats_df %>%
   group_by(Goalie.Name) %>%
   filter(Goalie.Shots.Against > 855) %>%
   summarize(
     long_range_shots_against = max(Goalie.Long.Range.Shots.Against),
     long_range_goals_allowed = max(Goalie.Long.Range.Goals.Allowed),
     long_range_save_pct = max(Goalie.Long.Range.Save..)
   ) %>%
   ungroup() %>%
   arrange(desc(long_range_save_pct)) %>%
   mutate(Rank = row_number()) %>%
   slice_head(n = 10)

#Table used to rank top 10 goalies by overall save percentage
best_save_pct_table <- best_save_pct %>%
  gt() %>%
  tab_header(
    title = "Top 10 Goalies by Save %"
  ) %>%
  cols_label(
    Goalie.Name = "Goalie Name",
    shots_against = "Shots Faced",
    goals_allowed = "Goals Allowed",
    save_pct = "Save Percentage"
  ) %>%
  tab_options(
    table.font.size = 12
  )

#Table used to rank top 10 goalies by best PK save %
best_pk_save_pct_table <- best_pk_save_pct %>%
  gt() %>%
  tab_header(
    title = "Top 10 Goalies by PK Save %"
  ) %>%
  cols_label(
    Goalie.Name = "Goalie Name",
    pk_shots_against = "Shots Faced",
    pk_goals_allowed = "Goals Allowed",
    pk_save_pct = "Save Percentage"
  ) %>%
  tab_options(
    table.font.size = 12
  )
#Table used to rank top 10 goalies based on High Danger save %
best_hd_save_pct_table <- best_hd_save_pct %>%
  gt() %>%
  tab_header(
    title = "Top 10 Goalies by High Danger Save %"
  ) %>%
  cols_label(
    Goalie.Name = "Goalie Name",
    high_danger_shots_against = "Shots Faced",
    high_danger_goals_allowed = "Goals Allowed",
    high_danger_save_pct = "Save Percentage"
  ) %>%
  tab_options(
    table.font.size = 12
  )
#Table used to rank top 10 goalies based on Mid Range/Danger save %
best_mid_range_save_pct_table <- best_mid_range_save_pct %>%
  gt() %>%
  tab_header(
    title = "Top 10 Goalies by Mid Range Save %"
  ) %>%
  cols_label(
    Goalie.Name = "Goalie Name",
    mid_danger_shots_against = "Shots Faced",
    mid_danger_goals_allowed = "Goals Allowed",
    mid_range_save_pct = "Save Percentage"
  ) %>%
  tab_options(
    table.font.size = 12
  )
#Table used to rank top 10 goalies based on Long Range save %

best_long_range_save_pct_table <- best_long_range_save_pct %>%
  gt() %>%
  tab_header(
    title = "Top 10 Goalies by Long Range Save %"
  ) %>%
  cols_label(
    Goalie.Name = "Goalie Name",
    long_range_shots_against = "Shots Faced",
    long_range_goals_allowed = "Goals Allowed",
    long_range_save_pct = "Save Percentage"
  ) %>%
  tab_options(
    table.font.size = 12
  )

# Print the tables
best_pk_save_pct_table
best_save_pct_table
best_hd_save_pct_table
best_mid_range_save_pct_table
best_long_range_save_pct_table

#Plotting a graph showcasing shots against vs save percentage for 
#different goalies
plot_ly(x = goalie_stats_df$Goalie.Shots.Against, 
        y = goalie_stats_df$Goalie.Save..,
        type = "scatter",
        mode = "markers", 
        marker = list(size = 10, color = "blue"), 
        text = goalie_stats_df$Goalie.Name, 
        hoverinfo = 'text') %>%
  layout(
    title = "Save % vs Shots Against", 
    xaxis = list(title = "Shots Against"),
    yaxis = list(title = "Save %")
  )
#creating a graph showcasing goals against vs goals allowed
#for every goalie
plot_ly(x = goalie_stats_df$Goalie.Shots.Against, 
        y = goalie_stats_df$Goalie.Goals.Allowed,
        type = "scatter",
        mode = "markers", 
        marker = list(size = 10, color = "blue"), 
        text = goalie_stats_df$Goalie.Name, 
        hoverinfo = 'text') %>%
  layout(
    title = "Shots Against vs Goals Allowed", 
    xaxis = list(title = "Shots Against"),
    yaxis = list(title = "Goals Allowed")
  )

#Creating a graph analysing PK Shots Against vs PK Goals Allowed
#for every goalie
plot_ly(x = goalie_stats_df$Goalie.PK.Shots.Against, 
        y = goalie_stats_df$Goalie.PK.Goals.Allowed,
        type = "scatter",
        mode = "markers", 
        marker = list(size = 10, color = "blue"), 
        text = goalie_stats_df$Goalie.Name, 
        hoverinfo = 'text') %>%
  layout(
    title = "PK Shots Against vs PK Goals Allowed", 
    xaxis = list(title = "PK Shots Against"),
    yaxis = list(title = "PK Goals Allowed")
  )

# Creating a graph showcasing PK Shots Against vs PK Save % for 
# every goalie
plot_ly(x = goalie_stats_df$Goalie.PK.Shots.Against, 
        y =  goalie_stats_df$Goalie.PK.Save.., 
        type = "scatter",
        mode = "markers", 
        marker = list(size = 10, color = "blue"), 
        text = goalie_stats_df$Goalie.Name, 
        hoverinfo = 'text') %>%
  layout(
    title = "PK Shots Against vs PK Save %", 
    xaxis = list(title = "PK Shots Against"),
    yaxis = list(title = "PK Save %")
  )


