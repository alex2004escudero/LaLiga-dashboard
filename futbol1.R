library(DBI)
library(dplyr)
library(RSQLite)

# Connect to the database
con <- dbConnect(RSQLite::SQLite(), "datos/database.sqlite")

# Load tables
match_data <- dbReadTable(con, "Match")
league <- dbReadTable(con, "League")
team <- dbReadTable(con, "Team")

# ✅ Fix: Use == instead of = for filtering
df_years <- match_data %>%
  filter(season %in% c("2013/2014", "2014/2015", "2015/2016"))

# ✅ Fix: Extract Spanish league ID correctly
spanish_league_id <- league %>%
  filter(name == "Spain LIGA BBVA") %>%
  pull(id)

# ✅ Filter match data for Spanish league and selected seasons


spanish_match_years <- df_years %>%
  filter(league_id == spanish_league_id)

# ✅ Join team names
spanish_match_years <- spanish_match_years %>%
  left_join(team, by = c("home_team_api_id" = "team_api_id")) %>%
  rename(home_team_name = team_long_name) %>%
  left_join(team, by = c("away_team_api_id" = "team_api_id")) %>%
  rename(away_team_name = team_long_name)

spanish_match_1314 <- spanish_match_years[spanish_match_years$season == "2013/2014",]

celta13_14 <- spanish_match_years %>% 
  filter(season == "2013/2014", 
         home_team_name == "RC Celta de Vigo" | away_team_name == "RC Celta de Vigo")



get_team_stats <- function(data, team_name, season_input) {
  # Filter for the selected team and season
  team_matches <- data %>%
    filter(season == season_input) %>%
    filter(home_team_name == team_name | away_team_name == team_name) %>%
    mutate(
      is_home = home_team_name == team_name,
      goals_for = ifelse(is_home, home_team_goal, away_team_goal),
      goals_against = ifelse(is_home, away_team_goal, home_team_goal),
      result = case_when(
        goals_for > goals_against ~ "Win",
        goals_for < goals_against ~ "Loss",
        TRUE ~ "Draw"
      ),
      points = case_when(
        result == "Win" ~ 3,
        result == "Draw" ~ 1,
        TRUE ~ 0
      )
    )
  
  # Summarize stats
  summary <- team_matches %>%
    summarise(
      Season = season_input,
      Team = team_name,
      Matches = n(),
      Wins = sum(result == "Win"),
      Draws = sum(result == "Draw"),
      Losses = sum(result == "Loss"),
      Goals_For = sum(goals_for, na.rm = TRUE),
      Goals_Against = sum(goals_against, na.rm = TRUE),
      Points = sum(points),
      Win_Rate = round(100 * sum(result == "Win") / n(), 1)
    )
  
  return(summary)
}

#13/14 season
spanish_match_1314 <- spanish_match_years[spanish_match_years$season == "2013/2014",]
team_names <- unique(c(spanish_match_1314$home_team_name, spanish_match_1314$away_team_name))

# Initialize an empty list to store results
team_stats_list <- list()

# Loop through each team
for (k in seq_along(team_names)) {
  team_stats_list[[k]] <- get_team_stats(spanish_match_1314, team_names[k], "2013/2014")
}

# Combine all results into a single dataframe
team_stats_1314 <- bind_rows(team_stats_list)

# View the result
print(team_stats_1314)


#14/15 season

spanish_match_1415 <- spanish_match_years[spanish_match_years$season == "2014/2015",]
team_names <- unique(c(spanish_match_1415$home_team_name, spanish_match_1415$away_team_name))

# Initialize an empty list to store results
team_stats_list <- list()

# Loop through each team
for (k in seq_along(team_names)) {
  team_stats_list[[k]] <- get_team_stats(spanish_match_years, team_names[k], "2014/2015")
}

# Combine all results into a single dataframe
team_stats_1415 <- bind_rows(team_stats_list)

# View the result
print(team_stats_1415)


#15/16 season
spanish_match_1516 <- spanish_match_years[spanish_match_years$season == "2015/2016",]
team_names <- unique(c(spanish_match_1516$home_team_name, spanish_match_1516$away_team_name))

# Initialize an empty list to store results
team_stats_list <- list()

# Loop through each team
for (k in seq_along(team_names)) {
  team_stats_list[[k]] <- get_team_stats(spanish_match_years, team_names[k], "2015/2016")
}

# Combine all results into a single dataframe
team_stats_1516 <- bind_rows(team_stats_list)

# View the result
print(team_stats_1516)


# Combine all seasons into one dataframe
team_stats_all <- bind_rows(team_stats_1314, team_stats_1415, team_stats_1516)


team_stats_all <- team_stats_all %>%
  group_by(Season) %>%
  arrange(desc(Points)) %>%
  mutate(Rank = row_number()) %>%
  ungroup()




# Export to CSV
write.csv(team_stats_all, "datos/team_stats_all_seasons.csv", row.names = FALSE)


team_names <- unique(spanish_match_years$home_team_name)

unique(spanish_match_1314$home_team_name)
unique(spanish_match_1415$home_team_name)
unique(spanish_match_1516$home_team_name)
