library(tidyverse)
library(zoo)
library(purrr)

#Load results.csv (football matches from 1872 to 2026)
matches <- read_csv("data/results.csv")
head(matches)

dataset <- matches |> 
  filter(date >= '2018-01-01') |> #We want to work only with recent years
  distinct(home_team, date, .keep_all = TRUE) |> #Fiji has two matches on the same day
  distinct(away_team, date, .keep_all = TRUE) |> #Saint Kitts and Nevis has two matches on the same day
  mutate(
    result = case_when(
      home_score > away_score ~ "win",
      home_score == away_score ~ "tie",
      TRUE ~ "lose"
     ), #Define result column as response: win -> home_team won the match and so on.
    result = factor(result)
  ) |>
  select(date, home_team, away_team, result) #We only care about this covariates.

###################### Obtain results on the five last game #################################

#We are interested to use as covariate the performance on the last games
#We can use the "points" that the team acummulated on the last games (3 win, 1 tie, 0 lose)
#And then make the sum.

points <- dataset |>
  mutate(
    home_points = case_when(
      result == "win" ~ 3, 
      result == "tie" ~ 1,
      TRUE ~ 0
    )
  ) |>
  select(-result) |>
  pivot_longer(
    c(home_team, away_team),
    names_to = "role",
    values_to = "country"
  ) |> #We split each row in one role for the home team and one row for the away team
  mutate(
    points = case_when( #For the away team we need to invert the points
      role == "away_team" & home_points == 3 ~ 0, 
      role == "away_team" & home_points == 0 ~ 3, 
      TRUE ~ home_points
    )
  ) |>
  select(date, country, points)

#We sum the points of the last 5 games, when we don't have 5 games we fill with NA
points_last_five_games <- points |>
                      arrange(country, date) |> 
                      group_by(country) |>
                      mutate(last_five = rollsumr(points, k = 5, fill = NA)) |> #Make the sum
                      select(country, date, last_five)

#Add the calculated points to the dataset
dataset <- dataset |>
  inner_join(points_last_five_games, 
             by = join_by(home_team == country, date == date)) |> #get the home_team points
  rename(home_team_last_5_games = last_five) |>
  inner_join(points_last_five_games, 
             by = join_by(away_team == country, date == date)) |> #get the away_team points
  rename(away_team_last_5_games = last_five) |>
  drop_na()
####################

############### Add ELO Ranking to dataset #####################
elo_rating <- read_csv("data/elo_rating.csv")

#To avoid data lakeage, since ranking is calculated using the result of the matches, 
# we need to move to use the ranking of the previous year
elo_rating <- elo_rating |>
  mutate(year = year + 1) |>
  select(year, team, rating)

dataset_2 <- dataset |> 
  mutate(year = year(date)) |>
  left_join(elo_rating, 
             by = join_by(home_team == team, year == year)) 

countries_without_elo <- dataset_2 |> 
  filter(is.na(rating)) |>
  distinct(home_team)

nrow(countries_without_elo)
countries_without_elo

#Here we have to define what we want to do, try to recover them or delete them.
# For example on the ELO Czech Republic is Czechia
elo_rating <- elo_rating |>
  mutate(team = recode(team,
                        "Czechia" = "Czech Republic")) |>
  select(year, team, rating)


#For now, I will ignore those countries (we lost ~ 400 matches)
dataset <- dataset |> 
  mutate(year = year(date)) |> 
  inner_join(elo_rating, 
            by = join_by(home_team == team, year == year)) |>
  mutate(home_elo_rating = rating) |>
  select(-c(rating)) |>
  inner_join(elo_rating, 
             by = join_by(away_team == team, year == year)) |>
  mutate(away_elo_rating = rating) |>
  select(-c(year,rating)) 
#################

## Join with transfermarkt
transfermarkt <- read_csv("data/transfermarkt.csv") 

dataset <- dataset |> 
  mutate(year = year(date)) |> 
  inner_join(
    transfermarkt |>
      rename_with(~ paste0("home_", .x), -c(country,year)), 
    by = join_by(home_team == country, year == year)) |>
  inner_join(
    transfermarkt |>
      rename_with(~ paste0("away_", .x), -c(country,year)), 
    by = join_by(away_team == country, year == year)) |>
  select(-c(year))
#####

#Now we are ready for export

#Only for better reading I will put the response at the end
dataset <- dataset |>
  relocate(result, .after = last_col())

write_csv(dataset, "matches.csv")
