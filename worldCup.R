library(tidyverse)
library(zoo)
library(purrr)

nationalTeams <- 
  c("Mexico",                   #A
    "South Africa",             #A
    "South Korea",              #A
    "Czech Republic",           #A
    "Canada",                   #B
    "Bosnia and Herzegovina",   #B
    "Qatar",                    #B
    "Switzerland",              #B
    "Brazil",                   #C
    "Morocco",                  #C
    "Haiti",                    #C
    "Scotland",                 #C
    "United States",            #D
    "Paraguay",                 #D
    "Australia",                #D
    "Turkey",                   #D
    "Germany",                  #E
    "Curaçao",                  #E
    "Ivory Coast",              #E
    "Ecuador",                  #E
    "Netherlands",              #F
    "Japan",                    #F
    "Sweden",                   #F
    "Tunisia",                  #F
    "Belgium",                  #G
    "Egypt",                    #G
    "Iran",                     #G
    "New Zealand",              #G
    "Spain",                    #H
    "Cape Verde",               #H
    "Saudi Arabia",             #H
    "Uruguay",                  #H
    "France",                   #I
    "Senegal",                  #I
    "Iraq",                     #I
    "Norway",                   #I
    "Argentina",                #J
    "Algeria",                  #J
    "Austria",                  #J
    "Jordan",                   #J
    "Portugal",                 #K
    "DR Congo",                 #K
    "Uzbekistan",               #K
    "Colombia",                 #K
    "England",                  #L
    "Croatia",                  #L
    "Ghana",                    #L
    "Panama")                   #L


################ Build the worldcup dataset ############################
possible_matches <-  expand.grid( #Create all possible matches
  home_team = nationalTeams,
  away_team = nationalTeams
)

possible_matches$date <- "2026-06-01" #For date the important thing is that is in 2026 and after previous matches
possible_matches$result <- "tie" #dummy result 


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

dataset <- rbind(dataset, possible_matches)

points <- dataset |>
  distinct(home_team, date, .keep_all = TRUE) |>
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
  select(date, country, points) |>
  distinct()

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

dataset <- dataset |>
  filter(date >= '2026-06-01')

elo_rating <- read_csv("data/elo_rating.csv") |>
  mutate(team = recode(team,
                       "Czechia" = "Czech Republic")) |>
  select(year, team, rating)

elo_rating <- elo_rating |>
  mutate(year = year + 1) |>
  select(year, team, rating)

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

dataset <- dataset |>
  mutate(elo_rating_diff = home_elo_rating - away_elo_rating,
         last_5_games_diff = home_team_last_5_games - away_team_last_5_games,
         goalkeeper_market_value_diff = home_goalkeeper_market_value - away_goalkeeper_market_value,
         defender_market_value_diff = home_defender_market_value - away_defender_market_value,
         midfielder_marker_value_diff = home_midfielder_market_value - away_midfielder_market_value,
         attacker_market_value_diff = home_attacker_market_value - away_attacker_market_value,
         goalkeeper_age_diff = home_goalkeeper_age - away_goalkeeper_age,
         defender_age_diff = home_defender_age - away_defender_age,
         midfielder_age_diff = home_midfielder_age - away_midfielder_age,
         attacker_age_diff = home_attacker_age - away_attacker_age) |>
  select(date, 
         home_team, 
         away_team, 
         ends_with("diff"),
         result)
#################################################################################

model <- readRDS("fittedModel.rds") 

predictMatch <- function(team_1, team_2){
  p <- rbinom(1, 1, 0.5)
  
  if(p < 0.5){
    ht <- team_1
    at <- team_2
  }
  else{
    ht <- team_2
    at <- team_1
  }
  
  observation <- dataset |> 
    filter(home_team == ht, away_team == at)
  
  prediction <- predict(model, observation)
  
  if(prediction == "win"){
    result <- c(3,0) 
  }else if(prediction == "tie"){
    result <- c(1,1)
  }else{
    result <- c(0,3)
  }
  
  data.frame(country = c(ht, at), result)
}

predictMatch("Australia", "Uruguay")


############ PREDICT GROUP STAGE ################
groups <- lapply(split(nationalTeams, rep(LETTERS[1:12], each = 4)), as.vector)
g <- lapply(groups, function(x){
  matches <- combn(x,2)
  results <- apply(matches, 2, function(m){
    predictMatch(m[[1]], m[[2]])
  })
  #browser()
  results <- do.call(rbind, results)
  results <- as.data.frame(results) |>
    group_by(country) |>
    summarise(points = sum(result)) |>
    arrange(desc(points)) |>
    select(country)
  results[1:3,]
})

group_stage <- do.call(cbind, g) 
colnames(group_stage) <- LETTERS[1:12]
group_stage <- t(group_stage)

##################################################

create_round_of_32  <- function(x) {
  rownames(x) <- LETTERS[1:12]
  colnames(x) <- c("W", "RU", "T")
  
  team <- function(g, place) x[g, place]
  
  # sample 8 third-place groups at random
  third_groups <- sample(LETTERS[1:12], 8)
  
  data.frame(
    match = paste0("M", 73:88),
    team_1 = c(
      team("A","RU"), team("E","W"),  team("F","W"),  team("C","W"),
      team("I","W"),  team("E","RU"), team("A","W"),  team("L","W"),
      team("D","W"),  team("G","W"),  team("K","RU"), team("H","W"),
      team("B","W"),  team("D","RU"), team("J","W"),  team("K","W")
    ),
    team_2 = c(
      team("B","RU"),
      team(third_groups[1], "T"),
      team("C","RU"),
      team("F","RU"),
      team(third_groups[2], "T"),
      team("I","RU"),
      team(third_groups[3], "T"),
      team(third_groups[4], "T"),
      team(third_groups[5], "T"),
      team(third_groups[6], "T"),
      team("L","RU"),
      team("J","RU"),
      team(third_groups[7], "T"),
      team("G","RU"),
      team("H","RU"),
      team(third_groups[8], "T")
    ),
    stringsAsFactors = FALSE
  )
}

predictWhoGoNextStage <- function(team_1, team_2){
  result <- predictMatch(team_1, team_2)
  if(result[result$country == team_1, 2] == 3){
    team_1
  }else if(result[result$country == team_1, 2] == 1){
    sample(c(team_1, team_2), 1) #On tie choose 1 by random
  }else{
    team_2
  }
}

r32 <- create_round_of_32(group_stage)
r32_winners <- apply(r32, 1, function(x){
   predictWhoGoNextStage(x["team_1"], x["team_2"])
})

names(r32_winners) <- r32$match
  
r16 = data.frame(
  match = paste0("M", 89:96),
  team_1 = c(r32_winners["M73"], r32_winners["M74"], r32_winners["M76"], r32_winners["M79"],
           r32_winners["M83"], r32_winners["M81"], r32_winners["M86"], r32_winners["M85"]),
  team_2 = c(r32_winners["M75"], r32_winners["M77"], r32_winners["M78"], r32_winners["M80"],
           r32_winners["M84"], r32_winners["M82"], r32_winners["M88"], r32_winners["M87"])
)

r16_winners <- apply(r16, 1, function(x){
  predictWhoGoNextStage(x["team_1"], x["team_2"])
})
names(r16_winners) <- r16$match


qf = data.frame(
  match = paste0("M", 97:100),
  team_1 = c(r16_winners["M89"], r16_winners["M91"], r16_winners["M93"], r16_winners["M95"]),
  team_2 = c(r16_winners["M90"], r16_winners["M92"], r16_winners["M94"], r16_winners["M96"])
)
qf_winners <- apply(qf, 1, function(x){
  predictWhoGoNextStage(x["team_1"], x["team_2"])
})
names(qf_winners) <- qf$match


sf = data.frame(
  match = paste0("M", 100:101),
  team_1 = c(qf_winners["M97"], qf_winners["M99"]),
  team_2 = c(qf_winners["M98"], qf_winners["M100"])
)
sf_winners <- apply(sf, 1, function(x){
  predictWhoGoNextStage(x["team_1"], x["team_2"])
})
names(sf_winners) <- sf$match

final <-  predictWhoGoNextStage(sf_winners["M100"], sf_winners["M101"])
print(sprintf("And the world cup goes too ... %s", final[1]))
