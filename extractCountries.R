library(tidyverse)

matches <- read_csv("matches.csv")

home_teams <- 
  matches |> 
    distinct(home_team)

