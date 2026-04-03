library(tidyverse)
library(logr)

devtools::load_all("../worldfootballR")

countries <- read_csv("transfermarkt_countries.csv")
years <- 2018:2025

tmp <- file.path(getwd(), "test.log")

lf <- log_open(tmp)
final_df <- data.frame()

t <- apply(countries[1:5, ], 1, function(country){
  rows <- list()
  i <- 1
  for( year in years[1:2]){
    tryCatch({
      link <- paste0(country["links"], "/saison_id/", year)
      print(sprintf(
        "Retrieving %s for year %s. Link: %s", 
        country["country"], 
        year,
        link
      ))
      vals <- tm_each_team_player_market_val(each_team_url = link) 
      row <- vals |>
        select(player_age, player_position, player_market_value_euro) |>
        drop_na() |>
        group_by(player_position) |>
        summarize(
          market_value = mean(player_market_value_euro),
          age = mean(player_age)
        ) |>
        mutate(
          player_position = player_position |>
            str_to_lower() |>           
            str_replace_all(" ", "_")
        ) |>
        pivot_wider(
          names_from = player_position,
          values_from = c(market_value, age),
          names_glue = "{player_position}_{.value}",
        ) |>
        mutate(country = country[["country"]],
               year = year)
      rows[[i]] <- row
      i <- i+1
    },
    error = function(msg) {
      where <- sprintf("There was an error in %s, year %s", country, year)
      log_print(where)
      log_close()
      print(paste0(where, msg))
    })
  }
  bind_rows(rows)
})

final_dataframe <- bind_rows(t) |>
  relocate(year) |>
  relocate(country)

#Rename positions
final_dataframe <- final_dataframe |>
  pivot_longer(
    cols = -c(country, year),
    names_to = c("position", ".value"),
    names_pattern = "(.+)_(market_value|age)"
  ) |>
  mutate(
    position = recode(
      position,
      "attacking_midfield" = "midfielder",
      "central_midfield" = "midfielder",
      "centre-back" = "defender",
      "centre-forward" = "attacker",
      "defensive_midfield" = "midfielder",
      "goalkeeper" = "goalkeeper",
      "left_winger" = "attacker",
      "left-back" = "defender",
      "right_winger" = "attacker",
      "right-back" = "defender",
      "second_striker" = "attacker",
      "left_midfield" = "midfielder",
      "right_midfield" = "midfielder",
      "striker" = "attacker"
    )
  ) |>
  drop_na() |>
  group_by(country, year, position) |>
  summarise(
    market_value = mean(market_value),
    age = mean(age)
  ) |>
  ungroup() |>
  pivot_wider(
    names_from = position,
    values_from = c(market_value, age),
    names_glue = "{position}_{.value}"
  )


## Sanity checks

#Contries for which we don't have data
no_data <- countries |> 
  left_join(df) |>
  filter(is.na(year)) |>
  select(country)

#Contries for which we have data but not all the years
df |>
  group_by(country) |>
  summarize(n = n()) |>
  filter(n < 7)

write_csv(final_dataframe, "data/transfermarkt.csv")

