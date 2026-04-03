transfermarkt <- read_csv("data/transfermarkt.csv")

positions <- transfermarkt |>
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

nrow(positions) == nrow(transfermarkt)

write_csv(positions, "data/transfermarkt.csv")
