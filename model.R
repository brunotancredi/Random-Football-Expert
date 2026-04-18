library(tidyverse)
library(tidymodels)
library(doParallel)
library(vip) 

set.seed(123)

#Load matches
matches <- read_csv("matches.csv")

#Use diff of points on the last 5 matches and ELO difference
matches <- matches |>
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


##### Modelling 

#Train/Test Split
split <- initial_split(matches)
train <- training(split)
test <- testing(split)

#Define model (Random Forest)
rf_model <-
  rand_forest(
    mtry = tune(), #Number of selected variables
    trees = 1000,
    min_n = tune() #the number of observations needed to keep splitting nodes
  ) |>
  set_mode("classification") |>
  set_engine("ranger",
             importance = "impurity")

#Formula and set variables as ID so they're not used as predictors
rf_recipe <- recipe(result ~ ., data = train) |>
  update_role(date, new_role = "ID") |>
  update_role(home_team, new_role = "ID") |>
  update_role(away_team, new_role = "ID") 
  
#Tidymodel concept
rf_workflow <- workflow() |>
  add_recipe(rf_recipe) |>
  add_model(rf_model) 

#Cross-Validation  
rf_folds <- vfold_cv(train, v = 10)

#Latin Hypercubes to build search space [grid] (explore this later)
rf_grid <- grid_latin_hypercube(
  mtry(c(1, 2)), #mtry vary between 1 
  min_n(),
  size = 10
)

cl <- makeCluster(15)
registerDoParallel(cl) #Parallelize to avoid yikes

#Run the cross validation
rf_tune_res <- tune_grid(
  rf_workflow,
  resamples = rf_folds,
  grid = rf_grid,
  control = control_resamples(save_pred = TRUE, save_workflow = TRUE)
)

#Choose the best
rf_best <- select_best(rf_tune_res, metric = "brier_class")

rf_workflow <- rf_workflow |> finalize_workflow(rf_best)

#Fit the rf with the best parameters
rf_fit <- fit(rf_workflow, train)

#Predict test
test_with_predictions <- augment(rf_fit, test) |>
  mutate(
    result = as.factor(result),
    .pred_class = as.factor(.pred_class)
  )

accuracy(
  test_with_predictions,
  truth = result,
  estimate = .pred_class
)

#68.3% Accuracy!!

#Variable importance
importance <- extract_fit_parsnip(rf_fit)
vip(importance)
