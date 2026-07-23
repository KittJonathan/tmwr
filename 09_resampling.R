# Tidy Modeling With R
# Resampling for Evaluating Performance
# https://www.tmwr.org/resampling

# Packages ----

library(tidyverse)
library(tidymodels)
tidymodels_prefer()

# library(splines)

# Load the data ----

data(ames)

ames <- mutate(ames, Sale_Price = log10(Sale_Price))

set.seed(502)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

ames_rec <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train) |>
  step_log(Gr_Liv_Area, base = 10) |> 
  step_other(Neighborhood, threshold = 0.01) |> 
  step_dummy(all_nominal_predictors()) |> 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) |> 
  step_ns(Latitude, Longitude, deg_free = 20)

lm_model <- linear_reg() |> set_engine("lm")

lm_wflow <- 
  workflow() |> 
  add_model(lm_model) |> 
  add_recipe(ames_rec)

lm_fit <- fit(lm_wflow, ames_train)

# The resubstitution approach ----

rf_model <- 
  rand_forest(trees = 1000) |> 
  set_engine("ranger") |> 
  set_mode("regression")

rf_wflow <- 
  workflow() |> 
  add_formula(
    Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
      Latitude + Longitude) |> 
  add_model(rf_model)

rf_fit <- rf_wflow |> fit(data = ames_train)

estimate_perf <- function(model, dat) {
  # Capture the names of the `model` and `dat` objects
  cl <- match.call()
  obj_name <- as.character(cl$model)
  data_name <- as.character(cl$dat)
  data_name <- gsub("ames_", "", data_name)
  
  # Estimate these metrics:
  reg_metrics <- metric_set(rmse, rsq)
  
  model |> 
    predict(dat) |> 
    bind_cols(dat |> select(Sale_Price)) |> 
    reg_metrics(Sale_Price, .pred) |> 
    select(-.estimator) |> 
    mutate(object = obj_name, data = data_name)
}

estimate_perf(rf_fit, ames_train)

estimate_perf(lm_fit, ames_train)

estimate_perf(rf_fit, ames_test)

# Resampling methods ----

## Cross-validation ----

set.seed(1001)
ames_folds <- vfold_cv(ames_train, v = 10)
ames_folds

# For the first fold:
ames_folds$splits[[1]] |> analysis() |> dim()

# Repeated cross-validation
vfold_cv(ames_train, v = 10, repeats = 5)

# Leave-one-out cross-validation
loo_cv(ames_train)

# Monte Carlo cross-validation
mc_cv(ames_train, prop = 9/10, times = 20)

## Validation sets ----

set.seed(52)
# To put 60% into training, 20% in validation and 20% in testing:
ames_val_split <- initial_validation_split(ames, prop = c(0.6, 0.2))
ames_val_split

# Object used for resampling:
val_set <- validation_set(ames_val_split)
val_set

## Bootstrapping ----

bootstraps(ames_train, times = 5)

## Rolling forecasting origin resampling ----

time_slices <- 
  tibble(x = 1:365) |> 
  rolling_origin(initial = 6 * 30, assess = 30, skip = 29, cumulative = FALSE)

data_range <- function(x) {
  summarise(x, first = min(x), last = max(x))
}

map_dfr(time_slices$splits, ~ analysis(.x) |> data_range())

map_dfr(time_slices$splits, ~ assessment(.x) |> data_range())

# Estimating performance ----

keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)

set.seed(1003)

rf_res <- 
  rf_wflow |> 
  fit_resamples(resamples = ames_folds, control = keep_pred)

rf_res

collect_metrics(rf_res)
collect_metrics(rf_res, summarize = FALSE)

assess_res <- collect_predictions(rf_res)
assess_res

assess_res |> 
  ggplot(aes(x = Sale_Price, y = .pred)) +
  geom_point(alpha = .15) +
  geom_abline(color = "red") +
  coord_obs_pred() +
  ylab("Predicted")

over_predicted <- 
  assess_res |> 
  mutate(residual = Sale_Price - .pred) |> 
  arrange(desc(abs(residual))) |> 
  slice(1:2)

over_predicted

ames_train |> 
  slice(over_predicted$.row) |> 
  select(Gr_Liv_Area, Neighborhood, Year_Built, Bedroom_AbvGr, Full_Bath)

val_res <- rf_wflow |> fit_resamples(resamples = val_set)
val_res
collect_metrics(val_res)

# Parallel processing ----

# The number of physical cores in the hardware:
parallel::detectCores(logical = FALSE)  # 10

# The number of possible independent processes that can be
# simultaneously used:
parallel::detectCores(logical = TRUE)  # 12

# Create a cluster object and then register:
cl <- parallel::makePSOCKcluster(10)
doParallel::registerDoParallel(cl)

# Fit resamples
rf_wflow |> 
  fit_resamples(resamples = ames_folds, control = keep_pred)

parallel::stopCluster(cl)

# Saving the resampled objects ----

ames_rec <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type +
           Latitude + Longitude, data = ames_train) |> 
  step_other(Neighborhood, threshold = 0.01) |> 
  step_dummy(all_nominal_predictors()) |> 
  step_interact(~ Gr_Liv_Area:starts_with("Bldg_Type_")) |> 
  step_ns(Latitude, Longitude, deg_free = 20)

ames_rec

lm_wflow <- 
  workflow() |> 
  add_recipe(ames_rec) |> 
  add_model(linear_reg() |> set_engine("lm"))

lm_wflow

lm_fit <- lm_wflow |> fit(data = ames_train)

# Select the recipe:
extract_recipe(lm_fit, estimated = TRUE)

# Save the linear model coefficients for a fitted model object from a workflow:
get_model <- function(x) {
  extract_fit_parsnip(x) |> tidy()
}

get_model(lm_fit)

ctrl <- control_resamples(extract = get_model)

lm_res <- lm_wflow |> fit_resamples(resamples = ames_folds, control = ctrl)
lm_res

lm_res$.extracts[[1]]
lm_res$.extracts[[1]][[1]]

all_coef <- map_dfr(lm_res$.extracts, ~ .x[[1]][[1]])

# Show the replicates for a single predictor:
filter(all_coef, term == "Year_Built")

# Chapter summary ----

set.seed(502)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

ames_rec <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train) |>
  step_log(Gr_Liv_Area, base = 10) |> 
  step_other(Neighborhood, threshold = 0.01) |> 
  step_dummy(all_nominal_predictors()) |> 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) |> 
  step_ns(Latitude, Longitude, deg_free = 20)

lm_model <- linear_reg() |> set_engine("lm")

lm_wflow <- 
  workflow() |> 
  add_model(lm_model) |> 
  add_recipe(ames_rec)

lm_fit <- fit(lm_wflow, ames_train)

rf_model <- 
  rand_forest(trees = 1000) |> 
  set_engine("ranger") |> 
  set_mode("regression")

rf_wflow <- 
  workflow() |> 
  add_formula(
    Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
      Latitude + Longitude) |> 
  add_model(rf_model) 

set.seed(1001)
ames_folds <- vfold_cv(ames_train, v = 10)

keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)

set.seed(1003)
rf_res <- rf_wflow |> fit_resamples(resamples = ames_folds, control = keep_pred)
