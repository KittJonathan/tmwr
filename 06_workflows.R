# Tidy Modeling With R
# A Model Workflow
# https://www.tmwr.org/workflows

# Packages ----

library(tidyverse)
library(tidymodels)
tidymodels_prefer()

# Load the data ----

data(ames)

ames <- ames |> mutate(Sale_Price = log10(Sale_Price))

set.seed(502)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

# Workflow basics ----

lm_model <- 
  linear_reg() |> 
  set_engine("lm")

lm_wflow <- 
  workflow() |> 
  add_model(lm_model)

lm_wflow

lm_wflow <- 
  lm_wflow |> 
  add_formula(Sale_Price ~ Longitude + Latitude)

lm_wflow

lm_fit <- fit(lm_wflow, ames_train)
lm_fit

predict(lm_fit, ames_test |> slice(1:3))

lm_fit |> update_formula(Sale_Price ~ Longitude)

# Adding raw variables to the workflow() ----

lm_wflow <- 
  lm_wflow |> 
  remove_formula() |> 
  add_variables(
    outcome = Sale_Price, 
    predictors = c(Longitude, Latitude)
    )

lm_wflow

fit(lm_wflow, ames_train)

# How does a workflow() use the formula? ----

## Special formulas and inline functions ----

library(lme4)

lmer(distance ~ Sex + (age |  Subject), data = nlme::Orthodont)

model.matrix(distance ~ Sex + (age | Subject), data = nlme::Orthodont)

library(multilevelmod)

multilevel_spec <- linear_reg() |> set_engine("lmer")

multilevel_wflow <- 
  workflow() |> 
  add_variables(outcome = distance, predictors = c(Sex, age, Subject)) |> 
  add_model(multilevel_spec,
            formula = distance ~ Sex + (age |  Subject))

multilevel_fit <- fit(multilevel_wflow, data = nlme::Orthodont)
multilevel_fit

library(censored)

parametric_spec <- survival_reg()

parametric_wflow <- 
  workflow() |> 
  add_variables(
    outcome = c(fustat, futime),
    predictors = c(age, rx)
  ) |> 
  add_model(parametric_spec,
            formula = Surv(futime, fustat) ~ age + strata(rx))

parametrix_fit <- fit(parametric_wflow, data = ovarian)

parametrix_fit

# Creating multiple workflows at once ----

location <- list(
  longitude = Sale_Price ~ Longitude,
  latitude = Sale_Price ~ Latitude,
  coords = Sale_Price ~ Longitude + Latitude,
  neighborhood = Sale_Price ~ Neighborhood
)

library(workflowsets)

location_models <- workflow_set(preproc = location, 
                                models = list(lm = lm_model))

location_models

location_models$info[[1]]

extract_workflow(location_models, id = "coords_lm")

location_models <- 
  location_models |> 
  mutate(fit = map(info, ~ fit(.x$workflow[[1]], ames_train)))

location_models

location_models$fit[[1]]

# Evaluating the test set ----

final_lm_res <- last_fit(lm_wflow, ames_split)
final_lm_res

fitted_lm_wflow <- extract_workflow(final_lm_res)

collect_metrics(final_lm_res)
collect_predictions(final_lm_res) |> slice(1:5)

# Chapter summary ----

library(tidymodels)
data(ames)

ames <- mutate(ames, Sale_Price = log10(Sale_Price))

set.seed(502)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

lm_model <- linear_reg() %>% set_engine("lm")

lm_wflow <- 
  workflow() %>% 
  add_model(lm_model) %>% 
  add_variables(outcome = Sale_Price, predictors = c(Longitude, Latitude))

lm_fit <- fit(lm_wflow, ames_train)
