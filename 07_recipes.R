# Tidy Modeling With R
# Feature Engineering with recipes
# https://www.tmwr.org/recipes

# Packages ----

library(tidyverse)
library(tidymodels)
tidymodels_prefer()

# Load the data ----

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

# A simple recipe() for the Ames housing data ----

lm(Sale_Price ~ Neighborhood + log10(Gr_Liv_Area) + Year_Built + Bldg_Type,
   data = ames)

simple_ames <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type,
         data = ames_train) |> 
  step_log(Gr_Liv_Area, base = 10) |> 
  step_dummy(all_nominal_predictors())

simple_ames

# Using recipes ----

lm_wflow |> 
  add_recipe(simple_ames)

lm_wflow <- 
  lm_wflow |> 
  remove_variables() |> 
  add_recipe(simple_ames)

lm_wflow

lm_fit <- fit(lm_wflow, ames_train)

predict(lm_fit, ames_test |> slice(1:3))

lm_fit |> 
  extract_recipe(estimated = TRUE)

lm_fit |> 
  extract_fit_parsnip() |> 
  tidy() |> 
  slice(1:5)

# Examples of recipe steps ----

## Encoding qualitative data in a numeric format ----

simple_ames <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type,
         data = ames_train) |> 
  step_log(Gr_Liv_Area, base = 10) |> 
  step_other(Neighborhood, threshold = 0.01) |> 
  step_dummy(all_nominal_predictors())

simple_ames

## Interaction terms ----

