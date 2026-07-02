# Tidy Modeling With R
# Fitting Models with parsnip
# https://www.tmwr.org/models

# Packages ----

library(tidyverse)
library(tidymodels)
tidymodels_prefer()

# Load the data ----

data(ames)

ames <- ames %>% mutate(Sale_Price = log10(Sale_Price))

set.seed(502)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

ames_split

# Create the model ----

# Specify the type of the model based on its mathematical structure
# Specify the engine for fitting the model
# When required, declare the mode of the model

linear_reg() |> set_engine("lm")
linear_reg() |> set_engine("glmnet")
linear_reg() |> set_engine("stan")

linear_reg() |> set_engine("lm") |> translate()
linear_reg(penalty = 1) |> set_engine("glmnet") |> translate()
linear_reg() |> set_engine("stan") |> translate()

# Predict the sale price of houses in the Ames data
# as a function of only longitude and latitude.

lm_model <- 
  linear_reg() |>  
  set_engine("lm")

lm_form_fit <- 
  lm_model |>  
  # Recall that Sale_Price has been pre-logged
  fit(Sale_Price ~ Longitude + Latitude, data = ames_train)

lm_xy_fit <- 
  lm_model |>  
  fit_xy(
    x = ames_train |> select(Longitude, Latitude),
    y = ames_train |> pull(Sale_Price)
  )

lm_form_fit
lm_xy_fit

rand_forest(trees = 1000, min_n = 5) |> 
  set_engine("ranger") |> 
  set_mode("regression") |>  
  translate()

rand_forest(trees = 1000, min_n = 5) |> 
  set_engine("ranger", verbose = TRUE) |>  
  set_mode("regression") 

# Use the model results ----

lm_form_fit |> extract_fit_engine()

# vcov(): variance/covariance matrix
lm_form_fit |> extract_fit_engine() |> vcov()

model_res <- 
  lm_form_fit |>  
  extract_fit_engine() |>  
  summary()

model_res

# The model coefficient table is accessible 
# via the `coef` method.
param_est <- coef(model_res)
class(param_est)

param_est

tidy(lm_form_fit)

# Make predictions ----

