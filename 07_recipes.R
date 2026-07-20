# Tidy Modeling With R
# Feature Engineering with recipes
# https://www.tmwr.org/recipes

# Packages ----

library(tidyverse)
library(tidymodels)
tidymodels_prefer()

library(patchwork)
library(splines)

# Load the data ----

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

ggplot(ames_train, aes(x = Gr_Liv_Area, y = 10^Sale_Price)) + 
  geom_point(alpha = .2) + 
  facet_wrap(~ Bldg_Type) + 
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "lightblue") + 
  scale_x_log10() + 
  scale_y_log10() + 
  labs(x = "Gross Living Area", y = "Sale Price (USD)")

# Base R formula for interactions uses ':'
# Sale_Price ~ Neighborhood + log10(Gr_Liv_Area) + Bldg_Type + log10(Gr_Liv_Area):Bldg_Type
# or
# Sale_Price ~ Neighborhood + log10(Gr_Liv_Area) * Bldg_Type

simple_ames <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type,
         data = ames) |> 
  step_log(Gr_Liv_Area, base = 10) |> 
  step_other(Neighborhood, threshold = 0.01) |> 
  step_dummy(all_nominal_predictors()) |> 
  # Gr_Liv_Area is on the log scale from a previous step
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") )

## Spline functions ----

plot_smoother <- function(deg_free) {
  ggplot(ames_train, aes(x = Latitude, y = 10^Sale_Price)) + 
    geom_point(alpha = .2) + 
    scale_y_log10() +
    geom_smooth(
      method = lm,
      formula = y ~ ns(x, df = deg_free),
      color = "lightblue",
      se = FALSE
    ) +
    labs(title = paste(deg_free, "Spline Terms"),
         y = "Sale Price (USD)")
}

( plot_smoother(2) + plot_smoother(5) ) / ( plot_smoother(20) + plot_smoother(100) )

recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + Latitude,
       data = ames_train) |> 
  step_log(Gr_Liv_Area, base = 10) |>  
  step_other(Neighborhood, threshold = 0.01) |>  
  step_dummy(all_nominal_predictors()) |> 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) |>  
  step_ns(Latitude, deg_free = 20)

# Tidy a recipe() ----

ames_rec <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train) |>
  step_log(Gr_Liv_Area, base = 10) |> 
  step_other(Neighborhood, threshold = 0.01) |> 
  step_dummy(all_nominal_predictors()) |> 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) |> 
  step_ns(Latitude, Longitude, deg_free = 20)

tidy(ames_rec)

ames_rec <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train) |>
  step_log(Gr_Liv_Area, base = 10) |> 
  step_other(Neighborhood, threshold = 0.01, id = "my_id") |> 
  step_dummy(all_nominal_predictors()) |> 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) |> 
  step_ns(Latitude, Longitude, deg_free = 20)

lm_wflow <- 
  workflow() |> 
  add_model(lm_model) |> 
  add_recipe(ames_rec)

lm_fit <- fit(lm_wflow, ames_train)

estimated_recipe <- 
  lm_fit |> 
  extract_recipe(estimated = TRUE)

tidy(estimated_recipe, id = "my_id")

tidy(estimated_recipe, number = 2)

# Column roles ----

# ames_rec |> update_role(address, new_role = "street address")
