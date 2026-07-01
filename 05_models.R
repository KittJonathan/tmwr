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
