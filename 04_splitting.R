# Tidy Modeling With R
# Spending our Data
# https://www.tmwr.org/splitting

# Packages ----

library(tidyverse)
library(tidymodels)
tidymodels_prefer()

# Load the data ----

data(ames)

ames <- ames |> 
  mutate(Sale_Price = log10(Sale_Price))

# Common methods for splitting data -----

# Set the random number stream using `set.seed()` so that the 
# results can be reproduced later.
set.seed(501)

# Save the split information for an 80/20 split of the data
ames_split <- initial_split(ames, prop = 0.80)
ames_split

ames_train <- training(ames_split)
ames_test <- testing(ames_split)

dim(ames_train)

# Class imbalance: one class occurs much less frequently than another.
# To avoid this -> stratified sampling.

# The training/test split is conducted separately within each class
# and then these subsamples are combined into the overall training 
# and test set.

# For regression problems, the outcome data can be artificially binned
# into quartiles and then stratified sampling can be conducted four
# separate times.

set.seed(502)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test <- testing(ames_split)

# What about a validation set? ----

set.seed(52)
# 60% into training, 20% in validation, and 20% in testing
ames_val_split <- initial_validation_split(ames, prop = c(0.6, 0.2))
ames_val_split

ames_train <- training(ames_val_split)
ames_test <- testing(ames_val_split)
ames_val <- validation(ames_val_split)

# Chapter summary ----

library(tidymodels)
data(ames)
ames <- ames %>% mutate(Sale_Price = log10(Sale_Price))

set.seed(502)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)
