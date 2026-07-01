# Tidy Modeling With R
# The Ames Housing Data
# https://www.tmwr.org/ames

# Packages ----

library(tidyverse)
library(tidymodels)
tidymodels_prefer()

# Load the data ----

data("ames")
data(ames, package = "modeldata")

dim(ames)

# Exploring features of homes in Ames ----

ggplot(ames, aes(x = Sale_Price)) +
  geom_histogram(bins = 50, col = "white")

ggplot(ames, aes(x = Sale_Price)) +
  geom_histogram(bins = 50, col = "white") +
  scale_x_log10()

ames <- ames |> 
  mutate(Sale_Price = log10(Sale_Price))

# Chapter summary ----

library(tidymodels)
data(ames)
ames <- ames |> 
  mutate(Sale_Price = log10(Sale_Price))
