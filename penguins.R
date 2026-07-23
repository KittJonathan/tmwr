# Tidy Modeling With R
# {palmerpenguins}

# Packages ----

library(palmerpenguins)
library(tidymodels)
tidymodels_prefer()

conflicted::conflicts_prefer(palmerpenguins::penguins)

# Initial split ----

set.seed(42)
penguins_split <- initial_split(data = penguins, prop = 0.8)  # default: prop = 3/4

penguins_train <- training(penguins_split)
penguins_test <- testing(penguins_split)

penguins |> 
  janitor::tabyl(species)

# Adelie:    152 (44%)
# Chinstrap: 68  (20%)
# Gentoo:    124 (36%)

penguins_train |> 
  janitor::tabyl(species)

# Adelie:    121 (44%)
# Chinstrap: 55  (20%)
# Gentoo:    99  (36%)

penguins_test |> 
  janitor::tabyl(species)

# Adelie:    31  (45%)
# Chinstrap: 13  (19%)
# Gentoo:    25  (36%)

# Create the model ----

