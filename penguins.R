# Tidymodels on Palmer Penguins data

# Packages ----

library(palmerpenguins)
library(tidymodels)
tidymodels_prefer()

conflicts_prefer(palmerpenguins::penguins)

# Initial split ----

penguins |> 
  janitor::tabyl(species)

# Adelie:    152 (44%)
# Chinstrap: 68  (20%)
# Gentoo:    124 (36%)

set.seed(42)

penguins_split_01 <- initial_split(penguins, prop = 0.80)

penguins_train_01 <- training(penguins_split_01)
penguins_test_01 <- testing(penguins_split_01)

penguins_train_01 |> 
  janitor::tabyl(species)

# Adelie:    121 (44%)
# Chinstrap: 55 (20%)
# Gentoo:    99 (36%)

penguins_test_01 |> 
  janitor::tabyl(species)

# Adelie:    31 (45%)
# Chinstrap: 13 (19%)
# Gentoo:    25 (36%)

penguins |> 
  janitor::tabyl(island)

# Biscoe:    168 (49%)
# Dream:     124 (36%)
# Torgersen: 52 (15%)

set.seed(42)

penguins_split_02 <- initial_split(penguins, prop = 0.80)

penguins_train_02 <- training(penguins_split_02)
penguins_test_02 <- testing(penguins_split_02)

penguins_train_02 |> 
  janitor::tabyl(island)

# Biscoe:    135 (49%)
# Dream:     100 (36%)
# Torgersen: 40  (15%)

penguins_test_02 |> 
  janitor::tabyl(island)

# Biscoe:    33 (48%)
# Dream:     24 (35%)
# Torgersen: 12 (17%)

ggplot(penguins) +
  geom_histogram(aes(x = body_mass_g))

set.seed(42)
penguins_split_03 <- initial_split(penguins, 0.80, strata = flipper_length_mm)

penguins_train_03 <- training(penguins_split_03)
penguins_test_03 <- testing(penguins_split_03)

set.seed(42)
penguins_split_04 <- initial_split(penguins, 0.80, strata = body_mass_g)

penguins_train_04 <- training(penguins_split_04)
penguins_test_04 <- testing(penguins_split_04)

penguins |> 
  mutate(set = "raw", .before = species) |> 
  bind_rows(mutate(penguins_train_01, set = "train", .before = species)) |> 
  bind_rows(mutate(penguins_test_01, set = "test", .before = species)) |> 
  ggplot() +
  geom_boxplot(aes(x = set, y = flipper_length_mm))

penguins |> 
  mutate(set = "raw", .before = species) |> 
  bind_rows(mutate(penguins_train_03, set = "train", .before = species)) |> 
  bind_rows(mutate(penguins_test_03, set = "test", .before = species)) |> 
  ggplot() +
  geom_boxplot(aes(x = set, y = flipper_length_mm))

penguins |> 
  mutate(set = "raw", .before = species) |> 
  bind_rows(mutate(penguins_train_01, set = "train", .before = species)) |> 
  bind_rows(mutate(penguins_test_01, set = "test", .before = species)) |> 
  ggplot() +
  geom_boxplot(aes(x = set, y = body_mass_g))

penguins |> 
  mutate(set = "raw", .before = species) |> 
  bind_rows(mutate(penguins_train_04, set = "train", .before = species)) |> 
  bind_rows(mutate(penguins_test_04, set = "test", .before = species)) |> 
  ggplot() +
  geom_boxplot(aes(x = set, y = body_mass_g))
