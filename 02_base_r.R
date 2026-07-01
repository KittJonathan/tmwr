# Tidy Modeling With R
# A Review of R Modeling Fundamentals
# https://www.tmwr.org/base-r

# Packages ----

library(tidyverse)
theme_set(theme_bw())

# An example ----

data(crickets, package = "modeldata")
names(crickets)

ggplot(crickets,
       aes(x = temp, y = rate, color = species,
           shape = species, linetype = species)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, se = FALSE, alpha = 0.5) +
  scale_color_brewer(palette = "Paired") +
  labs(x = "Temperate (C)", y = "Chirp Rate (per minute)") +
  theme(legend.position = "top")

interaction_fit <- lm(rate ~ (temp + species)^2, data = crickets)

interaction_fit

par(mfrow = c(1, 2))
plot(interaction_fit, which = 1)
plot(interaction_fit, which = 2)

main_effect_fit <- lm(rate ~ temp + species, data = crickets)

anova(main_effect_fit, interaction_fit)

summary(main_effect_fit)

new_values <- data.frame(species = "O. exclamationis", temp = 15:20)
predict(main_effect_fit, new_values)

# Why tidyness is important for modeling ----

new_values$temp[1] <- NA

predict(main_effect_fit, new_values)

predict(main_effect_fit, new_values, na.action = na.fail)

predict(main_effect_fit, new_values, na.action = na.omit)

corr_res <- map(mtcars %>% select(-mpg), cor.test, y = mtcars$mpg)

corr_res[[1]]

broom::tidy(corr_res[[1]])

corr_res |> 
  map_dfr(broom::tidy, .id = "predictor") |> 
  ggplot(aes(x = fct_reorder(predictor, estimate))) +
  geom_point(aes(y = estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = .1) +
  labs(x = NULL, y = "Correlation with mpg")

# Combining base R models and the tidyverse ----

split_by_species <- crickets |> 
  group_nest(species)

split_by_species

model_by_species <- split_by_species |> 
  mutate(model = map(data, ~ lm(rate ~ temp, data = .x)))

model_by_species

model_by_species |> 
  mutate(coef = map(model, broom::tidy)) |> 
  select(species, coef) |> 
  unnest(cols = c(coef))

# The tidymodels metapackage ----

library(tidymodels)

library(conflicted)
conflict_prefer("filter", winner = "dplyr")

tidymodels_prefer(quiet = FALSE)
