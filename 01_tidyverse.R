# Tidy Modeling With R
# A Tidyverse Primer
# https://www.tmwr.org/tidyverse

# Packages ----

library(tidyverse)

# Tidyverse principles ----

## Reuse existing data structures ----

boot_samp <- rsample::bootstraps(mtcars, times = 3)
boot_samp
class(boot_samp)

## Design for the pipe and functional programming ----

small_mtcars <- mtcars |> 
  arrange(gear) |> 
  slice(1:10)

ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  geom_smooth(method = lm)

map(head(mtcars$mpg, 3), sqrt)

map_dbl(head(mtcars$mpg, 3), sqrt)

map2_dbl(.x = mtcars$mpg, .y = mtcars$wt, .f = ~ log(.x/.y)) |> 
  head()

# Examples of tidyverse syntax ----

data.frame(`variable 1` = 1:2, two = 3:4)

df <- data.frame(`variable 1` = 1:2, two = 3:4, check.names = FALSE)
df

tbbl <- tibble(`variable 1` = 1:2, two = 3:4)
tbbl

df$tw
tbbl$tw

df[, "two"]
tbbl[, "two"]

url <- "https://data.cityofchicago.org/api/views/5neh-572f/rows.csv?accessType=DOWNLOAD&bom=true&format=true"

all_stations <- 
  # Step 1: Read in data.
  read_csv(url) |> 
  # Step 2: filter columns and rename stationname.
  select(station = stationname, date, rides) |> 
  # Step 3: convert the character date field to a date encoding.
  # Also, put the data in units of 1k rides.
  mutate(date = mdy(date), rides = rides / 1000) |> 
  # Step 4: summarize the multiple records using the maximum.
  group_by(date, station) |> 
  summarise(rides = max(rides), .groups = "drop")

all_stations
