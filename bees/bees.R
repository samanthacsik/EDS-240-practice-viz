# https://www.kaggle.com/datasets/m000sey/save-the-honey-bees/
# https://www.ars.usda.gov/oc/br/ccd/index/

library(tidyverse)

bees <- read_csv(here::here("bees", "save_the_bees.csv"))

ca_bees <- bees |> 
  filter(state == "California") |> 
  group_by(year, quarter) |>
  ggplot(aes(x = vector, y = vector)) +
  # mutate(total_num_colonies = sum(num_colonies),
  #        total_max_colonies = sum(max_colonies),
  #        total_lost_colonies = sum(lost_colonies),
  #        total_)
  # mutate(year_string = as.character(year),
  #        quarter_string = as.character(quarter)) |> 
  # unite(col = "date_string", year_string, quarter_string, sep = "-")

# barplot (regular, flipped, circular)

# lollipop plot

# dumbbell plot: change in populations between two dates?

# parallel plot

# radar plot