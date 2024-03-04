# https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-03-01/readme.md
# https://github.com/doehm/tidyTuesday/tree/master/2022/week09-energy

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                    setup                                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#........................import libraries........................
library(tidyverse)
library(janitor)
library(showtext)
library(ggtext)

#............................load data...........................
stations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-01/stations.csv') |>
  clean_names()

#..........................import fonts..........................
font_add_google(name = "Anton", family = "anton")

#................enable {showtext} for rendering.................
showtext_auto()

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                wrangle data                              ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_base <- stations |>
  group_by(state) |>
  summarise(perc = sum(fuel_type_code == "ELEC")/n()) |>
  mutate(
    code = state,
    info_graph = glue::glue("<img src='alt-fuels/{state}.png' width = 35 height = 50>")
  )
