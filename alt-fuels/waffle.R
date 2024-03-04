library(waffle)
library(tidyverse)
library(hrbrthemes)

data <- data.frame(
  month = factor(rep(month.abb[1:3], 3), levels=month.abb[1:3]),
  vals = c(10, 20, 30, 6, 14, 40, 30, 20, 10),
  col = rep(c("navy", "black", "maroon"), 3),
  fct = c(
    rep("Thing 1", 3),
    rep("Thing 2", 3),
    rep("Thing 3", 3)
  )
)

data2 <- data %>%
  count(month, wt = vals)

# normal waffle chart ------------------------
ggplot(data2, aes(fill = month, values = n)) +
  geom_waffle(
    n_rows = 20,
    size = 0.33,
    colour = "white",
    flip = TRUE
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("#a40000", "#c68958", "#ae6056"),
    labels = c("Fruit", "Sammich", "Pizza")
  ) +
  coord_equal() +
  theme_ipsum_rc(grid = "") +
  theme_enhance_waffle()

tuesdata <- tidytuesdayR::tt_load('2022-09-13')
bigfoot <- tuesdata$bigfoot

# ca <- bigfoot |> 
#   filter(state == "California") |> 
#   filter(county == "Santa Barbara County")

bigfoot |> 
  group_by(season) |> 
  count(season)



