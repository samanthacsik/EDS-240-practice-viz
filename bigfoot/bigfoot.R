# https://github.com/curatedmess/TidyTuesday/blob/main/2022/09132022/bigfoot.R
# https://twitter.com/ryanahart/status/1569774454066012165/photo/1

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                    setup                                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#..........................load packages.........................
library(waffle)
library(tidyverse)
library(showtext)
library(ggtext)

#..........................import data...........................
tuesdata <- tidytuesdayR::tt_load('2022-09-13')
bigfoot <- tuesdata$bigfoot

#..........................import fonts..........................
# font_add_google(name = "Peralta", family = "peralta")
font_add_google(name = "Ultra", family = "ultra")
font_add_google(name = "Josefin Sans", family = "josefin")

#....................import Font Awesome fonts...................
# font_add(family = "fa-brands",
#          regular = here::here("fonts", "Font Awesome 6 Brands-Regular-400.otf"))
# font_add(family = "fa-regular",
#          regular = here::here("fonts", "Font Awesome 6 Free-Regular-400.otf")) 
# font_add(family = "fa-solid",
#          regular = here::here("fonts", "Font Awesome 6 Free-Solid-900.otf"))

#................enable {showtext} for rendering.................
showtext_auto()

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                wrangle data                              ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ca_season_counts <- bigfoot |>
  filter(state == "California") |> 
  group_by(season) |> 
  count(season) |> 
  filter(season != "Unknown") |> 
  arrange(match(season, c("Spring", "Summer", "Fall", "Winter"))) |> 
  ungroup() |> 
  mutate(season = fct_relevel(season, "Spring", "Summer", "Fall", "Winter")) |> 
  mutate(perc_sightings = (n/sum(n))*100)

ca_county_counts <- bigfoot |> 
  filter(state == "California") |> 
  group_by(county) |> 
  count(county) |> 
  rename(sightings = n)

ca_indiv_sightings <- bigfoot |> 
  filter(state == "California") 

# ca_county_coords <- bigfoot |> 
#   select(county, longitude, latitude)
# 
# ca_county_counts <- full_join(ca_county_counts, ca_county_coords)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            prep viz components                           ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#........................create palettes.........................
season_palette <- c("Spring" = "#357266", 
                    "Summer" = "#FFB813", 
                    "Fall" = "#983A06", 
                    "Winter" = "#005F71")

highlight_summer_palette <- c("Spring" = alpha("#357266", 1/3), 
                              "Summer" = "#FFB813", 
                              "Fall" = alpha("#983A06", 1/3), 
                              "Winter" = alpha("#005F71", 1/3))

#.......................create plot labels.......................
title <- "Summer is the season of Bigfoot sightings in CA"
subtitle <- "Winter, on the other hand, is a rare time to spot Sasquatch"
caption <- "Source: Bigfoot Field Researchers Organization"

# create theme ----
bigfood_theme <- function(){
  theme_void() +
    theme(
      plot.title = element_text(family = "ultra", 
                                size = 18, 
                                margin = margin(1, 0, 0.3, 1, "lines")),
      plot.subtitle = element_text(family = "josefin",
                                             size = 16,
                                             margin = margin(0, 0, 0.75, 0, "lines")),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(family = "josefin",
                                 size = 12),
      plot.caption = element_text(family = "josefin",
                                  size = 10,
                                  color = theme_colors["gray_text"], 
                                  margin = margin(2, 2, 3, 0, "lines")),
      plot.background = element_rect(fill = theme_colors["panel_beige"], 
                                     color = theme_colors["panel_beige"]),
      plot.margin = margin(1, 1, 1, 1, "cm")
    )
}


# base waffle ----
waffle1 <- ggplot(ca_season_counts, aes(fill = season, values = n)) +
  geom_waffle(color = "white", size = 0.3, 
              n_rows = 10, make_proportional = FALSE) +
  coord_equal() +
  labs(title = title,
       subtitle = subtitle,
       caption = caption) +
  waffle_theme()

waffle1

# update palette ----
waffle1 + 
  scale_fill_manual(values = season_palette)

# adjusted alpha ----
waffle1 + 
  scale_fill_manual(values = highlight_summer_palette)

# proportional waffle ----
waffle2 <- ggplot(ca_season_counts, aes(fill = season, values = n)) +
  geom_waffle(color = "white", size = 0.3, 
              n_rows = 10, make_proportional = TRUE) +
  coord_equal(clip = "off") +
  scale_fill_manual(values = season_palette) +
  labs(title = title,
       subtitle = subtitle,
       caption = caption) +
  waffle_theme() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.margin = margin(1, 6, 1, 6, "cm")
  ) +
  annotate(
    geom = "text",
    x = 14,
    y = 7,
    label = "Only 10% of sightings\nhave occurred in the winter",
    family = "josefin"
  ) +
  annotate(
    geom = "curve",
    x = 13.5, xend = 11,
    y = 6.2, yend = 5,
    curvature = -0.5,
    arrow = arrow(length = unit(0.3, "cm"))
  )

waffle2 

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                    map                                   ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#..........................load packages.........................
library(tidyverse)
library(tigris)

#.........................get shape data.........................
county_geo <- tigris::counties(class = "sf", cb = TRUE) |> 
  shift_geometry()

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                wrangle data                              ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ca_counties <- county_geo |> 
  janitor::clean_names() |> 
  filter(state_name == "California") |> 
  rename(county = namelsad)

ca_sightings <- full_join(ca_counties, ca_county_counts) #|> 
  #mutate(sightings = replace_na(sightings, 0))

ca_indiv_sightings <- full_join(ca_counties, ca_indiv_sightings)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                  plot map                                ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ggplot(ca_sightings) +
  geom_sf(aes(fill = sightings), linewidth = 0.2) +
  scale_fill_gradient(low = "#E9D9FF", high = "#2E2836") +
  guides(fill = guide_colorbar(barwidth = 15, barheight = 0.5, 
                                title.position = "top", title.hjust = 0.5,
                                ticks = TRUE)) +
  theme_void() +
  theme(
    legend.position = "top"
  )

#230C0F
#2E2836

ca_indiv_sightings |> 
  ggplot() +
  geom_sf(linewidth = 0.2, color = "white", fill = "gray") +
  geom_point(aes(x = longitude, y = latitude, color = season))
  
