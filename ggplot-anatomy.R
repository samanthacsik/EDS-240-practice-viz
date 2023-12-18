library(palmerpenguins)
library(tidyverse)


ggplot(data = penguins,
       aes(x = bill_length_mm,
           y = bill_depth_mm,
           group = species)) +
  geom_point(aes(color = species, 
                 shape = species),
             size = 3,
             alpha = 0.8) +
  scale_color_manual(values = c("darkorange","purple","cyan4")) +
  labs(title = "Penguin bill dimensions",
       subtitle = "Bill length and depth for Adelie, Chinstrap and Gentoo Penguins at Palmer Station LTER",
       x = "Bill length (mm)",
       y = "Bill depth (mm)",
       color = "Penguin species",
       shape = "Penguin species",
       caption = "Data from the {palmerpenguins} package") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 15, margin = margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 10, b = 0, l = 0)),
    axis.text = element_text(size = 13),
    #axis.line = element_line(linewidth = 0.25, color = "gray20"),
    plot.title.position = "plot",
    plot.title = element_text(size = 22),
    plot.subtitle = element_text(size = 17, margin = margin(t = 0, r = 0, b = 15, l = 0)),
    plot.caption = element_text(hjust = 1, face = "italic", size = 12),
    plot.background = element_rect(color = "gray20"),
    panel.background = element_rect(color = 'gray20'),
    plot.margin = margin(t = 25, r = 25, b = 25, l = 25),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 13),
    legend.background = element_rect(size = 0.25, color = "gray20"),
    legend.position = c(0.85, 0.15)
    )
  

ggsave("ggplot_anatomy.png", plot = last_plot(), width = 10, height = 8)
