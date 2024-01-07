# https://search.dataone.org/view/https%3A%2F%2Fpasta.lternet.edu%2Fpackage%2Fmetadata%2Feml%2Fknb-lter-hbr%2F208%2F9#https%3A%2F%2Fpasta.lternet.edu%2Fpackage%2Fdata%2Feml%2Fknb-lter-hbr%2F208%2F9%2F3b3cf7ea447cb875d7c7d68ebdfd24c7

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                    setup                                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#..........................load packages.........................
library(metajam) # https://nceas.github.io/metajam/
library(tidyverse)

#...................download data from DataOne...................
download_d1_data("https://cn.dataone.org/cn/v2/resolve/https%3A%2F%2Fpasta.lternet.edu%2Fpackage%2Fdata%2Feml%2Fknb-lter-hbr%2F208%2F9%2F3b3cf7ea447cb875d7c7d68ebdfd24c7",
                 path = here::here("stream-chem")) # rename downloaded folder to 'data/' so that it's ignored!

#....................read in downloaded files....................
stream_chem_pkg <- read_d1_files(here::here("stream-chem", "data"))

#........................get the data file.......................
stream_chem_data <- stream_chem_pkg$data

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                wrangle data                              ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

stream_2021 <- stream_chem_data |> 
  mutate(site = as.factor(site)) |> # 12 unique sites
  filter(waterYr == 2021) 

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                  plot data                               ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Cl carries a negative charge (https://archive.epa.gov/water/archive/web/html/vms59.html#:~:text=Conductivity%20in%20water%20is%20affected,that%20carry%20a%20positive%20charge).)
stream_chem_data |> 
  filter(waterYr == 2021) |> 
  ggplot(aes(x = Ca, y = spCond, color = site)) + # also check out NO3 vs PO4, 
  geom_point(alpha = 0.7) 

# sulfates contribute to acidification of surface water & soil (https://ww2.arb.ca.gov/resources/sulfate-and-health#:~:text=In%20addition%2C%20sulfates%20contribute%20to,cooling%20influence%20on%20climate%20change.)
# higher dissolved metal conc (e.g. Al) with lower pH (more acidic) (https://www.epa.gov/caddis-vol2/ph#:~:text=Acid%20rain%20mobilizes%20and%20leaches,in%20the%20free%20ionic%20form.)
stream_chem_data |> 
  filter(waterYr == 2021) |> 
  ggplot(aes(x = SO4, y = pH, color = site, size = Al_ICP)) + 
  geom_point(alpha = 0.7) 


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                      scatter plots + marginal density                    ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# scatter plot, no groups (just 2021 data)
p1 <- stream_chem_data |> 
  filter(waterYr == 2021) |> 
  ggplot(aes(x = SO4, y = pH)) + 
  geom_point(alpha = 0.7) # +
  # geom_density_2d(aes(color = ..level..)) +
  # scale_color_viridis_c() +
  # theme_minimal()

# scatter plot, no groups + marginal density plot (just 2021 data)
# https://cran.r-project.org/web/packages/ggExtra/vignettes/ggExtra.html
p1 + geom_rug()
ggExtra::ggMarginal(p1, type = "histogram")
ggExtra::ggMarginal(p1, type = "density")

# scatter plot with 12 groups (site) (just 2021 data)
p2 <- stream_chem_data |> 
  filter(waterYr == 2021) |> 
  ggplot(aes(x = SO4, y = pH, color = site)) + 
  geom_point(alpha = 0.7) +
  theme(legend.position = "bottom")

p2.2 <- stream_chem_data |> 
  filter(waterYr == 2021) |> 
  ggplot(aes(x = Al_ICP, y = pH, color = site)) + 
  geom_point(alpha = 0.7) +
  theme(legend.position = "bottom")

# scatter plot with 12 gruops (site) + marginal density plot (just 2021 data)
ggExtra::ggMarginal(p2, type = "density", groupFill = TRUE)

ggExtra::ggMarginal(p2.2, type = "density", groupFill = TRUE)

# with fewer groups 
p3 <- stream_chem_data |> 
  filter(waterYr == 2021) |> 
  filter(site %in% c("W1", "W5", "W9")) |> 
  ggplot(stream_clean, mapping = aes(x = SO4, y = pH, color = site)) + 
  geom_point(alpha = 0.7) +
  theme(legend.position = "bottom")

ggExtra::ggMarginal(p3, type = "density", groupFill = TRUE)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                          adding a third variable                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

stream_chem_data |> 
  filter(waterYr == 2021) |> 
  ggplot(aes(x = SO4, y = pH, color = site, size = Al_ICP)) + 
  geom_point(alpha = 0.7) +
  labs(size = "[Al] (mg/L)",
       color = "Site", 
       x = "SO4 (mg/L)")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                        when overplotting is an issue                     ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# example of overplotted data
ggplot(stream_chem_data, aes(x = SO4, y = pH)) + 
  geom_point()

# try transparency
ggplot(stream_chem_data, aes(x = SO4, y = pH)) + 
  geom_point(alpha = 0.3)

# try smaller points + transparency
ggplot(stream_chem_data, aes(x = SO4, y = pH)) + 
  geom_point(size = 0.5, alpha = 0.3) +
  geom_rug()

# try coloring by group + rug
ggplot(stream_chem_data, aes(x = SO4, y = pH, color = site)) + 
  geom_point(size = 0.5, alpha = 0.3) +
  geom_rug(alpha = 0.5)

# 2d histogram or hexbin (all data)
ggplot(stream_chem_data, aes(x = SO4, y = pH)) +
  geom_bin2d(bins = 20) +
  scale_fill_viridis_c()

ggplot(stream_chem_data, aes(x = SO4, y = pH)) +
  geom_hex() +
  scale_fill_viridis_c()

# adjust bins
ggplot(stream_chem_data, aes(x = SO4, y = pH)) +
  geom_hex(bins = 20) +
  scale_fill_viridis_c()

# adjust legend
ggplot(stream_chem_data, aes(x = SO4, y = pH)) +
  geom_hex() +
  scale_fill_viridis_c() + 
  guides(fill = guide_colourbar(title = "Count", 
                                barwidth = 0.7, barheight = 15))


# contour lines (hard to interpret)
ggplot(stream_chem_data, aes(x = SO4, y = pH)) +
  geom_density_2d()

# alternative
ggplot(stream_chem_data, aes(x = SO4, y = pH)) +
  ggdensity::geom_hdr_lines()

# hard to interpret
ggplot(stream_chem_data, aes(x = SO4, y = pH)) +
  geom_density_2d_filled()

# more interpretable visualizations of density estimates based on highest density regions
ggplot(stream_chem_data, aes(x = SO4, y = pH)) +
  ggdensity::geom_hdr()





#-----------------

# BASIC SCATTERPLOT (no groups) ----------
basic_scatter <- stream_chem_data |> 
  filter(waterYr == 2021) |> 
  ggplot(aes(x = DOC, y = pH)) + 
  geom_point(alpha = 0.7) #+
  #geom_rug()

ggExtra::ggMarginal(basic_scatter, type = "histogram")
ggExtra::ggMarginal(basic_scatter, type = "density")


# colored by groups (site)
scatterplot_site_groups <- stream_chem_data |> 
  filter(waterYr == 2021) |> 
  ggplot(aes(x = DOC, y = pH, color = site)) + 
  geom_point(alpha = 0.7) +
  # geom_rug()
  theme(legend.position = "bottom")

ggExtra::ggMarginal(p1.2, type = "density", groupFill = TRUE) # too many groups for this

# with fewer groups
fewer_groups <- stream_chem_data |> 
  filter(waterYr == 2021) |> 
  filter(site %in% c("W1", "W5", "W9")) |> 
  ggplot(stream_clean, mapping = aes(x = DOC, y = pH, color = site)) + 
  geom_point(alpha = 0.7) +
  theme(legend.position = "bottom")

ggExtra::ggMarginal(fewer_groups, type = "density", groupFill = TRUE)

# adding a third variable ----------------
stream_chem_data |> 
  filter(waterYr == 2021) |> 
  ggplot(aes(x = DOC, y = pH, color = site, size = Al_ICP)) + 
  geom_point(alpha = 0.7) +
  labs(size = "[Al] (mg/L)",
       color = "Site", 
       x = "DOC (mg/L)")



# OVER PLOTTING -----
# example of overplotted data
ggplot(stream_chem_data, aes(x = DOC, y = pH)) + 
  geom_point()

# try transparency
ggplot(stream_chem_data, aes(x = DOC, y = pH)) + 
  geom_point(alpha = 0.3)

# try smaller points + transparency
ggplot(stream_chem_data, aes(x = DOC, y = pH)) + 
  geom_point(size = 0.5, alpha = 0.3) +
  geom_rug()

# try coloring by group + rug
ggplot(stream_chem_data, aes(x = DOC, y = pH, color = site)) + 
  geom_point(size = 0.5, alpha = 0.3) +
  geom_rug(alpha = 0.5)

# 2d histogram or hexbin (all data)
ggplot(stream_chem_data, aes(x = DOC, y = pH)) +
  geom_bin2d(bins = 20) +
  scale_fill_viridis_c()

ggplot(stream_chem_data, aes(x = DOC, y = pH)) +
  geom_hex() +
  scale_fill_viridis_c()

# adjust bins
ggplot(stream_chem_data, aes(x = SO4, y = pH)) +
  geom_hex(bins = 20) +
  scale_fill_viridis_c()

# adjust legend
ggplot(stream_chem_data, aes(x = SO4, y = pH)) +
  geom_hex() +
  scale_fill_viridis_c() + 
  guides(fill = guide_colourbar(title = "Count", 
                                barwidth = 0.7, barheight = 15))


# contour lines (hard to interpret)
ggplot(stream_chem_data, aes(x = SO4, y = pH)) +
  geom_density_2d()

# alternative
ggplot(stream_chem_data, aes(x = SO4, y = pH)) +
  ggdensity::geom_hdr_lines()

# hard to interpret
ggplot(stream_chem_data, aes(x = SO4, y = pH)) +
  geom_density_2d_filled()

# more interpretable visualizations of density estimates based on highest density regions
ggplot(stream_chem_data, aes(x = SO4, y = pH)) +
  ggdensity::geom_hdr()
