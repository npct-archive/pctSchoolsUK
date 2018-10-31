# make national maps of schools scenarios
library(sf)
library(tmap)
library(tidyverse)
devtools::install_github("robinlovelace/ukboundaries")
lads = ukboundaries::lad2018
qtm(lads)
ukboundary = sf::st_union(lads)
qtm(ukboundary)


z = readRDS("private_data/z_all.Rds")
zc = rgeos::gCentroid(z, byid = T)
qtm(zc)
z_sf = st_as_sf(zc)
las_england = lads[z_sf, ]

# test plot - looks fine:
qtm(las_england) +
  tm_shape(ukboundary) +
  tm_borders()

# generate aggregated data
z = st_sf(st_geometry(z_sf), data = z@data)
names(z) = gsub(pattern = "data.", replacement = "", names(z))
summary(z$govtarget_slc)
vars_to_plot = c("all", "bicycle", "govtarget_slc", "dutch_slc")
lag = aggregate(z[vars_to_plot], las_england, FUN = sum)
# lag[2:4] = apply(lag[2:4], 2, function(x) x / lag$all)

lag_new = lag %>% 
  mutate_at(vars(contains("c")), function(x) x / lag$all * 100)

summary(lag)
plot(lag)

nrow(z)
names(z)
# tm_shape(z) +
#   tm_fill("govtarget_slc")

# good starter:
qtm(lag_new, vars_to_plot[2:4]) +
  tm_shape(ukboundary) +
  tm_borders()
  
# polished version
b = c(0, 2, 4, 6, 10, 15, 20, 25, 30, 40, 60)
m = tm_shape(lag_new) +
  tm_fill(col = vars_to_plot[2:4], title = "", breaks = b, palette = "RdYlBu", midpoint = 15) +
  tm_shape(ukboundary) +
  tm_borders() +
  tm_legend(title = "Cycle to school (%)") +
  tm_layout(panel.labels = c("Current", "Government Target", "Go Dutch"), legend.outside = T) 
  # tm_layout(legend.outside = T)
tmap_save(m, "paper/figures/pct-schools-scenarios-map.png")
browseURL("paper/figures/pct-schools-scenarios-map.png")
i = magick::image_read("paper/figures/pct-schools-scenarios-map.png")
it = magick::image_trim(i)
it
magick::image_write(it, "paper/figures/pct-schools-scenarios-map.png")
