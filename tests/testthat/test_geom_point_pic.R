pine <- as.matrix(as.raster(rsvg::rsvg(
  system.file('pics', 'pine_tree_2.svg', package = 'ggpic')
)))
ash <- as.matrix(as.raster(rsvg::rsvg(
  system.file('pics', 'deciduous_tree_2.svg', package = 'ggpic')
)))
pine_raster <- as.raster(rsvg::rsvg(
  system.file('pics', 'pine_tree_2.svg', package = 'ggpic')
))
ash_raster <- as.raster(rsvg::rsvg(
  system.file('pics', 'deciduous_tree_2.svg', package = 'ggpic')
))
pine_df <- as.data.frame(pine)
ash_df <- as.data.frame(ash)
building <- as.matrix(as.raster(rsvg::rsvg(
  system.file('pics', 'building.svg', package = 'ggpic')
)))
leaf <- as.matrix(as.raster(rsvg::rsvg(
  system.file('pics', 'leaf.svg', package = 'ggpic')
)))
rainbow_building <- as.matrix(as.raster(rsvg::rsvg(
  system.file('pics', 'rainbow_building.svg', package = 'ggpic')
)))
rainbow_building_png <- as.matrix(as.raster(png::readPNG(
  system.file('pics', 'rainbow_building.png', package = 'ggpic')
)))
mouse <- as.matrix(as.raster(rsvg::rsvg(
  system.file('pics', 'mouse.svg', package = 'ggpic')
)))

library(ggplot2)
library(dplyr)

data <- data.frame(
  Classes = LETTERS[1:6],
  Site = rep(LETTERS[1:3], 2),
  Species = rep(c('Pine', 'Ash'), each = 3),
  Country = rep(c('SPA', 'POR', 'ITA'), 2),
  Area = rep(c('Coast', 'Inside'), each = 3),
  Height = rpois(6, 15),
  Deep = rpois(6, 2.2),
  stringsAsFactors = FALSE
)

ggplot(data,
       aes(x = Height, y = Deep)) +
  geom_point_pic(aes(pic = Species, fill = Species)) +
  scale_pic_manual(values = c('ash', 'pine'), size = 0.05) +
  ggthemes::theme_solarized(light = FALSE)
