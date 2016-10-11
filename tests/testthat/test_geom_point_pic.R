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
  x = rnorm(10),
  y = rnorm(10, 4),
  f = rep(c('Q. robur', 'P. pinaster'), 10)
)

ggplot(data,
       aes(x = x, y = y, fill = f)) +
  geom_point_pic(aes(pic = f), size = 0.05, alpha = .5) +
  # geom_point(colour = 'yellow', size = 5) +
  scale_pic_manual(values = c('pine', 'ash')) +
  # stat_smooth(aes(colour = f), method = "glm") +
  ggthemes::theme_solarized(light = FALSE) +
  theme(legend.position = 'bottom')
