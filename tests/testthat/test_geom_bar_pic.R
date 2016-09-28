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
       aes(x = Classes)) +
  geom_bar_pic(aes(y = Height), pic = 'building',
               stat = 'identity', width = 0.8) +
  ggthemes::theme_solarized(light = FALSE)

ggplot(data,
       aes(x = Classes)) +
  geom_bar_pic(aes(y = Height), pic = 'leaf', fill = 'limegreen',
               stat = 'identity', width = 0.5) +
  labs(y = 'Leaf length [cm]')

iris %>%
  group_by(Species) %>%
  summarise_all(funs(mean)) %>%
  ggplot(aes(x = Species, fill = Species)) +
  geom_bar_pic(aes(y = Petal.Length), pic = 'leaf',
               stat = 'identity', width = 1.3, alpha = 0.7) +
  labs(y = 'Petal length [cm]') +
  ggthemes::scale_fill_solarized() +
  ggthemes::theme_solarized(light = FALSE)

ggplot(data,
       aes(x = Site, pic = Species, fill = Site)) +
  geom_bar_pic(aes(y = Height), stat = 'identity') +
  scale_pic_manual(values = c('ash', 'pine'))

ggplot(data,
       aes(x = Country, fill = Area)) +
  geom_bar_pic(aes(y = Height), pic = 'building',
               stat = 'identity', width = 0.8) +
  ggthemes::scale_fill_solarized(accent = 'orange') +
  ggthemes::theme_solarized(light = FALSE)

ggplot(data,
       aes(x = Site, pic = Species, fill = Species)) +
  geom_bar_pic(aes(y = Height), stat = 'identity') +
  scale_pic_manual(values = c('ash', 'pine')) +
  ggthemes::scale_fill_solarized(accent = 'orange') +
  ggthemes::theme_solarized(light = FALSE)

ggplot(data,
       aes(x = Site, pic = Species, fill = Species)) +
  geom_bar_pic(aes(y = Height), stat = 'identity') +
  scale_pic_manual(values = c('ash_raster', 'pine_raster')) +
  ggthemes::scale_fill_solarized(accent = 'orange') +
  ggthemes::theme_solarized(light = FALSE)

ggplot(data,
       aes(x = Site, pic = Species, fill = Species)) +
  geom_bar_pic(aes(y = Height), stat = 'identity') +
  scale_pic_manual(values = c('ash_df', 'pine_df')) +
  ggthemes::scale_fill_solarized(accent = 'orange') +
  ggthemes::theme_solarized(light = FALSE)

ggplot(data,
       aes(x = Site, pic = Species)) +
  geom_bar_pic(aes(y = Height), fill = 'darkgreen', stat = 'identity') +
  scale_pic_manual(values = c('ash', 'pine'))

ggplot(data,
       aes(x = Site)) +
  geom_bar_pic(aes(pic = Species), stat = 'count') +
  scale_pic_manual(values = c('ash', 'pine'))

