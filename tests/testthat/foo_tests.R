pine <- as.matrix(as.raster(rsvg::rsvg('img/pine_tree_2.svg')))
ash <- as.matrix(as.raster(rsvg::rsvg('img/deciduous_tree_2.svg')))
pine_raster <- as.raster(rsvg::rsvg('img/pine_tree_2.svg'))
ash_raster <- as.raster(rsvg::rsvg('img/deciduous_tree_2.svg'))
pine_df <- as.data.frame(pine)
ash_df <- as.data.frame(ash)
building <- as.matrix(as.raster(rsvg::rsvg('img/building.svg')))

library(ggplot2)

data <- data.frame(
  Site = rep(LETTERS[1:3], 2),
  Species = rep(c('Pine', 'Ash'), each = 3),
  Country = rep(c('SPA', 'POR', 'ITA'), 2),
  Area = rep(c('Coast', 'Inside'), each = 3),
  Height = rpois(6, 15),
  Deep = rpois(6, 2.2),
  stringsAsFactors = FALSE
)

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
  geom_bar_pic(aes(pic = Species), fill = 'darkgreen', stat = 'count') +
  scale_pic_manual(values = c('ash', 'pine'))

