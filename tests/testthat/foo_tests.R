pine <- as.matrix(as.raster(rsvg::rsvg('img/pine_tree_2.svg')))
ash <- as.matrix(as.raster(rsvg::rsvg('img/deciduous_tree_2.svg')))

library(ggplot2)

data <- data.frame(
  Site = rep(LETTERS[1:3], 2),
  Species = rep(c('Pine', 'Ash'), each = 3),
  Height = rpois(6, 15),
  Deep = rpois(6, 2.2),
  stringsAsFactors = FALSE
)

ggplot(data,
       aes(x = Site, img = Species, fill = Site)) +
  geom_bar_pic(aes(y = Height), stat = 'identity') +
  scale_pic_manual(values = c('pine', 'ash'))

ggplot(data,
       aes(x = Site, img = Species, group = Species)) +
  geom_bar_pic(aes(y = Height), fill = 'darkgreen', stat = 'identity') +
  scale_pic_manual(values = c('pine', 'ash'))
