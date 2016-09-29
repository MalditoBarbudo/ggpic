# ggpic

`ggpic` extends `ggplot2` package adding geoms based on the the default ones, but
using raster objects instead of rect elements. This way you can make a barplot
with trees, buildings or leaves instead bars.

## Usage

```r
# libraries
library(ggplot2)
library(ggpic)
library(dplyr)

# pic
leaf <- as.matrix(as.raster(rsvg::rsvg(
  system.file('pics', 'leaf.svg', package = 'ggpic')
)))

# plot
iris %>%
  ggplot(aes(x = Species)) +
  geom_bar_pic(pic = 'leaf')
```

## Installation

`ggpic` is only at GitHub at the moment. You can install it using `devtools`
package:

```r
# install devtools if necessary
install.packages('devtools')

# install ggpic
devtools::install_github('MalditoBarbudo/ggpic', build_vignettes = TRUE)
```

## Components:

+ `geom_bar_pic` A function to emulate `geom_bar` with pics

+ `scale_pic_manual` A scale function for *pics*

+ `draw_key_pic` A guide function to be able to generate legends for the *pics*

## See more

For more examples and a complete description of the packge
see the *Introduction to ggpic* vignette:

```
vignette('Introduction to ggpic', package = 'ggpic')
```
