# ggproto class
GeomBarPic <- ggplot2::ggproto(
  # class and inherit
  "GeomBarPic", Geom,

  # required aes, like a bar plot
  required_aes  = "x",

  # default_aes
  default_aes = ggplot2::aes(
    colour = NA, fill = 'grey35', size = 0.5,
    linetype = 1, alpha = NA, img = NA
  ),

  # we change draw_group because we want to redraw the raster for each
  # group to be able to fill and change of img
  draw_group = function(data, panel_scales, coord, width = NULL) {
    # data transform
    coords <- coord$transform(data, panel_scales)

    # print(coords$img)
    print(coords)
    print(coords$img[1])
    print(parse(text = coords$img[1]))
    # raster <- eval(parse(text = coords$img[1]))
    raster <- eval(as.name(coords$img[1]))
    print(class(raster))

    # fill
    fill_index <- !stringr::str_detect(raster, '#00000000')
    raster[fill_index] <- coords$fill[1]

    # rasterGrob to generate the "shape"
    grid::rasterGrob(
      raster,
      x = coords$xmin, y = coords$ymax,
      width = coords$xmax - coords$xmin,
      height = coords$ymax - coords$ymin,
      just = c("left", "top"),
      # gp = grid::gpar(
      #    col = coords$colour, fill = alpha(coords$fill, coords$alpha),
      #    lwd = coords$size * .pt, lty = coords$linetype, lineend = "butt",
      #    fontsize = 12
      # ),
      default.units = 'native'

    )
  },

  # the key also has to be modified to mimic that of BarGeom as the default in Geom
  # is for points and expects fontsize
  draw_key = ggplot2::draw_key_rect,

  # we need to change the setup data to create ymin
  setup_data = function(data, params) {
    # like in GeomBar
    if (is.null(data$width)) {
      if (is.null(params$width)) {
        data$width <- (ggplot2::resolution(data$x, FALSE) * 0.9)
      } else {
        data$width <- params$width
      }
    }

    # data$width <- data$width %||% params$width %||% (resolution(data$x, FALSE) * 0.9)
    transform(data, ymin = pmin(y, 0), ymax = pmax(y, 0), xmin = x -
                width/2, xmax = x + width/2, width = NULL)
  }
)

# geom_function
geom_bar_pic <- function(mapping = NULL, data = NULL, stat = "count", position = "dodge",
                         width = NULL, binwidth = NULL, na.rm = FALSE, show.legend = NA,
                         inherit.aes = TRUE, ...) {

  ggplot2::layer(
    geom = GeomBarPic, data = data, mapping = mapping, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(width = width, na.rm = na.rm, ...)
  )
}