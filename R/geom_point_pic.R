#' Pics as points
#'
#' As in \code{\link[ggplot2]{geom_point}}, can be used for scatterplots, but
#' using raster objects instead of points.
#'
#' \code{geom_point_pic} differs from \code{\link[ggplot2]{geom_point}} in some
#' aesthetics. There is no need of \code{colour} aesthetic and the \code{shape}
#' aesthetic is replaced by the  \code{pic} aesthetic.
#'
#' @section Aesthetics:
#' \code{geom_bar_pic} understand the following aesthetics (required aesthetics
#' are in bold):
#' \itemize{
#'     \item{\bold{x}}
#'     \item{\bold{y}}
#'     \item{pic}
#'     \item{alpha}
#'     \item{fill}
#'     \item{size}
#'     \item{stroke}
#' }
#'
#' @seealso \code{\link[ggplot2]{geom_point}} in the \code{ggplot2} package
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' @param asis Logical indicating if the pic must be used as is, with no filling.
#'   If FALSE, the default, image not transparent pixels will be filled to the
#'   color provided in \code{fill} or black (default). If TRUE, image will
#'   maintain the original colors.
#'
#' @export

# geom_function
geom_point_pic <- function(mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", width = NULL, na.rm = FALSE,
                           show.legend = NA, asis = FALSE,
                           inherit.aes = TRUE, ...) {

  ggplot2::layer(
    geom = GeomPointPic, data = data, mapping = mapping, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, asis = asis, ...)
  )
}

#' GeomPointPic ggproto class for geom_bar_pic
#' @format NULL
#' @usage NULL
#' @export

GeomPointPic <-  ggplot2::ggproto(
  # class and inherit
  "GeomPointPic", ggplot2::Geom,

  # required aes
  required_aes  = c("x", "y"),

  # non missing aes
  non_missing_aes = c("fill", "pic", "size"),

  # default_aes
  default_aes = ggplot2::aes(
    colour = NA, shape = 19, fill = '#000000FF', size = 0.5,
    stroke = 0.5, alpha = NA, pic = NA
  ),

  # we change draw_group because we want to redraw the raster for each
  # group to be able to fill and change of pic
  draw_group = function(self, data, panel_scales, coord,
                        width = NULL, asis = FALSE) {
    # data transform
    coords <- coord$transform(data, panel_scales)

    # load and fill pic
    raster <- pic_load_and_fill(coords$pic[1], coords$fill[1],
                                coords$alpha[1], asis)

    # rasterGrob to generate the "shape"
    ggplot2:::ggname(
      "point_pic",
      grid::rasterGrob(
        raster,
        x = coords$x, y = coords$y,
        width = coords$size,
        height = coords$size,
        just = c("center", "center"),
        # gp = grid::gpar(
        #    col = coords$colour, fill = alpha(coords$fill, coords$alpha),
        #    lwd = coords$size * .pt, lty = coords$linetype, lineend = "butt",
        #    fontsize = 12
        # ),
        default.units = 'native'
      )
    )
  },

  # key
  draw_key = draw_key_pic

  # we need to change the setup data to create the parameters for rasterGrob
  # setup_data = function(data, params) {
  #   # like in GeomBar
  #   # data$width <- data$width %||% params$width %||% (resolution(data$x, FALSE) * 0.9)
  #   # I didn't found the %||% function but it seems to do the following:
  #   if (is.null(data$width)) {
  #     if (is.null(params$width)) {
  #       data$width <- (ggplot2::resolution(data$x, FALSE) * 0.9)
  #     } else {
  #       data$width <- params$width
  #     }
  #   }
  #   transform(data, ymin = pmin(y, 0), ymax = pmax(y, 0), xmin = x -
  #               width/2, xmax = x + width/2, width = NULL)
  # }
)