#' Pics as bars
#'
#' As in \code{\link[ggplot2]{geom_bar}}, depending on what is maped to bar_pic
#' height two types of bar_pic charts are produced. Default is \code{stat = "count"},
#' which makes the height of the bar_pic proportional to the numbers of cases in
#' each group. \code{stat = "identity"} will map height to values in the data,
#' so you need to map an \code{y} aesthetic also.
#'
#' As opposed to \code{\link[ggplot2]{geom_bar}}, multiple x's occurring in the
#' same place will be "dodged" with \code{position = "dodge"}. If you want your
#' pics stacked, change to \code{position = "stack"}.
#'
#' @section Aesthetics:
#' \code{geom_bar_pic} understand the following aesthetics (required aesthetics
#' are in bold):
#' \itemize{
#'     \item{\bold{x}}
#'     \item{img}
#'     \item{alpha}
#'     \item{fill}
#'     \item{colour}
#'     \item{size}
#'     \item{linetype}
#' }
#'
#' @section Computed variables:
#' \describe{
#'   \item{count}{number of points in bin}
#'   \item{prop}{groupwise proportion}
#' }
#'
#' @seealso \code{\link[ggplot2]{geom_bar}} in the \code{ggplot2} package
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' @param width Pic width. By default, set to 90\% of the resolution of the data.
#' @param geom,stat Override the default connection between \code{geom_bar_pic}
#'   and \code{stat_count}.
#'
#' @export

# geom_function
geom_bar_pic <- function(mapping = NULL, data = NULL, stat = "count", position = "dodge",
                         width = NULL, na.rm = FALSE, show.legend = NA,
                         inherit.aes = TRUE, ...) {

  ggplot2::layer(
    geom = GeomBarPic, data = data, mapping = mapping, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(width = width, na.rm = na.rm, ...)
  )
}

#' GeomBarPic ggproto class for geom_bar_pic
#' @format NULL
#' @usage NULL
#' @export

# ggproto class
GeomBarPic <- ggplot2::ggproto(
  # class and inherit
  "GeomBarPic", ggplot2::Geom,

  #
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
    # print(coords$img[1])
    # print(parse(text = coords$img[1]))
    #
    raster <- eval(as.name(coords$img[1]))
    # print(class(raster))

    # fill
    fill_index <- !grepl('#00000000', raster, fixed = TRUE)
    raster[fill_index] <- coords$fill
    # fill_index <- !stringr::str_detect(raster, '#00000000')
    # raster[fill_index] <- coords$fill[1]

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
  draw_key = function(data, params, size) {
    draw_key_rect(data, params, size)
  },

  # we need to change the setup data to create ymin
  setup_data = function(data, params) {
    # like in GeomBar
    if (is.null(data$width)) {
      if (is.null(params$width)) {
        data$width <- (resolution(data$x, FALSE) * 0.9)
      } else {
        data$width <- params$width
      }
    }
    # data$width <- data$width %||% params$width %||% (resolution(data$x, FALSE) * 0.9)
    transform(data, ymin = pmin(y, 0), ymax = pmax(y, 0), xmin = x -
                width/2, xmax = x + width/2, width = NULL)
  }
)

# scale
manual_scale <- function(aesthetic, values, ...) {
  pal <- function(n) {
    if (n > length(values)) {
      stop("Insufficient values in manual scale. ", n, " needed but only ",
           length(values), " provided.", call. = FALSE)
    }
    values
  }
  ggplot2::discrete_scale(aesthetic, "manual", pal, ...)
}


scale_pic_manual <- function(..., values) {
  manual_scale("img", values, ...)
}