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
#'     \item{pic}
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
#' @param stat Override the default connection between \code{geom_bar_pic}
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

#' Draw key function for geom_bar_pic
#'
#' To be able to generate a legend with the raster image, ggpic needs a custom
#' \code{draw_key} function.
#'
#' When pic and fill aesthetics does not map to the same variable, fill guide
#' is redirected to \code{draw_key_rect} and pic guide to a raster grob.
#'
#' @inheritParams ggplot2::draw_key
#'
#' @export

draw_key_pic <- function(data, params, size) {
  # If pic and fill does not map to same variable, fill guide must be a
  # rect guide and pic guide must be a raster guide:
  if (is.na(data$pic)) {
    ggplot2::draw_key_rect(data, params, size)
  } else {
    raster <- eval(as.name(data$pic))
    # For correct filling we need to convert to matrix if not already
    if (class(raster) != 'matrix') {
      raster <- as.matrix(raster)
    }
    # filling
    fill_index <- !grepl('^#[0-9a-fA-F]{6}00$', raster)
    raster[fill_index] <- data$fill[1]
    # grob
    grid::rasterGrob(
      raster,
      default.units = 'native'
    )
  }
}

#' GeomBarPic ggproto class for geom_bar_pic
#' @format NULL
#' @usage NULL
#' @export

# ggproto class
GeomBarPic <- ggplot2::ggproto(
  # class and inherit
  "GeomBarPic", ggplot2::Geom,

  # required aes
  required_aes  = "x",

  # non missing aes
  # non_missing_aes = c("fill", "pic"),

  # default_aes
  default_aes = ggplot2::aes(
    colour = NA, fill = '#000000FF', size = 0.5,
    linetype = 1, alpha = NA, pic = NA
  ),

  # we change draw_group because we want to redraw the raster for each
  # group to be able to fill and change of pic
  draw_group = function(self, data, panel_scales, coord, width = NULL) {
    # data transform
    coords <- coord$transform(data, panel_scales)

    # print(coords$pic)
    # print(coords)
    # print(coords$pic[1])
    # print(parse(text = coords$pic[1]))
    #
    raster <- eval(as.name(coords$pic[1]))
    # For correct filling we need to convert to matrix if not already
    if (class(raster) != 'matrix') {
      raster <- as.matrix(raster)
    }
    # print(class(raster))
    # fill
    fill_index <- !grepl('^#[0-9a-fA-F]{6}00$', raster)
    raster[fill_index] <- coords$fill[1]
    # fill_index <- !stringr::str_detect(raster, '#00000000')
    # raster[fill_index] <- coords$fill[1]

    # rasterGrob to generate the "shape"
    ggplot2:::ggname(
      "bar_pic",
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
    )
  },

  # the key also has to be modified to mimic that of BarGeom as the default in Geom
  # is for points and expects fontsize.
  draw_key = draw_key_pic,

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
  manual_scale("pic", values, ...)
}