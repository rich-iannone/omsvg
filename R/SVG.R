#' Create an `svg` object
#'
#' The `SVG()` function is the entry point for building an SVG from the ground
#' up. We can provided predefined `height` and `width` attributes that define
#' the canvas size for the SVG. From here, we would want to use functions that
#' add elements to the SVG object (e.g., [svg_rect()], [svg_circle()], etc.) and
#' thus progressively build the graphic.
#'
#' @param width,height The dimensions of the SVG in `px` units.
#' @param title The `<title>` tag for the finalized SVG.
#' @param desc The `<desc>` tag for the finalized SVG.
#'
#' @export
SVG <- function(width,
                height,
                title = NULL,
                desc = NULL) {

  obj <-
    list(
      width = width,
      height = height,
      title = title,
      desc = desc,
      elements = list(),
      defs = list(),
      anims = list()
    )

  class(obj) <- "svg"
  obj
}
