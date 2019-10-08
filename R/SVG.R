#' Create an `svg` object
#'
#' The `SVG()` function is the entry point for building an SVG from the ground
#' up. We can provide predefined `height` and `width` attributes that define
#' the canvas size for the SVG. From here, we would want to use functions that
#' add elements to the SVG object (e.g., [svg_rect()], [svg_circle()], etc.) and
#' thus progressively build the graphic.
#'
#' @param width,height The dimensions of the SVG in `px` units.
#' @param title The `<title>` tag for the finalized SVG.
#' @param desc The `<desc>` tag for the finalized SVG.
#'
#' @examples
#' # Create an SVG with nothing drawn
#' # within it
#' svg <- SVG(width = 200, height = 100)
#'
#' # Add a rectangle and then a circle
#' svg <-
#'   svg %>%
#'   svg_rect(
#'     x = 20, y = 20,
#'     width = 40, height = 40
#'   ) %>%
#'   svg_circle(
#'     x = 100, y = 40,
#'     diameter = 40
#'   )
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
