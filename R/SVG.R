#' Create an `svg` object
#'
#' The `SVG()` function is the entry point for building an SVG from the ground
#' up. We can provide predefined `height` and `width` attributes that define
#' the canvas size for the SVG. From here, we would want to use functions that
#' add elements to the SVG object (e.g., [svg_rect()], [svg_circle()], etc.) and
#' thus progressively build the graphic.
#'
#' @param width,height The dimensions of the SVG in `px` units.
#' @param viewbox An optional set of dimensions that defines the SVG `viewBox`
#'   attribute. The `viewBox` for an SVG element is the position and dimension,
#'   in user space, of an SVG viewport. If supplied, this could either be in the
#'   form of a four-element, numeric vector corresponding to the `"min-x"`,
#'   `"min-y"`, `"width"`, and `"height"` of the rectangle, or, as `TRUE` which
#'   uses the vector `c(0, 0, width, height)`.
#' @param title The `<title>` tag for the finalized SVG.
#' @param desc The `<desc>` tag for the finalized SVG.
#' @param incl_xmlns Should the `xmlns` attribute be included in the `<svg>`
#'   tag? This attribute is only required on the outermost `svg` element of SVG
#'   documents, and, it's unnecessary for inner `svg` elements or inside of HTML
#'   documents. By default, this is set to `FALSE`.
#' @param anim_iterations How many should an SVG animation (if defined by use of
#'   the [anims()] function) be played? By default this is `"infinite"` (i.e.,
#'   looped indefinitely) but we can specify the animation iteration count as
#'   a positive number.
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
                viewbox = NULL,
                title = NULL,
                desc = NULL,
                incl_xmlns = FALSE,
                anim_iterations = "infinite") {

  if (is.null(anim_iterations)) {
    anim_iterations <- "infinite"
  }

  if (all(!is.numeric(anim_iterations) & anim_iterations != "infinite")) {
    stop("The `anim_iterations` value must either be `\"infinite\"` or a positive number.",
         call. = FALSE)
  }

  if (is.numeric(anim_iterations)) {
    anim_iterations <- anim_iterations %>% round(digits = 0) %>% abs()
  }

  obj <-
    list(
      width = width,
      height = height,
      viewbox = viewbox,
      title = title,
      desc = desc,
      incl_xmlns = incl_xmlns,
      anim_iterations = anim_iterations,
      elements = list(),
      defs = list(),
      anims = list(),
      filters = list()
    )

  class(obj) <- "svg"
  obj
}
