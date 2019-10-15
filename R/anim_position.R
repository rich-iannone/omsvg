#' Animate the position of an element
#'
#' With an [anims()] call, itself passed as to any `anims` argument, the
#' `anim_position()` function can be used to express an animation where the
#' position of the target element changes with time.
#'
#' @param x,y The position of the element, expressed as `x` and `y`, at the
#'   keyframe time (given as the LHS value in the [anims()] call).
#' @param timing The timing function to use for the movement to the new
#'   position.
#' @param initial Should this position be the initial position of the element?
#'   If so, use `TRUE` and any values provided to `x` and `y` will be
#'   disregarded.
#'
#' @examples
#' # Basic animation of an element's
#' # position (moving to a new `x` and
#' # `y` position)
#' SVG(width = 300, height = 300) %>%
#'   svg_rect(
#'     x = 50, y = 50,
#'     width = 50, height = 50,
#'     attrs = attrs_pres(
#'       stroke = "magenta",
#'       fill = "lightblue"
#'     ),
#'     anims = anims(
#'       2.0 ~ anim_position(x = 100, y = 50)
#'     )
#'   )
#'
#' @export
anim_position <- function(x = NULL,
                          y = NULL,
                          timing = NULL,
                          initial = FALSE) {

  if (initial == FALSE & (is.null(x) & is.null(y))) {
    stop("The `x` and `y` values must be provided if `initial = FALSE`.",
         call. = FALSE)
  }

  if (isTRUE(initial)) {
    x <- 0
    y <- 0
  }

  if (!is.null(x)) {
    if (is.numeric(x)) {
      x <- paste0(x, "px")
    }
  }

  if (!is.null(y)) {
    if (is.numeric(y)) {
      y <- paste0(y, "px")
    }
  }

  if (is.null(timing)) {
    timing <- "linear()"
  } else {
    if (timing == "linear") {
      timing <- "linear()"
    }
  }

  anim_property <-
    list(
      x = x,
      y = y,
      timing = timing,
      initial = initial
    )

  class(anim_property) <- "anim_position"
  anim_property
}
