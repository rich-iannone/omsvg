#' Animate the position of an element
#'
#' Within an [anims()] call, itself passed to any `anims` argument, the
#' `anim_position()` function can be used to express an animation where the
#' position of the target element changes with time.
#'
#' @param x,y The position of the element, expressed as `x` and `y`, at the
#'   keyframe time (given as the LHS value in the [anims()] call).
#' @param easing_fn The timing or easing function to use for the animation. If
#'   not provided, the [linear()] timing function will be used (which is doesn't
#'   use any easing in the animation, just a linear movement). The other timing
#'   and easing functions are: [step_start()], [step_end()], [ease_in()],
#'   [ease_out()], and [ease_in_out()].
#' @param initial Should this position be the initial position of the element?
#'   If so, use `TRUE` and any values provided to `x` and `y` will be
#'   disregarded.
#'
#' @return An `anim_opacity` object, which is to be used as part of an [anims()]
#'   call.
#'
#' @examples
#' if (interactive()) {
#'
#' # Basic animation of an element's
#' # position (moving to a new `x` and
#' # `y` position)
#' SVG(width = 300, height = 300) %>%
#'   svg_rect(
#'     x = 50, y = 50,
#'     width = 50, height = 50,
#'     attrs = svg_attrs_pres(
#'       stroke = "magenta",
#'       fill = "lightblue"
#'     ),
#'     anims = anims(
#'       2.0 ~ anim_position(x = 100, y = 50)
#'     )
#'   )
#' }
#'
#' @export
anim_position <- function(x = NULL,
                          y = NULL,
                          easing_fn = NULL,
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

  # Handle cases where `easing_fn` is NULL or `"linear"`
  easing_fn <- normalize_to_linear_easing(easing_fn = easing_fn)

  anim_property <-
    list(
      x = x,
      y = y,
      easing_fn = easing_fn,
      initial = initial
    )

  class(anim_property) <- "anim_position"
  anim_property
}
