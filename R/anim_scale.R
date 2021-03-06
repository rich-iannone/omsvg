#' Animate an element through scaling
#'
#' Within an [anims()] call, itself passed to any `anims` argument, the
#' `anim_scale()` function can be used to express an animation where the target
#' element undergoes a scaling change with time.
#'
#' @inheritParams anim_position
#' @param scale The scale value of the element at the keyframe time (given as
#'   the LHS value in the [anims()] call). If providing a single scaling value,
#'   the scaling will operate in the x and y directions (relative to the center
#'   of the element). If two values are provided, these will be taken as scaling
#'   values in the x and y directions.
#'
#' @return An `anim_opacity` object, which is to be used as part of an [anims()]
#'   call.
#'
#' @examples
#' if (interactive()) {
#'
#' # Basic animation of an element's
#' # scaling state (moving to a new
#' # `scale` value)
#' SVG(width = 300, height = 300) %>%
#'   svg_rect(
#'     x = 50, y = 50,
#'     width = 50, height = 50,
#'     attrs = svg_attrs_pres(
#'       stroke = "magenta",
#'       fill = "lightblue"
#'     ),
#'     anims = anims(
#'       2.0 ~ anim_scale(scale = 2)
#'     )
#'   )
#' }
#'
#' @export
anim_scale <- function(scale = NULL,
                       easing_fn = NULL) {

  if (is.null(scale)) {
    stop("A scale value must be provided to `scale`.",
         call. = FALSE)
  }

  # Verify that scale values are numeric
  if (!inherits(scale, "numeric")) {
    stop("Values provided to `scale` must be numeric",
         call. = FALSE)
  }

  # Verify that the vector of scale values has length 1 or 2
  if (length(scale) == 0 || length(scale) > 2) {
    stop("The length of `scale` must be either 1 or 2.",
         call. = FALSE)
  }

  # Verify that scale values are positive
  if ((scale < 0) %>% any()) {
    stop("Values for `scale` must be positive.",
         call. = FALSE)
  }

  # Create character vector with two scale values
  # (e.g., "1,1", or "2,1")
  if (length(scale) == 1) {
    scale <- rep(scale, 2)
  }
  scale <- paste(scale, collapse = ",")

  # Handle cases where `easing_fn` is NULL or `"linear"`
  easing_fn <- normalize_to_linear_easing(easing_fn = easing_fn)

  anim_property <-
    list(
      scale = scale,
      anchor = "center",
      easing_fn = easing_fn,
      initial = FALSE
    )

  class(anim_property) <- "anim_scale"
  anim_property
}
