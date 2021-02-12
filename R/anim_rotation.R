#' Animate an element through rotation
#'
#' Within an [anims()] call, itself passed to any `anims` argument, the
#' `anim_rotation()` function can be used to express an animation where the
#' target element undergoes a rotation change with time.
#'
#' @inheritParams anim_position
#' @param rotation The rotation value of the element at the keyframe time (given
#'   as the LHS value in the [anims()] call).
#' @param anchor The location of the element anchor about which rotation will
#'   occur. By default, this is the keyword `"center"`.
#' @param initial Should this rotation value be the initial rotation state of
#'   the element? If so, use `TRUE` and any value provided to `rotation` will be
#'   disregarded.
#'
#' @return An `anim_opacity` object, which is to be used as part of an [anims()]
#'   call.
#'
#' @examples
#' if (interactive()) {
#'
#' # This is a basic animation of an
#' # element's rotation state (moving to
#' # a new `rotation` value)
#' SVG(width = 300, height = 300) %>%
#'   svg_rect(
#'     x = 50, y = 50,
#'     width = 50, height = 50,
#'     attrs = svg_attrs_pres(
#'       stroke = "magenta",
#'       fill = "lightblue"
#'     ),
#'     anims = anims(
#'       2.0 ~ anim_rotation(rotation = 180)
#'     )
#'   )
#' }
#'
#' @export
anim_rotation <- function(rotation = NULL,
                          anchor = "center",
                          easing_fn = NULL,
                          initial = FALSE) {

  if (initial == FALSE & is.null(rotation)) {
    stop("The `rotation` value must be provided if `initial = FALSE`.",
         call. = FALSE)
  }

  if (isTRUE(initial)) {
    rotation <- 0
  }

  # Handle cases where `easing_fn` is NULL or `"linear"`
  easing_fn <- normalize_to_linear_easing(easing_fn = easing_fn)

  anim_property <-
    list(
      rotation = rotation,
      anchor = "center",
      easing_fn = easing_fn,
      initial = initial
    )

  class(anim_property) <- "anim_rotation"
  anim_property
}
