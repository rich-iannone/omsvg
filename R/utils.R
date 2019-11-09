#' The `NULL` coalescing operator
#'
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

normalize_to_linear_easing <- function(easing_fn) {

  if (is.null(easing_fn)) {
    easing_fn <- linear()
  } else {
    if (easing_fn == "linear") {
      easing_fn <- linear()
    }
  }

  easing_fn
}
