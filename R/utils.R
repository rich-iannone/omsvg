#' The `NULL` coalescing operator
#'
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
