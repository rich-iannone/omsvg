#' Paste a string either onto the left or the right of another string
#'
#' @param x A character vector of length equal to that of `x_side`.
#' @param x_side Another character vector, with a length equal to that of `x`.
#'   It will be pasted either to the left or to the right of `x` depending on
#'   the `direction`.
#' @param direction The side that `x_side` will be relative to `x`. This can
#'   be `left` or `right`.
#' @noRd
paste_on_side <- function(x,
                          x_side,
                          direction) {

  # Stop function if `direction` is not valid
  if (!(direction %in% c("left", "right"))) {
    stop("Internal error in `gt:::paste_on_side()`:\n",
         "* The `direction` must be either `left` or `right`.",
         call. = FALSE)
  }

  # Stop function if `x` and `x_side` are not both of class character
  if (any(!inherits(x, "character"), !inherits(x_side, "character"))) {
    stop("Internal error in `gt:::paste_on_side()`:\n",
         "* The `x` and `x_side` objects must be of class character.",
         call. = FALSE)
  }

  len <- length(x_side)

  # Stop function if the length of `x_side` is not 1 of the length of `x`
  if (!any(len == 1, len == length(x))) {
    stop("The length of the `x_side` vector must be 1 or the length of `x`.",
         call. = FALSE)
  }

  if (direction == "left") {

    return(paste0(x_side, x))

  } else if (direction == "right") {

    return(paste0(x, x_side))
  }
}

#' Paste a string onto the left side of another string
#'
#' @inheritParams paste_on_side
#' @param x_left Another character vector of length 1 that is to be pasted to
#'   the left of `x`.
#' @noRd
paste_left <- function(x, x_left) {
  paste_on_side(x, x_side = x_left, direction = "left")
}

#' Paste a string onto the right side of another string
#'
#' @inheritParams paste_on_side
#' @param x_right Another character vector of length 1 that is to be pasted to
#'   the right of `x`.
#' @noRd
paste_right <- function(x, x_right) {
  paste_on_side(x, x_side = x_right, direction = "right")
}

#' Wrapper for `gsub()` where `x` is the first argument
#'
#' This function is wrapper for `gsub()` that uses default argument values and
#' rearranges first three arguments for better piping
#' @param x,pattern,replacement,fixed Select arguments from the `gsub()`
#'   function.
#' @noRd
tidy_gsub <- function(x, pattern, replacement, fixed = FALSE) {

  gsub(pattern, replacement, x, fixed = fixed)
}

tidy_sub <- function(x, pattern, replacement, fixed = FALSE) {

  sub(pattern, replacement, x, fixed = fixed)
}

tidy_grepl <- function(x, pattern) {

  vapply(
    pattern,
    FUN = function(pattern) {
      grepl(pattern = pattern, x = x)
    },
    FUN.VALUE = logical(1),
    USE.NAMES = FALSE
  )
}
