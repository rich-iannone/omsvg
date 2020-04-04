#' Safely print any `svg` object in R Markdown
#'
#' This will transform `svg` objects to `knit_print` objects
#'
#' @param x an `svg` object.
#'
#' @keywords internal
knit_print.svg <- function(x, ...) {

  # nocov start

  if (requireNamespace("knitr", quietly = TRUE)) {

    knitr::knit_print(htmltools::HTML(as.character(x)), ...)
  }

  # nocov end
}
