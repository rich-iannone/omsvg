#' Get an information table showing all Line Awesome icons
#'
#' This informative table shows which Line Awesome icons are available inside of
#' **omsvg**. The icons are composed of lines and they look awesome! There are
#' plenty to choose from also, nearly *1400* icons across *69* categories. Just
#' take note of the ones you like and get their names, you'll need them when
#' using the [SVG_la()] function.
#'
#' @return Invisibly returns `NULL`. The side effect of displaying a table of
#'   icons is the purpose of this function.
#'
#' @export
info_lineawesome <- function() {

  htmltools::html_print(htmltools::HTML(gt_tbl_raw))

  invisible(NULL)
}
