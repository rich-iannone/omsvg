anchor_keyword_to_xy <- function(x,
                                 elements,
                                 index) {

  x %>%
    vapply(
      FUN.VALUE = character(1),
      USE.NAMES = FALSE,
      FUN = function(x) {

        if (x == "center") {

          x <-
            (c(elements[[index]]$width / 2, elements[[index]]$width / 2) * -1) %>%
            as.character() %>%
            paste_right("px") %>%
            collapse_strings(",")
        }
        x
      })
}
