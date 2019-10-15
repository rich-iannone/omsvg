#' Animate an element through an opacity change
#'
#' With an [anims()] call, itself passed as to any `anims` argument, the
#' `anim_opacity()` function can be used to express an animation where the
#' target element undergoes a change in opacity with time.
#'
#' @param opacity The opacity value of the element at the keyframe time (given
#'   as the LHS value in the [anims()] call).
#' @param timing The timing function to use for the movement to the new
#'   position.
#' @param initial Should this opacity value be the initial opacity value of
#'   the element? If so, use `TRUE` and any value provided to `opacity` will be
#'   disregarded.
#'
#' @examples
#' # Basic animation of an element's
#' # opacity value (moving to a new
#' # `opacity` value of `0`)
#' SVG(width = 300, height = 300) %>%
#'   svg_rect(
#'     x = 50, y = 50,
#'     width = 50, height = 50,
#'     attrs = attrs_pres(
#'       stroke = "magenta",
#'       fill = "lightblue"
#'     ),
#'     anims = anims(
#'       2.0 ~ anim_opacity(opacity = 0)
#'     )
#'   )
#'
#' @export
anim_opacity <- function(opacity = NULL,
                         timing = NULL,
                         initial = FALSE) {

  if (initial == FALSE & is.null(opacity)) {
    stop("The `opacity` value must be provided if `initial = FALSE`.",
         call. = FALSE)
  }

  # if (isTRUE(initial)) {
  #   opacity <- 1
  # }

  if (!inherits(opacity, "numeric")) {
    stop("The `opacity` value must be numeric.",
         call. = FALSE)
  }

  if (opacity < 0 || opacity > 1) {
    stop("The `opacity` value must be in the range of `0` to `1`.",
         call. = FALSE)
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
      opacity = opacity,
      timing = timing,
      initial = initial
    )

  class(anim_property) <- "anim_opacity"
  anim_property
}

process_anims_opacity <- function(elements,
                                  df_anims,
                                  index,
                                  max_time_s,
                                  anim_iterations) {

  anim_type_str <- "anim_opacity"

  if (!(anim_type_str %in% df_anims$anim_type)) {
    return(elements)
  }

  if (anim_iterations == "infinite") {
    rep_str <- "infinite" %>% paste_left(" ")
  } else {
    rep_str <- as.character(anim_iterations) %>% paste_left(" ")
  }

  # Obtain a subset of animation directives
  df_anims <- subset(df_anims, anim_type == anim_type_str)

  # Get the initial opacity value
  initial_opacity <- elements[[index]][["opacity"]]

  if (is.null(initial_opacity)) {
    initial_opacity <- 1.0
  }

  # Obtain an `anim_id` value to link together
  # element `<styles>` and `@keyframes`
  anim_id <- paste0(anim_type_str, "_", expand_index(index = index))

  df_anims$anim_id <- anim_id
  df_anims$total_time_s <- max_time_s
  df_anims$time_pct <-
    ((df_anims$time_s / df_anims$total_time_s) %>% round(digits = 4)) * 100

  # Fill in `0s` and `ns` animation states and set
  # initial values if necessary
  df_anims <-
    df_anims %>%
    add_0s_state(attr_names = "opacity") %>%
    add_ns_state(attr_names = "opacity", max_time_s = max_time_s) %>%
    dplyr::mutate(opacity = dplyr::case_when(
      initial ~ initial_opacity,
      TRUE ~ opacity
    ))

  # Get `@keyframes` string for the opacity transform
  elements <-
    df_anims$opacity %>%
    as.character() %>%
    paste_left("opacity: ") %>%
    paste_right("; ") %>%
    paste_right(df_anims$timing %>% include_as_timing_values()) %>%
    encase_in_braces() %>%
    paste_left(df_anims$time_pct %>% add_unit("%", x_right = " ")) %>%
    collapse_strings() %>%
    encase_in_braces() %>%
    paste_left(paste0(anim_id, " ")) %>%
    paste_left("@keyframes ") %>%
    add_keyframes_to_element_i(
      elements = elements,
      index = index
    )

  # Get the associated `style` value for the opacity transform
  elements <-
    max_time_s %>%
    add_unit("s", x_right = " ") %>%
    paste_right("linear") %>%
    paste_right(rep_str) %>%
    paste_right(" both ") %>%
    paste_right(anim_id) %>%
    paste_between("animation: ", ";") %>%
    add_style_to_element_i(
      elements = elements,
      index = index
    )

  elements
}
