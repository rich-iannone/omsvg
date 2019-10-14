#' Animate the position of an element
#'
#' With an [anims()] call, itself passed as to any `anims` argument, the
#' `anim_position()` function can be used to express an animation where the
#' position of the target element changes with time.
#'
#' @param x,y The position of the element, expressed as `x` and `y`, at the
#'   keyframe time (given as the LHS value in the [anims()] call).
#' @param timing The timing function to use for the movement to the new
#'   position.
#' @param initial Should this position be the initial position of the element?
#'   If so, use `TRUE` and any values provided to `x` and `y` will be
#'   disregarded.
#'
#' @examples
#' # Basic animation of an element's
#' # position (moving to a new `x` and
#' # `y` position)
#' SVG(width = 300, height = 300) %>%
#'   svg_rect(
#'     x = 50, y = 50,
#'     width = 50, height = 50,
#'     attrs = attrs_pres(
#'       stroke = "magenta",
#'       fill = "lightblue"
#'     ),
#'     anims = anims(
#'       2.0 ~ anim_position(x = 100, y = 50),
#'     )
#'   )
#'
#' @export
anim_position <- function(x = NULL,
                          y = NULL,
                          timing = NULL,
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

  if (is.null(timing)) {
    timing <- "linear()"
  } else {
    if (timing == "linear") {
      timing <- "linear()"
    }
  }

  anim_property <-
    list(
      x = x,
      y = y,
      timing = timing,
      initial = initial
    )

  class(anim_property) <- "anim_position"
  anim_property
}

process_animation_position <- function(elements,
                                       anim_types,
                                       df_anims,
                                       index,
                                       max_time_s,
                                       anim_iterations) {

  anim_type_str <- "anim_position"

  if (anim_iterations == "infinite") {
    rep_str <- "infinite" %>% paste_left(" ")
  } else {
    rep_str <- as.character(anim_iterations) %>% paste_left(" ")
  }

  if (!(anim_type_str %in% anim_types)) {
    return(elements)
  }

  # Obtain a subset of animation directives
  df_anims <- subset(df_anims, anim_type == anim_type_str)

  # Get initial position values (`x` and `y`) in `px` units
  initial_x <- elements[[index]]$x %>% as.character() %>% paste_right("px")
  initial_y <- elements[[index]]$y %>% as.character() %>% paste_right("px")

  # Obtain an `anim_id` value to link together
  # element `<styles>` and `@keyframes`
  anim_id <- paste0(anim_type_str, "_", expand_index(index = index))

  df_anims$anim_id <- anim_id
  df_anims$total_time_s <- max_time_s
  df_anims$time_pct <-
    ((df_anims$time_s / df_anims$total_time_s) %>% round(digits = 4)) * 100

  # Fill in `0s` and `ns` animation states
  df_anims <-
    df_anims %>%
    add_0s_state(attr_names = c("x", "y")) %>%
    add_ns_state(attr_names = c("x", "y"), max_time_s = max_time_s)

  # Set initial `x` and `y` for any rows where initial is `TRUE`
  df_anims <-
    df_anims %>%
    dplyr::mutate(x = dplyr::case_when(
      initial ~ initial_x,
      TRUE ~ x
    )) %>%
    dplyr::mutate(y = dplyr::case_when(
      initial ~ initial_y,
      TRUE ~ y
    ))

  # Get `@keyframes` string
  keyframes <-
    couple_values(df_anims$x, df_anims$y) %>%
    encase_in_css_fn(fn_name = "translate") %>%
    include_as_transform_values() %>%
    paste_right(paste0(" ", df_anims$timing %>% include_as_timing_values())) %>%
    encase_in_braces() %>%
    paste_left(df_anims$time_pct %>% add_unit("%", x_right = " ")) %>%
    collapse_strings() %>%
    encase_in_braces() %>%
    paste_left(paste0(anim_id, " ")) %>%
    paste_left("@keyframes ")

  # Get the associated `style` value
  style <-
    max_time_s %>%
    add_unit("s", x_right = " ") %>%
    paste_right("linear") %>%
    paste_right(rep_str) %>%
    paste_right(" both ") %>%
    paste_right(anim_id) %>%
    paste_between("animation: ", ";")

  # Append the `keyframes` string to `elements[[index]]$anims_built$keyframes`
  elements <-
    add_keyframes_to_element_i(
      elements = elements,
      index = index,
      keyframes = keyframes
    )

  # Append the `style` string to `elements[[index]]$anims_built$styles`
  elements <-
    add_style_to_element_i(
      elements = elements,
      index = index,
      style = style
    )

  elements
}
