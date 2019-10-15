process_animations_for_element <- function(elements,
                                           index,
                                           anim_iterations) {

  df_anims <- elements[[index]][["anims"]]

  element_type <- elements[[index]][["type"]]

  # Get all animation timings
  timings <- df_anims$time_s %>% sort()

  # Get the maximum and minimum times in seconds
  max_time_s <- max(timings, na.rm = TRUE)

  if (element_type == "rect") {

    # Get the initial x and y positions of the element
    x_initial <- elements[[index]][["x"]] %>% as.character() %>% paste_right("px")
    y_initial <- elements[[index]][["y"]] %>% as.character() %>% paste_right("px")

    # Get the anchor values for the element
    x_anchor <-
      ((elements[[index]]$width / 2) * -1) %>%
      as.character() %>% paste_right("px")

    y_anchor <-
      ((elements[[index]]$height / 2) * -1) %>%
      as.character() %>% paste_right("px")

  } else if (element_type %in% c("circle", "ellipse")) {

    # Get the initial x and y positions of the element
    x_initial <- elements[[index]][["cx"]] %>% as.character() %>% paste_right("px")
    y_initial <- elements[[index]][["cy"]] %>% as.character() %>% paste_right("px")

    # Get the anchor values for the element
    if (element_type == "circle") {

      x_anchor <- y_anchor <-
        ((elements[[index]]$r / 2) * -1) %>%
        as.character() %>% paste_right("px")

    } else {

      x_anchor <-
        ((elements[[index]]$rx / 2) * -1) %>%
        as.character() %>% paste_right("px")
      y_anchor <-
        ((elements[[index]]$ry / 2) * -1) %>%
        as.character() %>% paste_right("px")
    }
  }

  if ("replace_xy" %in% names(df_anims) &&
      (c("anchor", "initial") %in% df_anims$replace_xy) %>% any()) {

    df_anims <-
      df_anims %>%
      dplyr::mutate(x = dplyr::case_when(
        replace_xy == "anchor" ~ x_anchor,
        replace_xy == "initial" ~ x_initial,
        TRUE ~ x
      )) %>%
      dplyr::mutate(y = dplyr::case_when(
        replace_xy == "anchor" ~ y_anchor,
        replace_xy == "initial" ~ y_initial,
        TRUE ~ y
      ))
  }

  # Get all of the different types of animations
  anim_types <- df_anims$anim_type %>% unique()

  elements <-
    elements %>%
    process_anims_transform(
      df_anims = df_anims,
      index = index,
      max_time_s = max_time_s,
      anim_iterations = anim_iterations
    ) %>%
    process_anims_opacity(
      df_anims = df_anims,
      index = index,
      max_time_s = max_time_s,
      anim_iterations = anim_iterations
    )

  elements
}

# Determine if there are any animations
any_anims <- function(elements) {

  elements %>%
    vapply(
      FUN.VALUE = logical(1),
      USE.NAMES = FALSE,
      function(x) {length(x$anims) > 0}
    ) %>%
    any()
}

# Determine which elements have animations
which_have_anims <- function(elements) {

  elements %>%
    vapply(
      FUN.VALUE = logical(1),
      USE.NAMES = FALSE,
      function(x) {length(x$anims) > 0}
    ) %>%
    which()
}

extract_timings <- function(elements) {

  which_have_anims <-
    elements %>%
    vapply(
      FUN.VALUE = logical(1),
      USE.NAMES = FALSE,
      function(x) {length(x$anims) > 0}
    ) %>%
    which()
}

add_keyframes_to_element_i <- function(keyframes, elements, index) {

  if (is.null(elements[[index]]$anims_built$keyframes)) {
    elements[[index]]$anims_built$keyframes <- keyframes
  } else {
    elements[[index]]$anims_built$keyframes <-
      c(elements[[index]]$anims_built$keyframes, keyframes)
  }

  elements
}

add_style_to_element_i <- function(style, elements, index) {

  if (is.null(elements[[index]]$anims_built$style)) {
    elements[[index]]$anims_built$style <- style
  } else {
    elements[[index]]$anims_built$style <-
      c(elements[[index]]$anims_built$style, style)
  }

  elements
}

include_as_transform_values <- function(x) {
  x %>% paste_between("transform: ", ";")
}

include_as_timing_values <- function(x) {
  x %>% paste_between("animation-timing-function: ", ";")
}

add_0s_state <- function(df_anims,
                         attr_names,
                         set_initial = TRUE) {

  if (min(df_anims$time_s, na.rm = TRUE) == 0) {
    return(df_anims)
  }

  df_0_row <- df_anims[1, ]
  df_0_row$time_s <- 0.0
  df_0_row$time_pct <- 0.0

  if (isTRUE(set_initial)) {
    df_0_row$initial <- TRUE
  }

  for (attr_name in attr_names) {

    attr_vals <- df_anims[[attr_name]]

    attr_val_i <-
      attr_vals[Position(function(attr_vals) !is.na(attr_vals), attr_vals)]

    df_0_row[1, attr_name] <- attr_val_i
  }

  dplyr::bind_rows(df_0_row, df_anims)
}

add_ns_state <- function(df_anims,
                         attr_names,
                         max_time_s) {

  if (max(df_anims$time_s, na.rm = TRUE) == max_time_s) {
    return(df_anims)
  }

  df_n_row <- df_anims[nrow(df_anims), ]
  df_n_row$time_s <- max_time_s
  df_n_row$time_pct <- 100.0

  for (attr_name in attr_names) {

    attr_vals <- rev(df_anims[[attr_name]])

    attr_val_n <-
      attr_vals[Position(function(attr_vals) !is.na(attr_vals), attr_vals)]

    df_n_row[1, attr_name] <- attr_val_n
  }

  dplyr::bind_rows(df_anims, df_n_row)
}
