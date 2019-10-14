process_animations_for_element <- function(elements,
                                           index,
                                           anim_iterations) {

  df_anims <- elements[[index]][["anims"]]

  # Get all animation timings
  timings <- df_anims$time_s %>% sort()

  # Get the maximum and minimum times in seconds
  max_time_s <- max(timings, na.rm = TRUE)
  min_time_s <- min(timings, na.rm = TRUE)

  # Get all of the different types of animations
  anim_types <- df_anims$anim_type %>% unique()

  elements <-
    elements %>%
    process_animation_position(
      anim_types = anim_types,
      df_anims = df_anims,
      index = index,
      max_time_s = max_time_s,
      anim_iterations = anim_iterations
    ) %>%
    process_animation_rotation(
      anim_types = anim_types,
      df_anims = df_anims,
      index = index,
      max_time_s = max_time_s,
      anim_iterations = anim_iterations
    ) %>%
    process_animation_opacity(
      anim_types = anim_types,
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

add_keyframes_to_element_i <- function(elements, index, keyframes) {

  if (is.null(elements[[index]]$anims_built$keyframes)) {
    elements[[index]]$anims_built$keyframes <- keyframes
  } else {
    elements[[index]]$anims_built$keyframes <-
      c(elements[[index]]$anims_built$keyframes, keyframes)
  }

  elements
}

add_style_to_element_i <- function(elements, index, style) {

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
                         attr_names) {

  if (min(df_anims$time_s, na.rm = TRUE) == 0) {
    return(df_anims)
  }

  df_0_row <- df_anims[1, ]
  df_0_row$time_s <- 0.0
  df_0_row$time_pct <- 0.0
  df_0_row$initial <- TRUE

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
