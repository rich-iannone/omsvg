process_anims_transform <- function(elements,
                                    df_anims,
                                    index,
                                    max_time_s,
                                    anim_iterations) {

  # Get the element type
  element_type <- elements[[index]][["type"]]

  # Get the element's animation types
  anim_types <-
    transform_anims_vec()[transform_anims_vec() %in% df_anims$anim_type]

  # If there are no animations, return `elements` unchanged
  if (length(anim_types) == 0) {
    return(elements)
  }

  if (!("anim_anchor" %in% df_anims$anim_type)) {
    stop("The `anim_anchor` anim_type must be available in `df_anims`.")
  }

  if (!("anim_position" %in% df_anims$anim_type)) {
    stop("The `anim_position` anim_type must be available in `df_anims`.")
  }

  if (anim_iterations == "infinite") {
    rep_str <- "infinite" %>% paste_left(" ")
  } else {
    rep_str <- as.character(anim_iterations) %>% paste_left(" ")
  }

  #
  # Process `anim_position`
  #

  anim_type_str <- "anim_position"

  # Obtain a subset of animation directives for rotation
  df_anims_subset <-
    subset(df_anims, anim_type == anim_type_str) %>%
    dplyr::arrange(time_s)

  if (element_type == "rect") {

    # Get initial position values (`x` and `y`) in `px` units
    initial_x <- elements[[index]]$x %>% as.character() %>% paste_right("px")
    initial_y <- elements[[index]]$y %>% as.character() %>% paste_right("px")

  } else if (element_type %in% c("circle", "ellipse")) {

    # Get the initial x and y positions of the element
    initial_x <- elements[[index]][["cx"]] %>% as.character() %>% paste_right("px")
    initial_y <- elements[[index]][["cy"]] %>% as.character() %>% paste_right("px")
  }

  # Correct the `time_s` for those entries that were
  # automatically entered
  if ((df_anims_subset$time_s == 0) %>% all()) {
    df_anims_subset <-
      df_anims_subset %>%
      dplyr::mutate(time_s = dplyr::case_when(
        initial == FALSE ~ max_time_s,
        TRUE ~ time_s
      ))
  }

  # Obtain an `anim_id` value to link together
  # element `<styles>` and `@keyframes`
  anim_id <- paste0(anim_type_str, "_", expand_index(index = index))

  df_anims_subset$anim_id <- anim_id
  df_anims_subset$total_time_s <- max_time_s
  df_anims_subset$time_pct <-
    (
      (df_anims_subset$time_s / df_anims_subset$total_time_s) %>%
        round(digits = 4)
    ) * 100

  # Fill in `0s` and `ns` animation states and set
  # initial values if necessary
  df_anims_subset <-
    df_anims_subset %>%
    add_0s_state(attr_names = c("x", "y")) %>%
    add_ns_state(attr_names = c("x", "y"), max_time_s = max_time_s) %>%
    dplyr::mutate(x = dplyr::case_when(
      initial ~ initial_x,
      TRUE ~ x
    )) %>%
    dplyr::mutate(y = dplyr::case_when(
      initial ~ initial_y,
      TRUE ~ y
    ))

  # Get `@keyframes` string for the position transform
  elements <-
    couple_values(df_anims_subset$x, df_anims_subset$y) %>%
    encase_in_css_fn(fn_name = "translate") %>%
    include_as_transform_values() %>%
    paste_right(" ") %>%
    paste_right(df_anims_subset$easing_fn %>% include_as_timing_values()) %>%
    encase_in_braces() %>%
    paste_left(df_anims_subset$time_pct %>% add_unit("%", x_right = " ")) %>%
    collapse_strings() %>%
    encase_in_braces() %>%
    paste_left(paste0(anim_id, " ")) %>%
    paste_left("@keyframes ") %>%
    add_keyframes_to_element_i(
      elements = elements,
      index = index
    )

  # Get the associated `style` value for the position transform
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

  #
  # Process any transform functions either than 'position' and 'anchor'
  #

  # Process any `anim_rotation` entries
  if ("anim_rotation" %in% anim_types) {

    anim_type_str <- "anim_rotation"

    # Obtain a subset of animation directives for rotation
    df_anims_subset <-
      subset(df_anims, anim_type == anim_type_str) %>%
      dplyr::arrange(time_s)

    # Get the initial rotation value
    initial_rotation <- 0

    # Obtain an `anim_id` value to link together
    # element `<styles>` and `@keyframes`
    anim_id <- paste0(anim_type_str, "_", expand_index(index = index))

    df_anims_subset$anim_id <- anim_id
    df_anims_subset$total_time_s <- max_time_s
    df_anims_subset$time_pct <-
      (
        (df_anims_subset$time_s / df_anims_subset$total_time_s) %>%
          round(digits = 4)
      ) * 100

    # Fill in `0s` and `ns` animation states and set
    # initial values if necessary
    df_anims_subset <-
      df_anims_subset %>%
      add_0s_state(attr_names = "rotation") %>%
      add_ns_state(attr_names = "rotation", max_time_s = max_time_s) %>%
      dplyr::mutate(rotation = dplyr::case_when(
        initial ~ initial_rotation,
        TRUE ~ rotation
      ))

    # Get `@keyframes` string for the rotation transform
    elements <-
      df_anims_subset$rotation %>%
      add_unit("deg") %>%
      encase_in_css_fn(fn_name = "rotate") %>%
      include_as_transform_values() %>%
      paste_right(" ") %>%
      paste_right(df_anims_subset$easing_fn %>% include_as_timing_values()) %>%
      encase_in_braces() %>%
      paste_left(df_anims_subset$time_pct %>% add_unit("%", x_right = " ")) %>%
      collapse_strings() %>%
      encase_in_braces() %>%
      paste_left(paste0(anim_id, " ")) %>%
      paste_left("@keyframes ") %>%
      add_keyframes_to_element_i(
        elements = elements,
        index = index
      )

    # Get the associated `style` value for the rotation transform
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
  }

  # Process any `anim_scale` entries
  if ("anim_scale" %in% anim_types) {

    anim_type_str <- "anim_scale"

    # Obtain a subset of animation directives for rotation
    df_anims_subset <-
      subset(df_anims, anim_type == anim_type_str) %>%
      dplyr::arrange(time_s)

    # Get the initial scale value
    initial_scale <- "1,1"

    # Obtain an `anim_id` value to link together
    # element `<styles>` and `@keyframes`
    anim_id <- paste0(anim_type_str, "_", expand_index(index = index))

    df_anims_subset$anim_id <- anim_id
    df_anims_subset$total_time_s <- max_time_s
    df_anims_subset$time_pct <-
      (
        (df_anims_subset$time_s / df_anims_subset$total_time_s) %>%
          round(digits = 4)
      ) * 100

    # Fill in `0s` and `ns` animation states and set
    # initial values if necessary
    df_anims_subset <-
      df_anims_subset %>%
      add_0s_state(attr_names = "scale") %>%
      add_ns_state(attr_names = "scale", max_time_s = max_time_s) %>%
      dplyr::mutate(scale = dplyr::case_when(
        initial ~ initial_scale,
        TRUE ~ scale
      ))

    # Get `@keyframes` string for the scale transform
    elements <-
      df_anims_subset$scale %>%
      encase_in_css_fn(fn_name = "scale") %>%
      include_as_transform_values() %>%
      paste_right(" ") %>%
      paste_right(df_anims_subset$easing_fn %>% include_as_timing_values()) %>%
      encase_in_braces() %>%
      paste_left(df_anims_subset$time_pct %>% add_unit("%", x_right = " ")) %>%
      collapse_strings() %>%
      encase_in_braces() %>%
      paste_left(paste0(anim_id, " ")) %>%
      paste_left("@keyframes ") %>%
      add_keyframes_to_element_i(
        elements = elements,
        index = index
      )

    # Get the associated `style` value for the scale transform
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
  }

  #
  # Process `anim_anchor`
  #

  if (!(element_type %in% c("circle", "ellipse"))) {

    anim_type_str <- "anim_anchor"

    # Obtain a subset of animation directives for rotation
    df_anims_subset <-
      subset(df_anims, anim_type == anim_type_str) %>%
      dplyr::arrange(time_s)

    # Correct the `time_s` for those entries that were
    # automatically entered
    if ((df_anims_subset$time_s == 0) %>% all()) {
      df_anims_subset <-
        df_anims_subset %>%
        dplyr::mutate(time_s = dplyr::case_when(
          initial == FALSE ~ max_time_s,
          TRUE ~ time_s
        ))
    }

    # Obtain an `anim_id` value to link together
    # element `<styles>` and `@keyframes`
    anim_id <- paste0(anim_type_str, "_", expand_index(index = index))

    df_anims_subset$anim_id <- anim_id
    df_anims_subset$total_time_s <- max_time_s
    df_anims_subset$time_pct <-
      (
        (df_anims_subset$time_s / df_anims_subset$total_time_s) %>%
          round(digits = 4)
      ) * 100

    # Fill in `0s` and `ns` animation states
    df_anims_subset <-
      df_anims_subset %>%
      add_0s_state(attr_names = "rotation") %>%
      add_ns_state(attr_names = "rotation", max_time_s = max_time_s)

    # Get `@keyframes` string for the position transform
    elements <-
      couple_values(df_anims_subset$x, df_anims_subset$y) %>%
      encase_in_css_fn(fn_name = "translate") %>%
      include_as_transform_values() %>%
      encase_in_braces() %>%
      paste_left(df_anims_subset$time_pct %>% add_unit("%", x_right = " ")) %>%
      collapse_strings() %>%
      encase_in_braces() %>%
      paste_left(paste0(anim_id, " ")) %>%
      paste_left("@keyframes ") %>%
      add_keyframes_to_element_i(
        elements = elements,
        index = index
      )

    # Get the associated `style` value for the position transform
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
  }

  elements
}
