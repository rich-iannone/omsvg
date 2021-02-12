#' Express animations for an element
#'
#' All SVG element functions in **omsvg** (the `svg_*()` functions) are
#' animatable through their `anims` argument. The `anims()` function should be
#' used with that argument should we want to express animations for the element.
#' Within the `anims()` function call, we can insert a list of formulas that
#' incorporate calls to any of the `anim_*()` functions (e.g.,
#' [anim_position()], [anim_rotation()], etc.), and, have keyframe times as part
#' of the formula.
#'
#' A useful template to use for an `anims()` call within an `svg_*()` function
#' is:
#'
#' \preformatted{
#' anims = anims(
#'   <time_i> ~ <anim_fn>(...),
#'   ...,
#'   <time_n> ~ <anim_fn>(...)
#'   )
#' }
#'
#' We can also use multiple calls to `anim_*()` functions for each distinct keyframe
#' time by placing those calls in a list:
#'
#' \preformatted{
#' anims = anims(
#'   <time_i> ~ list(
#'     <anim_fn_x>(...),
#'     <anim_fn_y>(...)
#'     ),
#'   ...,
#'   <time_n> ~ list(
#'     <anim_fn_x>(...),
#'     <anim_fn_y>(...)
#'     )
#'   )
#' }
#'
#' @param ... One or more animations that included the use of `anim_*()`
#'   functions, expressed as two-sided formulas. The LHS provides the keyframe
#'   time (in units of seconds) and the RHS is the associated `anim_*()` call.
#'
#' @return A tibble of animation directives.
#'
#' @examples
#' if (interactive()) {
#'
#' # Basic animation of an element's
#' # position (moving to a new `x` and
#' # `y` position)
#' svg_1 <-
#'   SVG(width = 300, height = 300) %>%
#'   svg_rect(
#'     x = 50, y = 50,
#'     width = 50, height = 50,
#'     attrs = svg_attrs_pres(
#'       stroke = "magenta",
#'       fill = "lightblue"
#'     ),
#'     anims = anims(
#'       2.0 ~ anim_position(x = 100, y = 50)
#'     )
#'   )
#'
#' # We can define multiple animations
#' # for a single element: put them in a
#' # `list()`; the `easing_fn` function for
#' # both `anim_*()` function is no longer
#' # linear but now eases in and out
#' svg_2 <-
#'   SVG(width = 300, height = 300) %>%
#'   svg_rect(
#'     x = 50, y = 50,
#'     width = 50, height = 50,
#'     attrs = svg_attrs_pres(
#'       stroke = "black",
#'       fill = "yellow"
#'     ),
#'     anims = anims(
#'       0.5 ~ list(
#'         anim_position(x = 50, y = 50, easing_fn = ease_in_out()),
#'         anim_rotation(0, easing_fn = ease_in_out())
#'       ),
#'       2.0 ~ list(
#'         anim_position(x = 200, y = 50, easing_fn = ease_in_out()),
#'         anim_rotation(90, easing_fn = ease_in_out())
#'       )
#'     )
#'   )
#'
#' # The initial state of the element
#' # can be used in any `anim_*()`
#' # function with `initial = TRUE`
#' svg_3 <-
#'   SVG(width = 300, height = 300) %>%
#'   svg_rect(
#'     x = 50, y = 50,
#'     width = 50, height = 50,
#'     attrs = svg_attrs_pres(
#'       stroke = "black",
#'       fill = "yellow"
#'     ),
#'     anims = anims(
#'       1.0 ~ list(
#'         anim_position(initial = TRUE),
#'         anim_rotation(initial = TRUE)
#'       ),
#'       3.0 ~ list(
#'         anim_position(x = 200, y = 50),
#'         anim_rotation(90)
#'       ),
#'       5.0 ~ list(
#'         anim_position(initial = TRUE),
#'         anim_rotation(initial = TRUE)
#'       )
#'     )
#'   )
#' }
#'
#' @export
anims <- function(...) {

  x <- list(...)

  # If nothing is provided, stop the function
  if (length(x) == 0) {
    stop("Nothing was provided to `...`:\n",
         " * Use formula expressions to define animation timings",
         call. = FALSE)
  }

  all_formulas <-
    x %>%
    vapply(
      FUN.VALUE = logical(1),
      USE.NAMES = FALSE,
      FUN = function(x) {
        inherits(x, "formula")
      }
    ) %>%
    all()

  if (!all_formulas) {
    stop("Only two-sided formulas should be provided to `...`",
         call. = FALSE)
  }

  anims_df <-
    x %>%
    lapply(function(x) {

      rhs <- x %>% rlang::f_rhs() %>% rlang::eval_tidy()

      anims_dfs <- data.frame()

      # If `rhs` is not a list of `anim_*` objects (but a single one)
      # follow the first case, otherwise, parse each list item
      if (class(rhs) %>% tidy_grepl("^anim_")) {

        lhs <- x %>% rlang::f_lhs()

        easing_fn <- rhs$easing_fn

        if (is.null(easing_fn)) {
          easing_fn <- "linear"
        }

        anim_type <- class(rhs)

        rhs$easing_fn <- easing_fn
        rhs$time_s <- lhs
        rhs$anim_type <- anim_type

        rhs <-
          rhs %>%
          unclass() %>%
          as.data.frame(stringsAsFactors = FALSE)

        anims_dfs <- dplyr::bind_rows(anims_dfs, rhs)

      } else {

        for (i in seq(length(rhs))) {

          lhs <- x %>% rlang::f_lhs()
          rhs_i <- rhs[[i]]

          easing_fn <- rhs_i$easing_fn

          if (is.null(easing_fn)) {
            easing_fn <- "linear"
          }

          anim_type <- class(rhs_i)

          rhs_i$easing_fn <- easing_fn
          rhs_i$time_s <- lhs
          rhs_i$anim_type <- anim_type

          rhs_i <-
            rhs_i %>%
            unclass() %>%
            as.data.frame(stringsAsFactors = FALSE)

          anims_dfs <- dplyr::bind_rows(anims_dfs, rhs_i)
        }
      }

      anims_dfs <-
        anims_dfs %>%
        dplyr::select(
          dplyr::one_of(c("easing_fn", "initial", "time_s", "anim_type")),
          dplyr::everything()
        )

      anims_dfs
    }) %>%
    do.call(dplyr::bind_rows, .) %>%
    dplyr::arrange(anim_type, time_s)

  # Get all of the different anim types available
  anim_types <- anims_df %>% dplyr::pull(anim_type) %>% unique()

  # Determine if the anchor is always `"center"` or all NA
  # anchor_center <-
  #   (
  #     anims_df %>%
  #       dplyr::pull(anchor) %>%
  #       unique() %in%
  #       c("center", NA_character_)
  #   ) %>%
  #   all()

  if (has_transform_anims(anim_types = anim_types) &&
    has_position_transform_defined(anim_types = anim_types)) {

    # Add anchor df lines (to be further processed with correct
    # `time_s` and `x`/`y` values)
    anims_df <-
      anims_df %>%
      dplyr::bind_rows(anims_df_anchor_row()) %>%
      dplyr::bind_rows(anims_df_anchor_row(initial = FALSE))

  } else if (has_transform_anims(anim_types = anim_types)) {

    # Add anchor df lines (to be further processed with correct
    # `time_s` and `x`/`y` values)
    anims_df <-
      anims_df %>%
      dplyr::bind_rows(anims_df_anchor_row()) %>%
      dplyr::bind_rows(anims_df_anchor_row(initial = FALSE)) %>%
      dplyr::bind_rows(anims_df_position_row()) %>%
      dplyr::bind_rows(anims_df_position_row(initial = FALSE))
  }

  anims_df %>%
    dplyr::arrange(time_s)
}

#' Create a custom easing function for animation
#'
#' @param x1,y1,x2,y2 The `x` and `y` values for the first and second bezier
#'   control points.
#'
#' @return A `cubic-bezier` function call as a string for use as a CSS property.
#'
#' @export
cubic_bezier <- function(x1 = 0.5,
                         y1 = 0.5,
                         x2 = 0.5,
                         y2 = 0.5) {

  if (x1 < 0 || x1 > 1) {
    stop("The `x1` value must be a number in the range of 0 and 1.",
         call. = FALSE)
  }

  if (x2 < 0 || x2 > 1) {
    stop("The `x2` value must be a number in the range of 0 and 1.",
         call. = FALSE)
  }

  bezier_vctr <- c(x1, y1, x2, y2)

  include_as_bezier_values(bezier_vctr = bezier_vctr)
}

#' Use an 'easing in' animation
#'
#' The `ease_in()` function can be used as a value for the `easing_fn` argument,
#' which is available in every `anim_*()` function (e.g., [anim_position()]).
#'
#' @param power The preset to use for the easing in cubic bezier function.
#'
#' @return A `cubic-bezier` function call as a string for use as a CSS property.
#'
#' @export
ease_in <- function(power = "basic") {

  validate_easing_power(power = power)

  easing_fn(type = "ease_in", power = {{ power }})
}

#' Use an 'easing out' animation
#'
#' The `ease_out()` function can be used as a value for the `easing_fn` argument,
#' which is available in every `anim_*()` function (e.g., [anim_position()]).
#'
#' @inheritParams ease_in
#'
#' @return A `cubic-bezier` function call as a string for use as a CSS property.
#'
#' @export
ease_out <- function(power = "basic") {

  validate_easing_power(power = power)

  easing_fn(type = "ease_out", power = {{ power }})
}

#' Use an 'easing in and out' animation
#'
#' The `ease_in_out()` function can be used as a value for the `easing_fn`
#' argument, which is available in every `anim_*()` function (e.g.,
#' [anim_position()]).
#'
#' @inheritParams ease_in
#'
#' @return A `cubic-bezier` function call as a string for use as a CSS property.
#'
#' @export
ease_in_out <- function(power = "basic") {

  validate_easing_power(power = power)

  easing_fn(type = "ease_in_out", power = {{ power }})
}

#' Use a linear movement for animation
#'
#' The `linear()` function can be used as a value for the `easing_fn` argument,
#' which is available in every `anim_*()` function (e.g., [anim_position()]).
#'
#' @return A `linear` function call as a string for use as a CSS property.
#'
#' @export
linear <- function() {

  "linear()"
}

#' Use a 'step-start' animation
#'
#' The `step_start()` function can be used as a value for the `easing_fn`
#' argument, which is available in every `anim_*()` function (e.g.,
#' [anim_position()]).
#'
#' @return A `step-start` function call as a string for use as a CSS property.
#'
#' @export
step_start <- function() {

  "step-start()"
}

#' Use a 'step-end' animation
#'
#' The `step_end()` function can be used as a value for the `easing_fn`
#' argument, which is available in every `anim_*()` function (e.g.,
#' [anim_position()]).
#'
#' @return A `step-end` function call as a string for use as a CSS property.
#'
#' @export
step_end <- function() {

  "step-end()"
}

easing_fn <- function(type, power) {

  # Create a formatted `cubic-bezier()` function from
  # a query of the `bezier_vals()` table
  bezier_vals() %>%
    dplyr::filter(type == {{ type }}) %>%
    dplyr::filter(power == {{ power}}) %>%
    .["vals"] %>%
    dplyr::pull(vals) %>%
    .[[1]] %>%
    include_as_bezier_values()
}

bezier_vals <- function() {

  dplyr::tribble(
    ~type,          ~power,      ~vals,
    "ease_in",      "basic",     c(0.420, 0.000, 1.000, 1.000),
    "ease_in",      "quad",      c(0.550, 0.085, 0.680, 0.530),
    "ease_in",      "cubic",     c(0.550, 0.055, 0.675, 0.190),
    "ease_in",      "quart",     c(0.895, 0.030, 0.685, 0.220),
    "ease_in",      "quint",     c(0.755, 0.050, 0.855, 0.060),
    "ease_in",      "sine",      c(0.470, 0.000, 0.745, 0.715),
    "ease_in",      "expo",      c(0.950, 0.050, 0.795, 0.035),
    "ease_in",      "circ",      c(0.600, 0.040, 0.980, 0.335),
    "ease_in",      "back",      c(0.600, -0.280, 0.735, 0.045),

    "ease_out",     "basic",     c(0.000, 0.000, 0.580, 1.000),
    "ease_out",     "quad",      c(0.250, 0.460, 0.450, 1.000),
    "ease_out",     "cubic",     c(0.215, 0.610, 0.355, 1.000),
    "ease_out",     "quart",     c(0.165, 0.840, 0.440, 1.000),
    "ease_out",     "quint",     c(0.230, 1.000, 0.320, 1.000),
    "ease_out",     "sine",      c(0.390, 0.575, 0.565, 1.000),
    "ease_out",     "expo",      c(0.190, 1.000, 0.220, 1.000),
    "ease_out",     "circ",      c(0.075, 0.820, 0.165, 1.000),
    "ease_out",     "back",      c(0.175, 0.885, 0.320, 1.275),

    "ease_in_out",  "basic",     c(0.420, 0.000, 0.580, 1.000),
    "ease_in_out",  "quad",      c(0.455, 0.030, 0.515, 0.955),
    "ease_in_out",  "cubic",     c(0.645, 0.045, 0.355, 1.000),
    "ease_in_out",  "quart",     c(0.770, 0.000, 0.175, 1.000),
    "ease_in_out",  "quint",     c(0.860, 0.000, 0.070, 1.000),
    "ease_in_out",  "sine",      c(0.445, 0.050, 0.550, 0.950),
    "ease_in_out",  "expo",      c(1.000, 0.000, 0.000, 1.000),
    "ease_in_out",  "circ",      c(0.785, 0.135, 0.150, 0.860),
    "ease_in_out",  "back",      c(0.680, -0.550, 0.265, 1.550)
  )
}

validate_easing_power <- function(power) {

  power_values <-
    bezier_vals() %>%
    dplyr::pull(power) %>%
    unique()

  if (!(power %in% power_values)) {
    stop("The value for `power` is not valid.", call. = FALSE)
  }
}

include_as_bezier_values <- function(bezier_vctr) {

  bezier_vctr %>%
    as.character() %>%
    collapse_strings(",") %>%
    encase_in_css_fn("cubic-bezier")
}

transform_anims_vec <- function() {

  c(
    "anim_position",
    "anim_rotation",
    "anim_scale",
    "anim_skew"
  )
}

anims_df_anchor_row <- function(time_s = 0.0,
                                initial = TRUE,
                                anchor = "center") {
  dplyr::tibble(
    easing_fn = "linear()",
    initial = initial,
    time_s = time_s,
    anim_type = "anim_anchor",
    x = "0px",
    y = "0px",
    anchor = anchor,
    replace_xy = "anchor"
  )
}

anims_df_position_row <- function(time_s = 0.0,
                                  initial = TRUE,
                                  anchor = "center") {
  dplyr::tibble(
    easing_fn = "linear()",
    initial = initial,
    time_s = time_s,
    anim_type = "anim_position",
    x = "0px",
    y = "0px",
    anchor = anchor,
    replace_xy = "initial"
  )
}

# Determine whether there are transform-type animations defined
has_transform_anims <- function(anim_types) {
  (anim_types %in% transform_anims_vec()) %>%
    any()
}

# Determine whether the `"anim_position"` transform has been
# specifically used
has_position_transform_defined <- function(anim_types) {
  "anim_position" %in% anim_types
}
