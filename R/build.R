build_svg <- function(svg) {

  width  <- svg$width
  height <- svg$height
  title  <- svg$title
  anim_iterations <- svg$anim_iterations

  desc   <- svg$desc

  elements <- svg$elements
  defs <- svg$defs

  # Get vector of `id`s
  ids <-
    elements %>%
    names() %>%
    vapply(
      FUN.VALUE = character(1),
      USE.NAMES = FALSE,
      function(x) ifelse(x == ":no_id:", NA_character_, x)
    )

  # Get vector of `type`s
  types <-
    elements %>%
    vapply(
      FUN.VALUE = character(1),
      USE.NAMES = FALSE,
      `[[`, 1
    )

  if (any_anims(elements = elements)) {

    anim_elements <- which_have_anims(elements = elements)

    keyframes <- c()

    # get keyframes for each element with animations
    for (element_i in anim_elements) {

      elements <-
        process_animations_for_element(
          elements = elements,
          index = element_i,
          anim_iterations = anim_iterations
        )

      keyframes <- c(keyframes, elements[[element_i]]$anims_built$keyframes)
    }

    # Build all styles
    built_styles <-
      keyframes %>%
      collapse_strings("\n") %>%
      paste_left("<style>\n") %>%
      paste_right("\n</style>")

  } else {
    built_styles <- c()
  }

  # Build all elements
  built_elements <-
    elements %>%
    vapply(
      FUN.VALUE = character(1),
      USE.NAMES = FALSE,
      function(x) build_element_tag(x)
    )

  # Build all defs
  built_defs <-
    defs %>%
    unlist() %>%
    paste(collapse = "\n") %>%
    paste_left("<defs>\n") %>%
    paste_right("\n</defs>")

  svg_lines <- c()

  viewbox_dimensions <-
    c(0, 0, width, height) %>%
    paste(collapse = " ")

  svg_o_tag <-
    paste0(
      "<svg ",
      "width=\"", width, "px\" ",
      "height=\"", height, "px\" ",
      "viewBox=\"", viewbox_dimensions, "\" ",
      "version=\"1.1\" ",
      "xmlns=\"http://www.w3.org/2000/svg\" ",
      "xmlns:xlink=\"http://www.w3.org/1999/xlink\">"
    )

  svg_lines <- c(svg_lines, svg_o_tag)

  if (!is.null(title)) {

    svg_title_tag <- paste0("<title>", title, "</title>")
    svg_lines <- c(svg_lines, svg_title_tag)
  }

  if (!is.null(desc)) {

    svg_desc_tag <- paste0("<desc>", desc, "</desc>")
    svg_lines <- c(svg_lines, svg_desc_tag)
  }

  # Addition of built definitions
  svg_lines <- c(svg_lines, built_defs)

  # Addition of built styles
  if (length(built_styles) > 0) {
    svg_lines <- c(svg_lines, built_styles)
  }

  # Addition of the built elements
  svg_lines <- c(svg_lines, built_elements)

  # Addition of closing <svg> tag
  svg_lines <- c(svg_lines, "</svg>")

  svg_char <- svg_lines %>% paste(collapse = "\n")

  invisible(svg_char)
}

build_element_tag <- function(element) {

  e <- element

  type <- e$type

  # Get the main attributes
  attrs_m <-
    e[names(e) %>%
        base::setdiff(c("type", "text", "attrs", "anims", "anims_built", "tag", "path"))]

  # Get the extra attributes for the shape
  attrs_e <- e$attrs

  # Combine all attributes together
  attrs <- c(attrs_m, attrs_e)

  # Get the `text` attribute, if available
  if (type == "text") {
    inner <- e$text
  } else {
    inner <- ""
  }

  attr_names <- names(attrs)

  attr_str <-
    seq(attrs) %>%
    lapply(function(x) build_attr(name = attr_names[x], value = attrs[[x]])) %>%
    unlist()

  built_tag <- build_tag(name = type, attrs = attr_str, inner = inner)

  if (!is.null(e$anims_built$style)) {

    styles <- rev(e$anims_built$style)

    for (style in styles) {

      built_tag <-
        built_tag %>%
        paste_left(style %>% paste_left("<g style=\"") %>% paste_right("\">\n")) %>%
        paste_right("\n</g>")
    }

    if (built_tag %>% tidy_grepl("anim_position")) {
      built_tag <- built_tag %>% tidy_gsub("x=.*? y=.*? ", "")
    }

    if (built_tag %>% tidy_grepl("anim_scale")) {
      built_tag <- built_tag %>% tidy_gsub("x=.*? y=.*? ", "")
    }

    if (built_tag %>% tidy_grepl("anim_rotation")) {
      built_tag <- built_tag %>% tidy_gsub("cx=.*? cy=.*? ", "")
    }
  }

  built_tag
}

build_attr <- function(name, value = NULL) {

  if (is.null(value)) {
    return(NULL)
  }

  paste0(name, "=", "\"", value, "\"")
}

build_tag <- function(name, attrs, inner = NULL) {

  tag_o <- paste0("<", name)

  attrs_str <- paste(attrs, collapse = " ")

  if (is.null(inner)) {
    tag_str <- paste0(tag_o, " ", attrs_str, "/>")
    return(tag_str)
  }

  tag_c <- paste0("</", name, ">")

  paste0(tag_o, " ", attrs_str, ">", inner, tag_c)
}


