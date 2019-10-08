build_svg <- function(svg) {

  width  <- svg$width
  height <- svg$height
  title  <- svg$title
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

  # Addition of the built elements
  svg_lines <- c(svg_lines, built_elements)

  # Addition of closing <svg> tag
  svg_lines <- c(svg_lines, "</svg>")

  svg_char <- svg_lines %>% paste(collapse = "\n")

  svg_char
}

build_element_tag <- function(element) {

  e <- element

  type <- e$type

  # Get the main attributes
  attrs_m <-
    e[names(e) %>%
        base::setdiff(c("type", "text", "attrs", "anims", "tag", "path"))]

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

  build_tag(name = type, attrs = attr_str, inner = inner)
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


