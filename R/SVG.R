#' Create an `svg` object
#'
#' The `SVG()` function is the entry point for building an SVG from the ground
#' up. We can provide predefined `height` and `width` attributes that define
#' the canvas size for the SVG. From here, we would want to use functions that
#' add elements to the SVG object (e.g., [svg_rect()], [svg_circle()], etc.) and
#' thus progressively build the graphic.
#'
#' @param width,height The width and height attributes on the top-level `<svg>`
#'   element. Both of these attributes are optional but, if provided, take in a
#'   variety of dimensions and keywords. If numerical values are solely used,
#'   they are assumed to be 'px' length values. Dimensions can be percentage
#'   values (i.e., `"75%"`) or length values with the following units: `"em"`,
#'   `"ex"`, `"px"`, `"in"`, `"cm"`, `"mm"`, `"pt"`, and `"pc"`. Using `NULL`,
#'   the default, excludes the attribute.
#' @param viewbox An optional set of dimensions that defines the SVG `viewBox`
#'   attribute. The `viewBox` for an SVG element is the position and dimension,
#'   in user space, of an SVG viewport. If supplied, this could either be in the
#'   form of a four-element, numeric vector corresponding to the `"min-x"`,
#'   `"min-y"`, `"width"`, and `"height"` of the rectangle, or, as `TRUE` which
#'   uses the vector `c(0, 0, width, height)`. Using `NULL`, the default,
#'   excludes this attribute.
#' @param title The `<title>` tag for the finalized SVG.
#' @param desc The `<desc>` tag for the finalized SVG.
#' @param incl_xmlns Should the `xmlns` attribute be included in the `<svg>`
#'   tag? This attribute is only required on the outermost `svg` element of SVG
#'   documents, and, it's unnecessary for inner `svg` elements or inside of HTML
#'   documents. By default, this is set to `FALSE`.
#' @param oneline An option to compress the resulting SVG tags such that they
#'   are reduced to one line.
#' @param anim_iterations How many should an SVG animation (if defined by use of
#'   the [anims()] function) be played? By default this is `"infinite"` (i.e.,
#'   looped indefinitely) but we can specify the animation iteration count as
#'   a positive number.
#'
#' @return An `svg` object.
#'
#' @examples
#' if (interactive()) {
#'
#' # Create an SVG with nothing drawn
#' # within it
#' svg <- SVG(width = 200, height = 100)
#'
#' # Add a rectangle and then a circle
#' svg <-
#'   svg %>%
#'   svg_rect(x = 20, y = 20, width = 40, height = 40) %>%
#'   svg_circle(x = 100, y = 40, diameter = 40)
#' }
#'
#' @export
SVG <- function(width = NULL,
                height = NULL,
                viewbox = NULL,
                title = NULL,
                desc = NULL,
                incl_xmlns = FALSE,
                oneline = FALSE,
                anim_iterations = "infinite") {

  if (is.null(anim_iterations)) {
    anim_iterations <- "infinite"
  }

  if (all(!is.numeric(anim_iterations) & anim_iterations != "infinite")) {
    stop("The `anim_iterations` value must either be `\"infinite\"` or a positive number.",
         call. = FALSE)
  }

  if (is.numeric(anim_iterations)) {
    anim_iterations <- anim_iterations %>% round(digits = 0) %>% abs()
  }

  obj <-
    list(
      width = width,
      height = height,
      viewbox = viewbox,
      title = title,
      desc = desc,
      incl_xmlns = incl_xmlns,
      oneline = oneline,
      anim_iterations = anim_iterations,
      elements = list(),
      defs = list(),
      anims = list(),
      filters = list()
    )

  class(obj) <- "svg"
  obj
}

#' Create a compact `svg` object
#'
#' The `SVG_()` function is a variation on [SVG()] (the entry point for building
#' an SVG) in that the output tags will be as compact as possible (fewer
#' linebreaks, less space characters). This is a reasonable option if the
#' eventual use for the generated SVG is as inline SVG within HTML documents.
#'
#' @inheritParams SVG
#'
#' @return An `svg` object.
#'
#' @examples
#' if (interactive()) {
#'
#' # Create a simple SVG with a rectangle and a circle
#' svg <-
#'   SVG_(width = 100, height = 50) %>%
#'   svg_rect(x = 0, y = 0, width = 30, height = 20) %>%
#'   svg_circle(x = 50, y = 10, diameter = 20)
#' }
#'
#' @export
SVG_ <- function(width = NULL,
                 height = NULL,
                 viewbox = TRUE) {

  SVG(
    width = width,
    height = height,
    viewbox = viewbox,
    title = NULL,
    desc = NULL,
    incl_xmlns = FALSE,
    oneline = TRUE,
    anim_iterations = "infinite"
  )
}

#' Create a text-height `svg` object
#'
#' The `SVG_t()` function is a variation on [SVG()] (the entry point for
#' building an SVG) in that the output tags will be both as compact as possible
#' (fewer linebreaks, less space characters) and the height is relative to line
#' height of text (at `"0.75em"`). This is a good option if the eventual use for
#' the generated SVG is to be integrated with text in HTML `<p>` elements. For
#' scaling to function properly, the provision of the `viewbox` is required
#' here.
#'
#' @param height The height attribute on the top-level `<svg>` element. The
#'   default of `"0.75em"` is recommended here so that SVGs are scaled nicely to
#'   any adjacent text.
#' @inheritParams SVG
#'
#' @return An `svg` object.
#'
#' @examples
#' if (interactive()) {
#'
#' # Create a simple SVG with a rectangle and a circle
#' svg <-
#'   SVG_t(viewbox = c(0, 0, 60, 20)) %>%
#'   svg_rect(x = 0, y = 0, width = 30, height = 20) %>%
#'   svg_circle(x = 50, y = 10, diameter = 20)
#' }
#'
#' @export
SVG_t <- function(height = "0.75em",
                  viewbox) {

  SVG(
    width = NULL,
    height = height,
    viewbox = viewbox,
    title = NULL,
    desc = NULL,
    incl_xmlns = FALSE,
    oneline = TRUE,
    anim_iterations = "infinite"
  )
}

#' Import an SVG file and create an `svg` object
#'
#' @param data Either a file path to an SVG file or the SVG code itself as a
#'   character vector of length 1.
#' @inheritParams SVG
#'
#' @return An `svg` object.
#'
#' @export
SVG_import <- function(data = NULL,
                       width = NULL,
                       height = NULL,
                       viewbox = NULL,
                       title = NULL,
                       desc = NULL,
                       incl_xmlns = FALSE,
                       oneline = FALSE,
                       anim_iterations = "infinite") {

  if (grepl(".svg$", data)) {
    svg_text <- readLines(data, warn = FALSE)
  } else {
    svg_text <- data
  }

  # Strip the <?xml> tag if present
  if (grepl("<\\?xml", svg_text[1])) {
    svg_text <- svg_text[-1]
  }

  # Collapse text into a single string
  svg_text <- paste(svg_text, collapse = "\n")

  svg_list <- xml2::read_xml(svg_text) %>% xml2::as_list()

  svg_attrs_list <- svg_list$svg %>% attributes()

  svg_obj <-
    SVG(
      width = width %||% svg_attrs_list$width,
      height = height %||% svg_attrs_list$height,
      viewbox = viewbox %||% svg_attrs_list$viewBox,
      title = title,
      desc = desc,
      incl_xmlns = incl_xmlns,
      oneline = oneline,
      anim_iterations = anim_iterations
    )

  for (i in seq(length(svg_list$svg))) {

    type <- svg_list$svg[i] %>% attributes() %>% .[[1]]

    if (type == "path") {
      path <- svg_list$svg[[i]] %>% attributes() %>% .$d
      svg_obj <- svg_obj %>% svg_path(path = path)
    }
  }

  svg_obj
}

#' Create an svg object with a Line Awesome glyph
#'
#' @param name The name of the Line Awesome glyph.
#' @inheritParams SVG
#'
#' @return An `svg` object.
#'
#' @export
SVG_la <- function(name = "500px",
                   height = "0.75em",
                   width = NULL,
                   viewbox = NULL,
                   title = NULL,
                   desc = NULL,
                   incl_xmlns = FALSE,
                   anim_iterations = "infinite") {

  tbl_filter <- la_svg_tbl %>% dplyr::filter(icon_name == !!name)

  if (nrow(tbl_filter) == 0) {
    stop("The name provided (`", name, "`) isn't available", call. = FALSE)
  }

  SVG_import(
    data = tbl_filter[[1, "svg_text"]],
    width = width,
    height = height,
    viewbox = viewbox,
    title = title,
    desc = desc,
    incl_xmlns = incl_xmlns,
    oneline = TRUE,
    anim_iterations = anim_iterations
  )
}
