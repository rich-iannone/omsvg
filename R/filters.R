#' Build an SVG `<filter>`
#'
#' The `svg_filter()` let's us create a named `<filter>` element that we can
#' apply to any SVG elements (such as shapes). We can bundle one or more filter
#' elements by supplying a list of `filter_*()` calls to the `filters` argument.
#'
#' @inheritParams svg_rect
#' @param id The ID value to assign to the filter. This must be provided and it
#'   should be unique among all `<filter>` elements.
#' @param width,height The lengths of `width` and `height` define the extent of
#'   the filter.
#' @param filters A list of `filter_*()` function calls. Examples include
#'   [filter_image()] and [filter_gaussian_blur()].
#'
#' @examples
#' # Set up an `svg_filter()` (called
#' # `"blur"`) that has the blur effect
#' # (using the `filter_gaussian_blur()`
#' # function); have the ellipse element
#' # use the filter by referencing it
#' # by name via the `"filter"` attribute
#' SVG(width = 200, height = 100) %>%
#'   svg_filter(
#'     id = "blur",
#'     filters = list(
#'       filter_gaussian_blur(stdev = 2)
#'     )
#'   ) %>%
#'   svg_ellipse(
#'     x = 40, y = 40,
#'     width = 50, height = 30,
#'     attrs = attrs_pres(
#'       fill = "green",
#'       filter = "blur"
#'     )
#'   )
#'
#' @export
svg_filter <- function(svg,
                       id,
                       width = NULL,
                       height = NULL,
                       filters = list()) {

  attrs <- c()

  attrs <- c(attrs, build_attr(name = "id", value = id))

  if (!is.null(width)) {
    attrs <- c(attrs, build_attr(name = "width", value = width))
  }

  if (!is.null(height)) {
    attrs <- c(attrs, build_attr(name = "height", value = height))
  }

  attr_str <-
    paste(
      attrs,
      build_attr("width", svg$width),
      build_attr("height", svg$height),
      collapse = " "
    )

  inner_tags <-
    filters %>%
    lapply(FUN = function(x) {



      if (inherits(x, "filter_effects")) {
        build_tag(name = names(x), attrs = unname(x))
      }
    }
    ) %>%
    unlist() %>%
    paste(collapse = "\n")

  if (inner_tags == "") {
    return(svg)
  }

  filter_tag <-
    build_tag(
      name = "filter",
      attrs = attr_str,
      inner = paste0("\n", inner_tags, "\n")
    )

  add_defs_list(
    svg = svg,
    defs_list = filter_tag
  )
}

#' Filter: display an image
#'
#' Display an image using a URL or a relative path to an on-disk resource.
#'
#' @param image A link or path to an image resource.
#'
#' @examples
#' # Place an image (obtained via an image
#' # link) within a rectangle element using
#' # the `filter_image()` filter
#' SVG(width = 500, height = 500) %>%
#'   svg_filter(
#'     id = "image",
#'     filters = list(
#'       filter_image(
#'         image = "https://www.r-project.org/logo/Rlogo.png"
#'       )
#'     )
#'   ) %>%
#'   svg_rect(
#'     x = 25, y = 25,
#'     width = "50%", height = "50%",
#'     attrs = attrs_pres(filter = "image")
#'   )
#'
#' @export
filter_image <- function(image) {

  filter_spec <- c(feImage = paste0("xlink:href=\"", image, "\""))

  class(filter_spec) <- "filter_effects"

  filter_spec
}

#' Filter: add a gaussian blur to an element
#'
#' A gaussian blur effectively blurs an input image or shape by the amount
#' specified in `stdev`. The standard deviation of `stdev` is in direct
#' reference to the gaussian distribution that governs the extent of blurring.
#'
#' @param stdev The number of standard deviations for the blur effect.
#' @param what What exactly should be blurred? By default, it is the `"source"`
#'   image.
#'
#' @examples
#' # Add a green ellipse to an SVG and
#' # then apply the `filter_gaussian_blur()`
#' # filter to blur the edges
#' SVG(width = 200, height = 100) %>%
#'   svg_filter(
#'     id = "blur",
#'     filters = list(
#'       filter_gaussian_blur(stdev = 2)
#'     )
#'   ) %>%
#'   svg_ellipse(
#'     x = 40, y = 40,
#'     width = 50, height = 30,
#'     attrs = attrs_pres(
#'       fill = "green",
#'       filter = "blur"
#'     )
#'   )
#'
#' @export
filter_gaussian_blur <- function(stdev = 1,
                                 what = "source") {

  if (what == "source") {
    what <- "SourceGraphic"
  }


  filter_spec <-
    c(
      feGaussianBlur = paste(
        build_attr("in", what),
        build_attr("stdDeviation", stdev),
        collapse = " "
      )
    )

  class(filter_spec) <- "filter_effects"

  filter_spec
}

#' Filter: add an erosion effect to an element
#'
#' The `filter_erode()` filter effectively thins out a source graphic by a given
#' `radius` value. The higher the `radius`, the greater the extent of thinning.
#'
#' @param radius The extent to which the source graphic will be eroded. If a
#'   vector of two values are provided, the first value represents the x-radius
#'   and the second one the y-radius. If one value is provided, then that value
#'   is used for both x and y.
#'
#' @examples
#' # Add a text element to an
#' # SVG drawing and erode it with
#' # the `filter_erode()` filter
#' SVG(width = 200, height = 100) %>%
#'   svg_filter(
#'     id = "erode",
#'     filters = list(
#'       filter_erode(radius = c(1, 0))
#'     )
#'   ) %>%
#'   svg_text(
#'     x = 10, y = 40,
#'     text = "Erosion",
#'     attrs = attrs_pres(
#'       font_size = "3em",
#'       font_weight = "bolder",
#'       filter = "erode"
#'     )
#'   )
#' @export
filter_erode <- function(radius = 1) {

  radius <- radius %>% paste(collapse = " ")

  filter_spec <-
    c(
      feMorphology = paste(
        build_attr("operator", "erode"),
        build_attr("radius", radius),
        collapse = " "
      )
    )

  class(filter_spec) <- "filter_effects"

  filter_spec
}

#' Filter: add a dilation effect to an element
#'
#' The `filter_dilate()` filter applies a dilation effect to a source graphic by
#' a given `radius` value. The higher the `radius`, the greater the dilation
#' potential.
#'
#' @param radius The extent to which the source graphic will be dilated. If a
#'   vector of two values are provided, the first value represents the x-radius
#'   and the second one the y-radius. If one value is provided, then that value
#'   is used for both x and y.
#'
#' @examples
#' # Add a text element to an
#' # SVG drawing and erode it with
#' # the `filter_dilate()` filter
#' SVG(width = 200, height = 100) %>%
#'   svg_filter(
#'     id = "dilate",
#'     filters = list(
#'       filter_dilate(radius = c(0, 1))
#'     )
#'   ) %>%
#'   svg_text(
#'     x = 10, y = 40,
#'     text = "Dilation",
#'     attrs = attrs_pres(
#'       font_size = "3em",
#'       filter = "dilate"
#'     )
#'   )
#' @export
filter_dilate <- function(radius = 1) {

  radius <- radius %>% paste(collapse = " ")

  filter_spec <-
    c(
      feMorphology = paste(
        build_attr("operator", "dilate"),
        build_attr("radius", radius),
        collapse = " "
      )
    )

  class(filter_spec) <- "filter_effects"

  filter_spec
}

#' Filter: add a drop shadow to an element
#'
#' With the `filter_drop_shadow()` drop shadow appears beneath the input image
#' or shape and its offset is controlled by `dx` and `dy`. The blurring of the
#' drop shadow is set by the `stdev` value.
#'
#' @param dx,dy The offset of the drop shadow compared to the position of the
#'   input image or shape.
#' @param stdev The number of standard deviations for the blur effect.
#' @param color The color of the drop shadow.
#' @param opacity The opacity of the drop shadow. We can use a real number from
#'   `0` to `1` or a value in percentage units.
#'
#' @examples
#' # Apply a drop shadow filter on a
#' # text element (orange in color,
#' # and semi-opaque)
#' SVG(width = 250, height = 100) %>%
#'   svg_filter(
#'     id = "shadow",
#'     filters = list(
#'       filter_drop_shadow(
#'         dx = 1, dy = 2,
#'         color = "orange",
#'         opacity = 0.5
#'       )
#'     )
#'   ) %>%
#'   svg_text(
#'     x = 10, y = 40,
#'     text = "Shadowed",
#'     attrs = attrs_pres(
#'       font_size = "2em",
#'       fill = "#555555",
#'       font_weight = "bolder",
#'       filter = "shadow"
#'     )
#'   )
#' @export
filter_drop_shadow <- function(dx = 0.2,
                               dy = 0.2,
                               stdev = 1,
                               color = "black",
                               opacity = 1) {

  filter_spec <-
    c(
      feDropShadow = paste(
        build_attr("dx", dx),
        build_attr("dy", dy),
        build_attr("stdDeviation", stdev),
        build_attr("flood-color", color),
        build_attr("flood-opacity", opacity),
        collapse = " "
      )
    )

  class(filter_spec) <- "filter_effects"

  filter_spec
}

#' Filter: offset an element a specified amount
#'
#' The offset filter applies an offset in the x and y directions to an existing
#' element. The offset is handled by setting values for `dx` and `dy`.
#'
#' @param dx,dy The offset of the element position compared to its initial
#'   position.
#' @param what What exactly should be offset? By default, it is the `"source"`
#'   image.
#'
#' @examples
#' # Add a circle element to an
#' # SVG drawing and offset it
#' # by 10px to the right
#' SVG(width = 150, height = 150) %>%
#'   svg_filter(
#'     id = "offset_right",
#'     filters = list(
#'       filter_offset(dx = 10, dy = 0)
#'     )
#'   ) %>%
#'   svg_circle(
#'     x = 30, y = 30,
#'     diameter = 40,
#'     attrs = attrs_pres(
#'       fill = "red",
#'       filter = "offset_right"
#'     )
#'   )
#'
#' @export
filter_offset <- function(dx = NULL,
                          dy = NULL,
                          what = "source") {

  if (what == "source") {
    what <- "SourceGraphic"
  }

  dx <- dx %||% 0
  dy <- dy %||% 0

  filter_spec <-
    c(
      feOffset = paste(
        build_attr("in", what),
        build_attr("dx", dx),
        build_attr("dy", dy),
        collapse = " "
      )
    )

  class(filter_spec) <- "filter_effects"

  filter_spec
}
