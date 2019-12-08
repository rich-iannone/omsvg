#' Addition of a `rect` element
#'
#' The `svg_rect()` function adds a rectangle to an `svg` object. The position
#' of the rectangle is given by `x` and `y`, and this refers to the upper left
#' point of the rectangle. The `width` and the `height` are the dimensions of
#' the rectangle. All of these dimensions are in units of `px`. The optional
#' `rx` and `ry` parameter are corner radius values (again, in `px` units) that
#' define x and y radius of the corners of the rectangle.
#'
#' @param svg The `svg` object that is created using the [SVG()] function.
#' @param x,y The `x` and `y` positions of the upper left point of the rectangle
#'   to be drawn. The `x` and `y` values are relative to upper left of the SVG
#'   drawing area.
#' @param width,height The `width` and `height` of the element that is to be
#'   drawn. The `width` is the distance in the 'x' direction from point `x`
#'   (proceeding right) and the `height` is the distance in the 'y' direction
#'   from point `y` (proceeding downward).
#' @param rx,ry Optional corner radius values in the 'x' and 'y' directions.
#'   Applies to all corners of the rectangle. If only one value is provided
#'   (say, just for `rx`) then the unset value will take that set value as well.
#' @param stroke The color of the stroke applied to the element (i.e., the
#'   outline).
#' @param stroke_width The width of the stroke in units of pixels.
#' @param fill The fill color of the element.
#' @param opacity The opacity of the element. Must be a value in the
#'   range of `0` to `1`.
#' @param attrs A presentation attribute list. The helper function
#'   [svg_attrs_pres()] can help us easily generate this named list object. For the
#'   most part, the list's names are the presentation attribute names and the
#'   corresponding values are the matching attribute values.
#' @param anims An animation directive list for the element. This should be
#'   structured using the [anims()] function.
#' @param filters A filter directive list for the element. This is easily
#'   created by using a list of `filter_*()` functions (e.g.,
#'   `list(filter_gaussian_blur(2), filter_drop_shadow(2, 2))`).
#' @param id An optional ID value to give to the built tag. This is useful for
#'   modifying this element in a later function call or for interacting with
#'   CSS.
#'
#' @examples
#' # Create an SVG with a single
#' # rectangle element
#' svg_1 <-
#'   SVG(width = 100, height = 100) %>%
#'     svg_rect(
#'       x = 20, y = 10,
#'       width = 40, height = 15,
#'       stroke = "blue", fill = "yellow"
#'     )
#'
#' # Create an SVG with a single
#' # rectangle element that moves
#' # to new `x` positions
#' svg_2 <-
#'   SVG(width = 300, height = 300) %>%
#'     svg_rect(
#'       x = 50, y = 50,
#'       width = 50, height = 50,
#'       stroke = "magenta", fill = "lightblue",
#'       anims = anims(
#'         0.5 ~ list(
#'           anim_position(
#'             x = 50, y = 50,
#'             easing_fn = ease_out()
#'           ),
#'           anim_rotation(rotation = 0)
#'         ),
#'         2.0 ~ list(
#'           anim_position(
#'             x = 200, y = 50,
#'             easing_fn = ease_in_out()
#'           ),
#'           anim_rotation(rotation = 90)
#'         )
#'       )
#'     )
#'
#' @export
svg_rect <- function(svg,
                     x,
                     y,
                     width,
                     height,
                     rx = NULL,
                     ry = NULL,
                     stroke = NULL,
                     stroke_width = NULL,
                     fill = NULL,
                     opacity = NULL,
                     attrs = list(),
                     anims = list(),
                     filters = list(),
                     id = NULL) {

  # Develop the `start` list and normalize it
  # against any `attrs` defined
  start <-
    list(
      x_i = x,
      y_i = y,
      width_i = width,
      height_i = height,
      rx_i = rx,
      ry_i = ry,
      stroke_i = stroke,
      stroke_width_i = stroke_width,
      fill_i = fill,
      opacity_i = opacity
    ) %>%
    normalize_start_list(attrs = attrs)

  # Develop the `element` list and normalize it
  # against any `attrs` defined
  element <-
    list(
      type = "rect",
      x = x,
      y = y,
      width = width,
      height = height,
      rx = rx,
      ry = ry,
      stroke = stroke,
      stroke_width = stroke_width,
      fill = fill,
      opacity = opacity,
      attrs = attrs,
      anims = anims,
      filters = filters,
      start = start,
      tag = NA_character_
    ) %>%
    normalize_element_list(attrs = attrs)

  # Add the `element` list to the `svg` object
  svg %>%
    add_element_list(
      element_list = element,
      id = id
    )
}

#' Addition of a `circle` element
#'
#' The `svg_circle()` function adds a circle to an `svg` object. The position of
#' the circle is given by `x` and `y`, and this refers to the center point of
#' the point of the circle. The `diameter` of the circle is given in units of
#' `px`.
#'
#' @param x,y The `x` and `y` positions of the center of the circle to be drawn.
#'   The `x` and `y` values are relative to upper left of the SVG drawing area.
#' @param diameter The diameter of the circle shape in units of `px`.
#' @inheritParams svg_rect
#'
#' @examples
#' # Create an SVG with a single
#' # circle element
#' svg <-
#'   SVG(width = 80, height = 80) %>%
#'     svg_circle(
#'       x = 30, y = 30,
#'       diameter = 40,
#'       stroke = "magenta",
#'       fill = "olive"
#'     )
#'
#' @export
svg_circle <- function(svg,
                       x,
                       y,
                       diameter,
                       stroke = NULL,
                       stroke_width = NULL,
                       fill = NULL,
                       opacity = NULL,
                       attrs = list(),
                       anims = list(),
                       filters = list(),
                       id = NULL) {

  radius <- diameter / 2

  # Develop the `start` list and normalize it
  # against any `attrs` defined
  start <-
    list(
      x_i = x,
      y_i = y,
      width_i = diameter,
      height_i = diameter,
      stroke_i = stroke,
      stroke_width_i = stroke_width,
      fill_i = fill,
      opacity_i = opacity
    ) %>%
    normalize_start_list(attrs = attrs)

  # Develop the `element` list and normalize it
  # against any `attrs` defined
  element <-
    list(
      type = "circle",
      cx = x,
      cy = y,
      r = radius,
      stroke = stroke,
      stroke_width = stroke_width,
      fill = fill,
      opacity = opacity,
      attrs = attrs,
      anims = anims,
      filters = filters,
      start = start,
      tag = NA_character_
    ) %>%
    normalize_element_list(attrs = attrs)

  # Add the `element` list to the `svg` object
  svg %>%
    add_element_list(
      element_list = element,
      id = id
    )
}

#' Addition of an `ellipse` element
#'
#' The `svg_ellipse()` function adds an ellipse to an `svg` object. The position
#' of the ellipse is given by `x` and `y`, and they refer to the center point of
#' the point of the ellipse. The `width` and the `height`, both in units of
#' `px`, provide the horizontal and vertical extents of the ellipse.
#'
#' @param x,y The `x` and `y` positions of the center of the ellipse to be
#'   drawn. The `x` and `y` values are relative to upper left of the SVG drawing
#'   area.
#' @param width,height The `width` and `height` of the ellipse that is to be
#'   drawn. The `width` is the overall width of the ellipse in the 'x'
#'   direction, centered on point `x`. The `height` is the distance in the 'y'
#'   direction, centered on point `y`.
#' @inheritParams svg_rect
#'
#' @examples
#' # Create an SVG with a single
#' # ellipse element
#' svg <-
#'   SVG(width = 60, height = 60) %>%
#'     svg_ellipse(
#'       x = 30, y = 30,
#'       width = 50, height = 30,
#'       fill = "purple"
#'     )
#'
#' @export
svg_ellipse <- function(svg,
                        x,
                        y,
                        width,
                        height,
                        stroke = NULL,
                        stroke_width = NULL,
                        fill = NULL,
                        opacity = NULL,
                        attrs = list(),
                        anims = list(),
                        filters = list(),
                        id = NULL) {

  rx <- width / 2
  ry <- height / 2

  # Develop the `start` list and normalize it
  # against any `attrs` defined
  start <-
    list(
      x_i = x,
      y_i = y,
      width_i = width,
      height_i = height,
      stroke_i = stroke,
      stroke_width_i = stroke_width,
      fill_i = fill,
      opacity_i = opacity
    ) %>%
    normalize_start_list(attrs = attrs)

  # Develop the `element` list and normalize it
  # against any `attrs` defined
  element <-
    list(
      type = "ellipse",
      cx = x,
      cy = y,
      rx = rx,
      ry = ry,
      stroke = stroke,
      stroke_width = stroke_width,
      fill = fill,
      opacity = opacity,
      attrs = attrs,
      anims = anims,
      filters = filters,
      start = start,
      tag = NA_character_
    ) %>%
    normalize_element_list(attrs = attrs)

  # Add the `element` list to the `svg` object
  svg %>%
    add_element_list(
      element_list = element,
      id = id
    )
}

#' Addition of an `line` element
#'
#' The `svg_line()` function adds a line to an `svg` object. The line is drawn
#' using a start point (`x1` and `y1`) and an end point (`x2` and `y2`) points.
#' These positions are in units of `px`.
#'
#' @param x1,y1 The `x` and `y` positions of the line's start point.
#' @param x2,y2 The `x` and `y` positions of the line's end point.
#' @inheritParams svg_rect
#'
#' @examples
#' # Create an SVG with a single
#' # line element
#' svg <-
#'   SVG(width = 100, height = 50) %>%
#'     svg_line(
#'       x1 = 5, y1 = 5,
#'       x2 = 95, y2 = 45,
#'       stroke = "blue"
#'     )
#'
#' @export
svg_line <- function(svg,
                     x1,
                     y1,
                     x2,
                     y2,
                     stroke = NULL,
                     stroke_width = NULL,
                     opacity = NULL,
                     attrs = list(),
                     anims = list(),
                     filters = list(),
                     id = NULL) {

  width <- abs(x1 - x2)
  height <- abs(y1 - y2)

  # Develop the `start` list and normalize it
  # against any `attrs` defined
  start <-
    list(
      x_i = x1,
      y_i = y1,
      width_i = width,
      height_i = height,
      stroke_i = stroke,
      stroke_width_i = stroke_width,
      opacity_i = opacity
    ) %>%
    normalize_start_list(attrs = attrs)

  # Develop the `element` list and normalize it
  # against any `attrs` defined
  element <-
    list(
      type = "line",
      x1 = x1,
      x2 = x2,
      y1 = y1,
      y2 = y2,
      stroke = stroke,
      stroke_width = stroke_width,
      opacity = opacity,
      attrs = attrs,
      anims = anims,
      filters = filters,
      start = start,
      tag = NA_character_
    ) %>%
    normalize_element_list(attrs = attrs)

  # Add the `element` list to the `svg` object
  svg %>%
    add_element_list(
      element_list = element,
      id = id
    )
}

#' Addition of an `polyline` element
#'
#' The `svg_polyline()` function adds a polyline to an `svg` object. The
#' polyline is drawn by connecting a series of points with straight lines. The
#' points can be provided as a vector that's exactly divisible by two, or, as a
#' formatted string that adheres to the specification of the `points` attribute
#' of the SVG `<polyline>` tag. All point positions are in units of `px`.
#'
#' @param points A numeric vector of points (with alternating values for `x` and
#'   `y` positions) that define the polyline. This can also be a single-length
#'   character vector that holds the formatted points string (space-separated
#'   `x` and `y` values, and comma-separated points).
#' @inheritParams svg_rect
#'
#' @examples
#' # Create an SVG with a single
#' # polyline element; here `points`
#' # is a numeric vector where pairs
#' # of values are the `x` and `y`
#' # point position
#' svg_1 <-
#'   SVG(width = 300, height = 300) %>%
#'     svg_polyline(
#'       points = c(
#'         10, 10, 15, 20, 20, 15, 25, 30, 30, 25,
#'         35, 40, 40, 35, 45, 50, 50, 45
#'       ),
#'       stroke = "blue"
#'     )
#'
#' # Create the same SVG with a single
#' # polyline element; this time `points`
#' # is a formatted points string
#' svg_2 <-
#'   SVG(width = 300, height = 300) %>%
#'     svg_polyline(
#'       points =
#'         "10,10 15,20 20,15 25,30 30,25 35,40 40,35 45,50 50,45",
#'       stroke = "blue"
#'     )
#'
#' @export
svg_polyline <- function(svg,
                         points,
                         stroke = NULL,
                         stroke_width = NULL,
                         fill = NULL,
                         opacity = NULL,
                         attrs = list(),
                         anims = list(),
                         filters = list(),
                         id = NULL) {

  if (inherits(points, "numeric")) {
    mat <- matrix(points, nrow = 2) %>% t()
    points <- paste0(mat[, 1], ",", mat[, 2], collapse = " ")
  }

  points_str_attrs <- get_points_str_attrs(points)

  # Develop the `start` list and normalize it
  # against any `attrs` defined
  start <-
    list(
      x_i = points_str_attrs$ul[1],
      y_i = points_str_attrs$ul[2],
      width_i = points_str_attrs$width,
      height_i =points_str_attrs$height,
      stroke_i = stroke,
      stroke_width_i = stroke_width,
      fill_i = fill,
      opacity_i = opacity
    ) %>%
    normalize_start_list(attrs = attrs)

  # Develop the `element` list and normalize it
  # against any `attrs` defined
  element <-
    list(
      type = "polyline",
      points = points,
      stroke = stroke,
      stroke_width = stroke_width,
      fill = fill,
      opacity = opacity,
      attrs = attrs,
      anims = anims,
      filters = filters,
      start = start,
      tag = NA_character_
    ) %>%
    normalize_element_list(attrs = attrs)

  # Add the `element` list to the `svg` object
  svg %>%
    add_element_list(
      element_list = element,
      id = id
    )
}

#' Addition of an `polygon` element
#'
#' The `svg_polygon()` function adds a polygon to an `svg` object. In the
#' context of an SVG shape a polygon is similar to a polyline (defined by a
#' series of points) except that the path will be automatically closed (i.e.,
#' last point connects to the first point). Like a polyline, a polygon is drawn
#' by connecting a series of points with straight lines. The points can be
#' provided as a vector that's exactly divisible by two, or, as a formatted
#' string that adheres to the specification of the `points` attribute of the SVG
#' `<polygon>` tag. All point positions are in units of `px`.
#'
#' @param points A numeric vector of points (with alternating values for `x` and
#'   `y` positions) that define the polygon. This can also be a single-length
#'   character vector that holds the formatted points string (space-separated
#'   `x` and `y` values, and comma-separated points).
#' @inheritParams svg_rect
#'
#' @examples
#' # Create an SVG with a single
#' # polygon element
#' svg <-
#'   SVG(width = 300, height = 300) %>%
#'     svg_polygon(
#'       points = "100,10 40,198 190,78 10,78 160,198",
#'       stroke = "orange",
#'       stroke_width = 4,
#'       fill = "yellow"
#'     )
#'
#' @export
svg_polygon <- function(svg,
                        points,
                        stroke = NULL,
                        stroke_width = NULL,
                        fill = NULL,
                        opacity = NULL,
                        attrs = list(),
                        anims = list(),
                        filters = list(),
                        id = NULL) {

  if (inherits(points, "numeric")) {
    mat <- matrix(points, ncol = 2)
    points <- paste(mat[, 1], mat[, 2], collapse = ", ")
  }

  points_str_attrs <- get_points_str_attrs(points)

  # Develop the `start` list and normalize it
  # against any `attrs` defined
  start <-
    list(
      x_i = points_str_attrs$ul[1],
      y_i = points_str_attrs$ul[2],
      width_i = points_str_attrs$width,
      height_i =points_str_attrs$height,
      stroke_i = stroke,
      stroke_width_i = stroke_width,
      fill_i = fill,
      opacity_i = opacity
    ) %>%
    normalize_start_list(attrs = attrs)

  # Develop the `element` list and normalize it
  # against any `attrs` defined
  element <-
    list(
      type = "polygon",
      points = points,
      stroke = stroke,
      stroke_width = stroke_width,
      fill = fill,
      opacity = opacity,
      attrs = attrs,
      anims = anims,
      filters = filters,
      start = start,
      tag = NA_character_
    ) %>%
    normalize_element_list(attrs = attrs)

  # Add the `element` list to the `svg` object
  svg %>%
    add_element_list(
      element_list = element,
      id = id
    )
}

#' Addition of an `path` element
#'
#' The `svg_path()` function adds a path to an `svg` object. A path can
#' potentially be quite complex (with an interplay of line and curve commands),
#' so, a hand-encoded `path` string is not often done by hand. For this reason,
#' the `path` argument accepts only a formatted string that complies with the
#' input requirements for the `d` attribute of the SVG `<path>` tag. All point
#' positions are in units of `px`.
#'
#' @param path A single-length character vector that holds the formatted path
#'   string.
#' @inheritParams svg_rect
#'
#' @examples
#' # Create an SVG with a single
#' # path element
#' svg <-
#'   SVG(width = 300, height = 300) %>%
#'     svg_path(
#'       path = "M 50 160 q 100 -300 200 0",
#'       stroke = "magenta",
#'       stroke_width = 5,
#'       fill = "lightblue"
#'     )
#'
#' @export
svg_path <- function(svg,
                     path,
                     stroke = NULL,
                     stroke_width = NULL,
                     fill = NULL,
                     opacity = NULL,
                     attrs = list(),
                     anims = list(),
                     filters = list(),
                     id = NULL) {

  # TODO: determine the upper-left position of a path
  # TODO: determine the width and height of a path

  # Develop the `start` list and normalize it
  # against any `attrs` defined
  start <-
    list(
      x_i = 0,
      y_i = 0,
      width_i = 0,
      height_i = 0,
      stroke_i = stroke,
      stroke_width_i = stroke_width,
      fill_i = fill,
      opacity_i = opacity
    ) %>%
    normalize_start_list(attrs = attrs)

  # Develop the `element` list and normalize it
  # against any `attrs` defined
  element <-
    list(
      type = "path",
      d = path,
      stroke = stroke,
      stroke_width = stroke_width,
      fill = fill,
      opacity = opacity,
      attrs = attrs,
      anims = anims,
      filters = filters,
      start = start,
      tag = NA_character_
    ) %>%
    normalize_element_list(attrs = attrs)

  # Add the `element` list to the `svg` object
  svg %>%
    add_element_list(
      element_list = element,
      id = id
    )
}

#' Addition of an `text` element
#'
#' The `svg_text()` function adds text to an `svg` object. As with many of the
#' functions that create shape elements (such as [svg_rect()]), the starting
#' position is defined by `x` and `y` values. All point positions are in units
#' of `px`.
#'
#' @param x,y The `x` and `y` positions of the upper left of the text to be
#'   drawn. The `x` and `y` values are relative to upper left of the SVG drawing
#'   area itself.
#' @param text A character vector that contains the text to be rendered.
#' @param fill The color of the text.
#' @inheritParams svg_rect
#' @inheritParams svg_path
#'
#' @examples
#' # Create an SVG with a single
#' # text element
#' svg <-
#'   SVG(width = 300, height = 300) %>%
#'     svg_text(
#'       x = 10, y = 20,
#'       text = "A line of text"
#'     )
#'
#' @export
svg_text <- function(svg,
                     x,
                     y,
                     text,
                     fill = NULL,
                     opacity = NULL,
                     path = NULL,
                     attrs = list(),
                     anims = list(),
                     filters = list(),
                     id = NULL) {

  # Develop the `start` list and normalize it
  # against any `attrs` defined
  start <-
    list(
      x_i = x,
      y_i = y,
      width_i = 0,
      height_i = 0,
      fill_i = fill,
      opacity_i = opacity
    ) %>%
    normalize_start_list(attrs = attrs)

  # Develop the `element` list and normalize it
  # against any `attrs` defined
  element <-
    list(
      type = "text",
      x = x,
      y = y,
      text = text,
      path = path,
      fill = fill,
      opacity = opacity,
      attrs = attrs,
      anims = anims,
      filters = filters,
      start = start,
      tag = NA_character_
    ) %>%
    normalize_element_list(attrs = attrs)

  # Add the `element` list to the `svg` object
  svg %>%
    add_element_list(
      element_list = element,
      id = id
    )
}

#' Addition of an `image` element
#'
#' The `svg_image()` function adds an image to an `svg` object. The starting
#' position is defined by `x` and `y` values. The image `width` and `height` are
#' also required. All of these attributes are expressed in units of `px`.
#'
#' @param x,y The `x` and `y` positions of the upper left of the image to be
#'   included. The `x` and `y` values are relative to upper left of the SVG
#'   drawing area itself.
#' @param image The URL for the image file.
#' @param width,height The width and height of the rectangle in which the image
#'   will be placed. If both are not provided, the image's original dimensions
#'   will be used. If one of these is provided, then the image will be scaled to
#'   the provided value with the aspect ratio intact. Providing both will result
#'   in the image placed in center of the rectangle with the aspect ratio
#'   preserved.
#' @param preserve_aspect_ratio Controls how the aspect ratio of the image is
#'   preserved. Use `"none"` if the image's original aspect ratio should not be
#'   respected; this will fill the rectangle defined by `width` and `height`
#'   with the image (and this is only if both values are provided).
#' @inheritParams svg_rect
#'
#' @examples
#' # Create an SVG with an SVG image
#' # (the R logo) contained within it
#' svg <-
#'   SVG(width = 300, height = 300) %>%
#'     svg_image(
#'       x = 20, y = 20,
#'       width = 100,
#'       height = 100,
#'       image = "https://www.r-project.org/logo/Rlogo.svg"
#'     )
#'
#' @export
svg_image <- function(svg,
                      x,
                      y,
                      image,
                      width = NULL,
                      height = NULL,
                      preserve_aspect_ratio = NULL,
                      opacity = NULL,
                      attrs = list(),
                      anims = list(),
                      filters = list(),
                      id = NULL) {

  # Develop the `start` list and normalize it
  # against any `attrs` defined
  start <-
    list(
      x_i = x,
      y_i = y,
      width_i = width,
      height_i = height
    ) %>%
    normalize_start_list(attrs = attrs)

  # Develop the `element` list and normalize it
  # against any `attrs` defined
  element <-
    list(
      type = "image",
      x = x,
      y = y,
      width = width,
      height = height,
      href = image,
      preserveAspectRatio = preserve_aspect_ratio,
      opacity = opacity,
      attrs = attrs,
      anims = anims,
      filters = filters,
      start = start,
      tag = NA_character_
    ) %>%
    normalize_element_list(attrs = attrs)

  # Add the `element` list to the `svg` object
  svg %>%
    add_element_list(
      element_list = element,
      id = id
    )
}

#' Addition of a group element
#'
#' The `svg_group()` function allows for grouping of several SVG elements. This
#' is useful if we'd like to pass presentation attributes to several elements
#' at once.
#'
#' @param ... a collection of named arguments that consist of presentation
#' attributes (e.g., `stroke = "blue"`) and formulas that represent elements
#' (e.g, `~ svg_rect(., x = 60, y = 60, width = 50, height = 50)`).
#' @param .list Allows for the use of a list as an input alternative to `...`.
#' @inheritParams svg_rect
#'
#' @examples
#' # Create an SVG with two rectangles
#' # contained within a group
#' SVG(width = 300, height = 300) %>%
#'   svg_group(
#'     fill = "steelblue", stroke = "red", opacity = 0.5,
#'     ~ svg_rect(., x = 20, y = 20, width = 50, height = 50),
#'     ~ svg_rect(., x = 40, y = 40, width = 50, height = 50, fill = "red")
#'   )
#'
#' # Create an SVG with two rectangles
#' # that are nested within two
#' # different groups
#' SVG(width = 300, height = 300) %>%
#'   svg_group(
#'     fill = "green", stroke = "red",
#'     ~ svg_rect(., x = 30, y = 30, width = 40, height = 50),
#'     ~ svg_group(.,
#'       fill = "steelblue", opacity = 0.5,
#'       ~ svg_rect(., x = 60, y = 60, width = 50, height = 50)
#'       )
#'     )
#'
#' @import rlang
#' @export
svg_group <- function(svg,
                      ...,
                      .list = list2(...),
                      attrs = list(),
                      anims = list(),
                      filters = list(),
                      id = NULL) {

  # Obtain all of the group's elements
  list_elements <- .list

  dots_attrs <- list_elements[rlang::names2(list_elements) != ""]

  group_elements <-
    list_elements[
      vapply(
        list_elements,
        function(x) rlang::is_formula(x),
        FUN.VALUE = logical(1),
        USE.NAMES = FALSE
      )
    ]

  # Develop the `element` list and normalize it
  # against any `attrs` defined
  element <-
    c(
      list(type = "g"),
      dots_attrs,
      list(
        attrs = attrs,
        anims = anims,
        filters = filters,
        start = NA_character_,
        tag = NA_character_
      )
    ) %>%
    normalize_element_list(attrs = attrs)

  # Add the `element` list to the `svg` object
  svg <-
    svg %>%
    add_element_list(
      element_list = element,
      id = id
    )

  for (g_element in group_elements) {

    svg <-
      eval(
        expr = parse(
          text =
            g_element %>%
            rlang::f_rhs() %>%
            rlang::expr_deparse() %>% tidy_gsub("(.", "(svg", fixed = TRUE)
        ),
        envir = NULL
      )
  }

  # Develop another `element` list and normalize it
  # against any `attrs` defined
  element <-
    list(
      type = "g",
      attrs = attrs,
      anims = anims,
      filters = filters,
      start = NA_character_,
      tag = "</g>"
    ) %>%
    normalize_element_list(attrs = attrs)

  # Add the `element` list to the `svg` object
  svg <-
    svg %>%
    add_element_list(
      element_list = element,
      id = "::closing_tag::"
    )

  svg
}

shape_types <- function() {
  c("rect", "circle", "ellipse", "line", "polyline", "polygon", "path", "image")
}

add_element_list <- function(svg,
                             element_list,
                             id) {

  svg_element_count <- svg$elements %>% length()

  svg$elements <- c(svg$elements, list(element_list))

  if (!is.null(id)) {
    names(svg[["elements"]])[svg_element_count + 1] <- id
  } else {
    names(svg[["elements"]])[svg_element_count + 1] <- ":no_id:"
  }

  svg
}

add_defs_list <- function(svg, defs_list) {

  svg_defs_count <- svg$defs %>% length()

  svg$defs <- c(svg$defs, list(defs_list))

  svg
}

normalize_start_list <- function(start, attrs) {

  # Are there assigned values in `attrs` also in `start`
  if (
    !(
      names(start) %>%
      tidy_gsub("_i$", "") %in% (names(attrs) %>% tidy_gsub("-", "_", fixed = TRUE))
    ) %>%
    any()
  ) {
    return(start)
  }

  attrs_names <- names(attrs) %>% tidy_gsub("-", "_", fixed = TRUE)
  names_start <- names(start) %>% tidy_gsub("_i$", "")

  for (name in attrs_names) {
    if (name %in% names_start) {

      start[[which(names_start %in% name)]] <-
        attrs[[name %>% tidy_gsub("_", "-", fixed = TRUE)]]
    }
  }

  start
}

normalize_element_list <- function(element, attrs) {

  # Are there assigned values in `attrs` also in `element`
  if (
    !(
      names(element) %in% (names(attrs) %>% tidy_gsub("-", "_", fixed = TRUE))
    ) %>%
    any()
  ) {
    return(element)
  }

  attrs_names <- names(attrs) %>% tidy_gsub("-", "_", fixed = TRUE)
  names_element <- names(element)

  for (name in attrs_names) {
    if (name %in% names_element) {

      element[[which(names_element %in% name)]] <-
        attrs[[name %>% tidy_gsub("_", "-", fixed = TRUE)]]
      attrs[name] <- NULL
    }
  }

  element$attrs <- attrs

  element
}

get_points_str_attrs <- function(points) {

  points_pairs <- strsplit(points, " ") %>% unlist() %>% strsplit(",")

  x <-
    dplyr::tibble(
      x = points_pairs %>% lapply(`[[`, 1) %>% unlist() %>% as.numeric()
    )

  y <-
    dplyr::tibble(
      y = points_pairs %>% lapply(`[[`, 2) %>% unlist() %>% as.numeric()
    )

  points_tbl <- dplyr::bind_cols(x, y)

  list(
    ul = c(min(points_tbl$x), min(points_tbl$y)),
    ur = c(max(points_tbl$x), min(points_tbl$y)),
    lr = c(max(points_tbl$x), max(points_tbl$y)),
    ll = c(min(points_tbl$x), max(points_tbl$y)),
    center = c(mean(points_tbl$x), mean(points_tbl$y)),
    width = max(points_tbl$x) - min(points_tbl$x),
    height = max(points_tbl$y) - min(points_tbl$y)
  )
}
