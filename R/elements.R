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
#' @param x,y The `x` and `y` positions of the upper left of the rectangle to be
#'   drawn. The `x` and `y` values are relative to upper left of the SVG drawing
#'   area. Both of these attributes can be animated.
#' @param width,height The `width` and `height` of the rectangle that is to be
#'   drawn. The `width` is the distance in the 'x' direction from point `x`
#'   (proceeding right) and the `height` is the distance in the 'y' direction
#'   from point `y` (proceeding downward). Both of these attributes can be
#'   animated.
#' @param rx,ry Optional corner radius values in the 'x' and 'y' directions.
#'   Applies to all corners of the rectangle. If only one value is provided
#'   (say, just for `rx`) then the unset value will take that set value as well.
#'   These attributes can both be animated.
#' @param attrs A presentation attribute list. The helper function
#'   [attrs_pres()] can help us easily generate this named list object. For the
#'   most part, the list's names are the presentation attribute names and the
#'   corresponding values are the matching attribute values.
#' @param animate An animation directive list.
#' @param id An optional ID value to give to the built tag. This is useful for
#'   modifying this element in a later function call or for interacting with
#'   CSS.
#'
#' @examples
#' # Create an SVG with a single
#' # rectangle element
#' SVG(width = 100, height = 40) %>%
#'   svg_rect(
#'     x = 20, y = 10,
#'     width = 40, height = 15,
#'     attrs = attrs_pres(
#'       stroke = "red",
#'       fill = "green"
#'     )
#'   )
#'
#' @export
svg_rect <- function(svg,
                     x,
                     y,
                     width,
                     height,
                     rx = NULL,
                     ry = NULL,
                     attrs = list(),
                     anims = list(),
                     id = NULL) {




  element_list <-
    list(
      type = "rect",
      x = x,
      y = y,
      width = width,
      height = height,
      rx = rx,
      ry = ry,
      attrs = attrs,
      anims = anims,
      tag = NA_character_
    )

  add_element_list(
    svg = svg,
    element_list = element_list,
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
#' @param x,y The `x` and `y` positions of the center of the circle to be
#'   drawn. The `x` and `y` values are relative to upper left of the SVG
#'   drawing area.
#' @param diameter The diameter of the circle shape in units of `px`.
#' @inheritParams svg_rect
#'
#' @examples
#' # Create an SVG with a single
#' # circle element
#' SVG(width = 80, height = 80) %>%
#'   svg_circle(
#'     x = 30, y = 30,
#'     diameter = 40,
#'     attrs = list(fill = "red")
#'   )
#'
#' @export
svg_circle <- function(svg,
                       x,
                       y,
                       diameter,
                       attrs = list(),
                       anims = list(),
                       id = NULL) {

  radius <- diameter / 2

  element_list <-
    list(
      type = "circle",
      cx = x,
      cy = y,
      r = radius,
      attrs = attrs,
      anims = anims,
      tag = NA_character_
    )

  add_element_list(
    svg = svg,
    element_list = element_list,
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
#'   drawn. The `width` is the overall width of the ellipse in the 'x' direction,
#'   centered on point `x`. The `height` is the distance in the 'y' direction,
#'   centered on point `y`.
#' @inheritParams svg_rect
#'
#' @examples
#' # Create an SVG with a single
#' # ellipse element
#' SVG(width = 60, height = 60) %>%
#'   svg_ellipse(
#'     x = 30, y = 30,
#'     width = 50, height = 30,
#'     attrs = list(fill = "purple")
#'   )
#'
#' @export
svg_ellipse <- function(svg,
                        x,
                        y,
                        width,
                        height,
                        attrs = list(),
                        anims = list(),
                        id = NULL) {

  rx <- width / 2
  ry <- height / 2

  element_list <-
    list(
      type = "ellipse",
      cx = x,
      cy = y,
      rx = rx,
      ry = ry,
      attrs = attrs,
      anims = anims,
      tag = NA_character_
    )

  add_element_list(
    svg = svg,
    element_list = element_list,
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
#' SVG(width = 100, height = 50) %>%
#'   svg_line(
#'     x1 = 5, y1 = 5,
#'     x2 = 95, y2 = 45,
#'     attrs = list(stroke = "blue")
#'   )
#'
#' @export
svg_line <- function(svg,
                     x1,
                     y1,
                     x2,
                     y2,
                     attrs = list(),
                     anims = list(),
                     id = NULL) {

  element_list <-
    list(
      type = "line",
      x1 = x1,
      x2 = x2,
      y1 = y1,
      y2 = y2,
      attrs = attrs,
      anims = anims,
      tag = NA_character_
    )

  add_element_list(
    svg = svg,
    element_list = element_list,
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
#' # polyline element
#' SVG(width = 300, height = 300) %>%
#'   svg_polyline(
#'     points = c(
#'       10, 10, 15, 20, 20, 15, 25, 30, 30, 25,
#'       35, 40, 40, 35, 45, 50, 50, 45),
#'     attrs = list(
#'       stroke = "blue",
#'       fill = "none"
#'     )
#'   )
#'
#' @export
svg_polyline <- function(svg,
                         points,
                         attrs = list(),
                         anims = list(),
                         id = NULL) {

  if (inherits(points, "numeric")) {
    mat <- matrix(points, ncol = 2)
    points <- paste(mat[, 1], mat[, 2], collapse = ", ")
  }

  element_list <-
    list(
      type = "polyline",
      points = points,
      attrs = attrs,
      anims = anims,
      tag = NA_character_
    )

  add_element_list(
    svg = svg,
    element_list = element_list,
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
#' SVG(width = 300, height = 300) %>%
#'   svg_polygon(
#'     points = "100,10 40,198 190,78 10,78 160,198",
#'     attrs = list(
#'       stroke = "orange",
#'       `stroke-width` = 2,
#'       fill = "yellow"
#'     )
#'   )
#'
#' @export
svg_polygon <- function(svg,
                        points,
                        attrs = list(),
                        anims = list(),
                        id = NULL) {

  if (inherits(points, "numeric")) {
    mat <- matrix(points, ncol = 2)
    points <- paste(mat[, 1], mat[, 2], collapse = ", ")
  }

  element_list <-
    list(
      type = "polygon",
      points = points,
      attrs = attrs,
      anims = anims,
      tag = NA_character_
    )

  add_element_list(
    svg = svg,
    element_list = element_list,
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
#' SVG(width = 300, height = 300) %>%
#'   svg_path(
#'     path = "M 50 160 q 100 -300 200 0",
#'     attrs = list(
#'       stroke = "magenta",
#'       `stroke-width` = 5,
#'       fill = "lightblue"
#'     )
#'   )
#'
#' @export
svg_path <- function(svg,
                     path,
                     attrs = list(),
                     anims = list(),
                     id = NULL) {

  element_list <-
    list(
      type = "path",
      d = path,
      attrs = attrs,
      anims = anims,
      tag = NA_character_
    )

  add_element_list(
    svg = svg,
    element_list = element_list,
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
#' @inheritParams svg_rect
#' @inheritParams svg_path
#'
#' @examples
#' # Create an SVG with a single
#' # text element
#' SVG(width = 300, height = 300) %>%
#'   svg_text(
#'     x = 10, y = 20,
#'     text = "A line of text"
#'   )
#'
#' @export
svg_text <- function(svg,
                     x,
                     y,
                     text,
                     path = NULL,
                     attrs = list(),
                     anims = list(),
                     id = NULL) {

  element_list <-
    list(
      type = "text",
      x = x,
      y = y,
      text = text,
      path = path,
      attrs = attrs,
      anims = anims,
      tag = NA_character_
    )

  add_element_list(
    svg = svg,
    element_list = element_list,
    id = id
  )
}

shape_types <- function() {

  c("rect", "circle", "ellipse", "line", "polyline", "polygon", "path")
}

add_element_list <- function(svg, element_list, id) {

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
