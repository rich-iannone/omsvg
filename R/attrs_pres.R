#' Define SVG presentation attributes for an element
#'
#' The `attrs_pres()` helper function can be used to more easily generate a
#' valid presentation attribute list for the `attrs` argument that is present
#' in every SVG element function (e.g., [svg_rect()], [svg_text()], etc.). All
#' of the presentation attributes formally included here as options can be
#' animated.
#'
#' @param stroke The color used to paint the outline of the shape.
#' @param stroke_width The width of the stroke to be applied to the shape. Can
#'   be expressed in `px` or percentage units.
#' @param stroke_opacity The opacity of the stroke of a shape. We can use a real
#'   number from `0` to `1` or a value in percentage units.
#' @param fill The color used to fill the inside of the element.
#' @param fill_opacity The opacity of the color or the content the current
#'   object is filled with. We can use a real number from `0` to `1` or a value
#'   in percentage units.
#' @param font_family Which font family will be used to render the text of the
#'   element?
#' @param font_size The size of the font.
#' @param font_weight The weight or boldness of the font. Possible values are
#'   `"normal"`, `"bold"`, `"lighter"`, `"bolder"`, and the values `100`, `200`,
#'   and so on, up to `900`.
#' @param font_style Whether a font should be styled with a `"normal"`,
#'   `"italic"`, or `"oblique"` face from its `font_family`.
#' @param text_decoration Add decorative lines on text. Options are
#'   `"underline"`, `"overline"`, `"line-through"`, and `"blink"`.
#' @param transform A list of transform definitions that are applied to an
#'   element and the element's children.
#' @param filter The filter effects defined by a `<filter>` element that shall
#'   be applied to its element. Requires a reference to a `<filter>` `id`
#'   attribute.
#' @param mask The mask defined by a `<mask>` element that shall be applied to
#'   its element. Requires a reference to a `<mask>` `id` attribute.
#' @param clip_path The clipping path defined by a `<clipPath>` element that
#'   shall be applied to its element. Requires a reference to a `<clipPath>`
#'   `id` attribute.
#' @param clip_rule A rule for determining what side of a path is inside of a
#'   shape in order to know how `clip_path` should clip its target.
#'   Options are `"nonzero"`, `"evenodd"`, and `"inherit"`.
#' @param stroke_dasharray The pattern of dashes and gaps used to paint the
#'   outline of the shape.
#' @param stroke_dashoffset Defines an offset on the rendering of the associated
#'   dash array.
#' @param stroke_linecap The shape to be used at the end of open subpaths when
#'   they are stroked. We can use the options `"butt"`, `"round"`, or
#'   `"square"`.
#' @param stroke_linejoin The shape to be used at the corners of paths when they
#'   are stroked (`"arcs"`, `"bevel"`, `"miter"`, `"miter-clip"`, and
#'   `"round"`).
#' @param stroke_miterlimit The limit on the ratio of the miter length to the
#'   `stroke_width` Used to draw a miter join. A numeric value should be used
#'   to define the limit.
#' @param fill_rule A rule for determining what side of a path is inside of a
#'   shape. Options are `"nonzero"`, `"evenodd"`, and `"inherit"`.
#' @param color Potentially provides an indirect value (as the `currentColor`)
#'   for `fill`, `stroke`, `stop_color`, `flood_color` and `lighting_color`
#'   options.
#' @param opacity Specifies the transparency of an object or a group of objects.
#'   We can use a real number from `0` to `1` or a value in percentage units.
#' @param color_interpolation The color space for gradient interpolations, color
#'   animations, and alpha compositing. Allowed values are: `"auto"`, `"sRGB"`,
#'   `"linearRGB"`, and `"inherit"`.
#' @param color_interpolation_filters The color space for imaging operations
#'   performed via filter effects. Allowed values are: `"auto"`, `"sRGB"`,
#'   `"linearRGB"`, and `"inherit"`.
#' @param lighting_color The color of the light source for filter primitives
#'   elements `<feSpecularLighting>` and `<feDiffuseLighting>`.
#' @param flood_color,flood_opacity The color and opacity level to use to flood
#'   the current filter primitive subregion defined through the `<feFlood>` or
#'   `<feDropShadow>` element.
#' @param stop_color,stop_opacity Sets the color and opacity at a gradient stop.
#' @param font_variant Determines whether a font should be used with some of
#'   their variation such as small caps or ligatures.
#' @param font_stretch Allows for a selection of a normal, condensed, or
#'   expanded face from a font.
#' @param font_size_adjust Specifies that the font size should be chosen based
#'   on the height of lowercase letters rather than the height of capital
#'   letters.
#' @param text_anchor The vertical alignment a string of text. We can use the
#'   values `"start"`, `"middle"`, `"end"`, or `"inherit"`.
#' @param letter_spacing,word_spacing The spacing between text characters and
#'   between words.
#' @param dominant_baseline The baseline used to align the box’s text and
#'   inline-level contents. The options for this are: `"auto"`, `"text-bottom"`,
#'   `"alphabetic"`, `"ideographic"`, `"middle"`, `"central"`, `"mathematical"`,
#'   `"hanging"`, and `"text-top"`.
#' @param alignment_baseline Determines how an object is to be aligned along the
#'   font baseline with respect to its parent. Allowed values are: `"auto"`,
#'   `"baseline"`, `"before-edge"`, `"text-before-edge"`, `"middle"`,
#'   `"central"`, `"after-edge"`, `"text-after-edge"`, `"ideographic"`,
#'   `"alphabetic"`, `"hanging"`, `"mathematical"`, and `"inherit"`.
#' @param baseline_shift An option for repositioning of the dominant-baseline
#'   relative to the dominant-baseline of the parent text content element. Valid
#'   options are: `"auto"`, `"baseline"`, `"super"`, `"sub"`, `"inherit"`, a
#'   length value, or a percentage value.
#' @param direction The base writing direction of text. Can be either `"ltr"`,
#'   `"rtl"`, or `"inherit"`.
#' @param writing_mode The initial inline-progression-direction for a `<text>`
#'   element (can be left-to-right, right-to-left, or top-to-bottom). Valid
#'   values are `"lr-tb"`, `"rl-tb"`, `"tb-rl"`, `"lr"`, `"rl"`, `"tb"`, or
#'   `"inherit"`.
#' @param overflow The overflow behavior for the content of a block-level
#'   element when it overflows the element's box. Options are: `"visible"`,
#'   `"hidden"`, `"scroll"`, `"auto"`, and `"inherit"`.
#' @param marker_start,marker_mid,marker_end The arrowhead or polymarker that
#'   will be drawn at the first node, the final node, or, the in-between nodes.
#'   This applies to a `<path>` element or a basic shape. These attributes can
#'   be applied to any element but only have an effect on the following seven
#'   elements: `<rect>`, `<circle>`, `<ellipse>`, `<line>`, `<path>`,
#'   `<polygon>`, and `<polyline>`. Requires a reference to a `<marker>` `id`
#'   attribute (defined within the SVG's `<defs>` area).
#' @param pointer_events Defines whether or when an element may be the target of
#'   a mouse event. Options are: `"bounding-box"`, `"visiblePainted"`,
#'   `"visibleFil"`, `"visibleStroke"`, `"visible"`| `"painted"`, `"fill"`,
#'   `"stroke"`, `"all"`, and `"none"`.
#' @param cursor The mouse cursor displayed when the mouse pointer is over an
#'   element.
#' @param vector_effect The vector effect to use when drawing an object. Options
#'   are: `"default"`, `"non-scaling"`, `"stroke"`, and `"inherit"`.
#' @param shape_rendering,color_rendering,text_rendering,image_rendering A
#'   quality setting parameter for shapes, color interpolation and compositing,
#'   text, and image processing. All of the rendering attributes can use the
#'   `"auto"` and `"optimizeSpeed"` directives. For shape rendering, we can
#'   elect for `"crispEdges"`, `"geometricPrecision"`, or just `"inherit"`. When
#'   rendering color, additional choices are `"optimizeQuality"` and
#'   `"inherit"`. Text rendering allows us the additional
#'   `"optimizeLegibility"`, `"geometricPrecision"`, and `"inherit'` options.
#'   With image rendering, we can furthermore choose to `"optimizeSpeed"`.
#' @param display Allows for control of the rendering of graphical or container
#'   elements. A value of `"none"` indicates that the given element and its
#'   children will not be rendered. Any value other than `"none"` or `"inherit"`
#'   indicates that the given element will be rendered by the browser.
#' @param visibility The visibility attribute lets us control the visibility of
#'   graphical elements. With a value of `"hidden"` or `"collapse"`, the element
#'   is invisible.
#' @export
attrs_pres <- function(stroke = NULL,
                       stroke_width = NULL,
                       stroke_opacity = NULL,
                       fill = NULL,
                       fill_opacity = NULL,
                       font_family = NULL,
                       font_size = NULL,
                       font_weight = NULL,
                       font_style = NULL,
                       text_decoration = NULL,
                       transform = NULL,
                       filter = NULL,
                       mask = NULL,
                       clip_path = NULL,
                       clip_rule = NULL,
                       stroke_dasharray = NULL,
                       stroke_dashoffset = NULL,
                       stroke_linecap = NULL,
                       stroke_linejoin = NULL,
                       stroke_miterlimit = NULL,
                       fill_rule = NULL,
                       color = NULL,
                       opacity = NULL,
                       color_interpolation = NULL,
                       color_interpolation_filters = NULL,
                       lighting_color = NULL,
                       flood_color = NULL,
                       flood_opacity = NULL,
                       stop_color = NULL,
                       stop_opacity = NULL,
                       font_variant = NULL,
                       font_stretch = NULL,
                       font_size_adjust = NULL,
                       text_anchor = NULL,
                       letter_spacing = NULL,
                       word_spacing = NULL,
                       dominant_baseline = NULL,
                       alignment_baseline = NULL,
                       baseline_shift = NULL,
                       direction = NULL,
                       writing_mode = NULL,
                       overflow = NULL,
                       marker_start = NULL,
                       marker_mid = NULL,
                       marker_end = NULL,
                       pointer_events = NULL,
                       cursor = NULL,
                       vector_effect = NULL,
                       shape_rendering = NULL,
                       color_rendering = NULL,
                       text_rendering = NULL,
                       image_rendering = NULL,
                       display = NULL,
                       visibility = NULL) {

  arg_names <- formals(attrs_pres) %>% names()
  arg_vals <- mget(arg_names)
  arg_vals <- arg_vals[!vapply(arg_vals, FUN = is.null, FUN.VALUE = logical(1))]

  arg_names_svg <-
    names(arg_vals) %>%
    vapply(
      FUN.VALUE = character(1),
      USE.NAMES = FALSE,
      function(x) {
        if (grepl("_", x)) {
          x <- x %>%
            tidy_gsub("_", "-", fixed = TRUE)

          return(x)
        }

        x
      })

  names(arg_vals) <- arg_names_svg

  arg_vals <- attrs_to_filter(arg_vals = arg_vals)

  arg_vals
}

attrs_to_filter <- function(arg_vals) {

  if (!(any(c("filter", "mask", "clip_path") %in% names(arg_vals)))) {
    return(arg_vals)
  }

  styles <- c()

  if ("filter" %in% names(arg_vals)) {
    styles <- c(styles, ref_to_url(attr = "filter", arg_vals = arg_vals))
    arg_vals[names(arg_vals) == "filter"] <- NULL
  }

  if ("mask" %in% names(arg_vals)) {
    styles <- c(styles, ref_to_url(attr = "mask", arg_vals = arg_vals))
    arg_vals[names(arg_vals) == "mask"] <- NULL
  }

  if ("clip_path" %in% names(arg_vals)) {
    styles <- c(styles, ref_to_url(attr = "clip_path", arg_vals = arg_vals))
    arg_vals[names(arg_vals) == "clip_path"] <- NULL
  }

  styles <- styles %>% paste(collapse = "; ")

  c(arg_vals, list(filter = styles))
}

ref_to_url <- function(attr, arg_vals) {

  arg_vals[[which(names(arg_vals) == attr)]] %>%
    paste_left("url(#") %>%
    paste_right(")")
}
