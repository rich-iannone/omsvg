library(omsvg)

#
# Animation tests
#

# Basic animation of an element's
# rotation state (moving to a new
# `rotation` value)
SVG(width = 300, height = 300) %>%
  svg_rect(
    x = 50, y = 50,
    width = 50, height = 50,
    attrs = attrs_pres(
      stroke = "magenta",
      fill = "lightblue"
    ),
    anims = anims(
      2.0 ~ anim_rotation(rotation = 180)
    )
  )

# Same, but with a circle
SVG(width = 300, height = 300) %>%
  svg_circle(
    x = 50, y = 50,
    diameter = 30,
    attrs = attrs_pres(
      stroke = "magenta",
      fill = "lightblue"
    ),
    anims = anims(
      2.0 ~ anim_rotation(rotation = 180)
    )
  )

# With an ellipse
SVG(width = 300, height = 300) %>%
  svg_ellipse(
    x = 50, y = 50,
    width = 50, height = 80,
    attrs = attrs_pres(
      stroke = "magenta",
      fill = "lightblue"
    ),
    anims = anims(
      2.0 ~ anim_rotation(rotation = 180)
    )
  )

# Basic animation of an element's
# opacity value (moving to a new
# `opacity` value of `0`)
SVG(width = 300, height = 300) %>%
  svg_rect(
    x = 50, y = 50,
    width = 50, height = 50,
    attrs = attrs_pres(
      stroke = "magenta",
      fill = "lightblue"
    ),
    anims = anims(
      2.0 ~ anim_opacity(opacity = 0)
    )
  )

# Lots of different animation directives applied
# to two different elements (position, rotation,
# scaling, opacity) with wildly different
# keyframe times
SVG(width = 300, height = 300) %>%
  svg_rect(
    x = 50, y = 50,
    width = 50, height = 50,
    attrs = attrs_pres(
      stroke = "magenta",
      fill = "lightblue"
    ),
    anims = anims(
      0.5 ~ anim_position(initial = TRUE),
      2.0 ~ anim_position(x = 100, y = 50),
      2.5 ~ anim_position(x = 150, y = 50),
      1.0 ~ anim_rotation(initial = TRUE),
      3.5 ~ anim_rotation(rotation = 90),
      6.0 ~ anim_opacity(opacity = 0.2),
      2.3 ~ anim_scale(scale = 1.0),
      3.0 ~ anim_scale(scale = 1.5)
    )
  ) %>%
  svg_rect(
    x = 100, y = 50,
    width = 50, height = 50,
    attrs = attrs_pres(
      stroke = "black",
      fill = "green"
    ),
    anims = anims(
      0.5 ~ anim_position(initial = TRUE),
      2.0 ~ anim_position(x = 150, y = 50),
      2.5 ~ anim_position(x = 200, y = 50),
      1.0 ~ anim_rotation(initial = TRUE),
      3.5 ~ anim_rotation(rotation = 90),
      6.0 ~ anim_opacity(opacity = 0.2),
      2.3 ~ anim_scale(scale = 1.0),
      3.0 ~ anim_scale(scale = 1.5)
    )
  )

# Apply timing functions to animations
SVG(width = 300, height = 300) %>%
  svg_rect(
    x = 50, y = 50,
    width = 50, height = 50,
    attrs = attrs_pres(
      stroke = "black",
      fill = "yellow"
    ),
    anims = anims(
      0.5 ~ list(
        anim_position(x = 50, y = 50, timing = ease_in_out()),
        anim_rotation(0, timing = ease_in_out())
      ),
      2.0 ~ list(
        anim_position(x = 200, y = 50, timing = ease_in_out()),
        anim_rotation(90, timing = ease_in_out())
      )
    )
  )

# An animation solely with opacity
SVG(width = 300, height = 300) %>%
  svg_rect(
    x = 50, y = 50,
    width = 50, height = 50,
    opacity = 0.5,
    attrs = attrs_pres(
      stroke = "red",
      fill = "green"
    ),
    anims = anims(
      2.0 ~ anim_opacity(opacity = 1.0, timing = ease_in_out()),
      3.0 ~ anim_opacity(opacity = 0.2, timing = ease_in_out())
    )
  )

# A position and rotation animation that uses
# the initial position
SVG(width = 300, height = 300) %>%
  svg_rect(
    x = 50, y = 50,
    width = 50, height = 50,
    attrs = attrs_pres(
      stroke = "black",
      fill = "yellow"
    ),
    anims = anims(
      1.0 ~ list(
        anim_position(initial = TRUE),
        anim_rotation(initial = TRUE)
      ),
      3.0 ~ list(
        anim_position(x = 200, y = 50),
        anim_rotation(90)
      ),
      5.0 ~ list(
        anim_position(initial = TRUE),
        anim_rotation(initial = TRUE)
      )
    )
  )

# An animation with a position and
# scale change
SVG(width = 300, height = 300) %>%
  svg_rect(
    x = 50, y = 50,
    width = 40, height = 40,
    attrs = attrs_pres(
      stroke = "red",
      fill = "green"
    ),
    anims = anims(
      2.0 ~ list(
        anim_scale(scale = 2), anim_position(x = 50, y = 100)
      )
    )
  )

