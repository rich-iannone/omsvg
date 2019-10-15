% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/anim_scale.R
\name{anim_scale}
\alias{anim_scale}
\title{Animate an element through scaling}
\usage{
anim_scale(scale = NULL, timing = NULL)
}
\arguments{
\item{scale}{The scale value of the element at the keyframe time (given as
the LHS value in the \code{\link[=anims]{anims()}} call). If providing a single scaling value,
the scaling will operate in the x and y directions (relative to the center
of the element). If two values are provided, these will be taken as scaling
values in the x and y directions.}

\item{timing}{The timing function to use for the movement to the new
position.}
}
\description{
With an \code{\link[=anims]{anims()}} call, itself passed as to any \code{anims} argument, the
\code{anim_scale()} function can be used to express an animation where the target
element undergoes a rotation change with time.
}
\examples{
# Basic animation of an element's
# rotation state (moving to a new
# `rotation` value)
SVG(width = 300, height = 300) \%>\%
  svg_rect(
    x = 50, y = 50,
    width = 50, height = 50,
    attrs = attrs_pres(
      stroke = "magenta",
      fill = "lightblue"
    ),
    anims = anims(
      2.0 ~ anim_scale(scale = 2)
    )
  )

}