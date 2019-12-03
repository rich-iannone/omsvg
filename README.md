
<!-- README.md is generated from README.Rmd. Please edit that file -->

# omsvg <img src="man/figures/logo.svg" align="right" height="250px" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/omsvg)](https://CRAN.R-project.org/package=omsvg)
[![Travis build
status](https://travis-ci.org/rich-iannone/omsvg.svg?branch=master)](https://travis-ci.org/rich-iannone/omsvg)
<!-- badges: end -->

## Overview

Scalable vector graphics, SVGs, are *fantastic*. The **omsvg** package
lets you make SVGs using the **R** programming language, and, it tries
to make the whole process a little less difficult. We can create SVG
elements, transform them programmatically, apply filters, and even
animate them.

### Creating a Simple SVG

The process of using **omsvg** begins with the `SVG()` function. That
allows you to define the overall size of the graphic and set some global
options. Let’s start simple and make an SVG with three elements: (1) a
rectangle, (2) an ellipse, and (3) some text. To do this, we’ll use the
`svg_rect()`, `svg_ellipse()`, and `svg_text()` functions.

``` r
svg <- 
  SVG(width = 250, height = 100) %>%
  svg_rect(x = 25, y = 25, width = 50, height = 50, fill = "yellow") %>%
  svg_ellipse(x = 125, y = 50, width = 50, height = 50, stroke = "magenta") %>%
  svg_text(x = 175, y = 50, text = "this is text", attrs = attrs_pres(font_family = "Helvetica"))
```

This `svg` object can be transformed to an `<svg>` tag with
`as.character()`. Wanna see the SVG code? Here it is:

``` r
svg %>% as.character() %>% cat()
#> <svg width="250px" height="100px" viewBox="0 0 250 100" version="1.1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
#> <defs>
#> 
#> </defs>
#> <rect x="25" y="25" width="50" height="50" fill="yellow"/>
#> <ellipse cx="125" cy="50" rx="25" ry="25" stroke="magenta"/>
#> <text x="175" y="50" font-family="Helvetica">this is text</text>
#> </svg>
```

And here’s the SVG:

<img src="man/figures/example_1.svg" align="center" />

Aside from rectangles, ellipses, and text, we can also elect to use
circles (`svg_circle()`), lines (`svg_line()`), *polylines*
(`svg_polyline()`), polygons (`svg_polygon()`), and
as-complex-as-you-can-make-’em paths (`svg_path()`).

### Animating an SVG

One thing that’s really great about SVGs is that they can be animated.
Almost everything in an SVG is animatable. The **omsvg** package lets us
animate each element with minimal frustration. Here is an example of a
rectangle being quite animated.

``` r
svg_rectangle_anim <- 
  SVG(width = 700, height = 150) %>%
  svg_rect(
    x = 100, y = 75,
    width = 100, height = 100,
    stroke = "cyan",
    fill = "lightblue",
    anims = anims(
      0.5 ~ list(
        anim_position(initial = TRUE),
        anim_rotation(initial = TRUE)
      ),
      2.0 ~ list(
        anim_position(x = 500, y = 75, easing_fn = ease_in_out()),
        anim_rotation(90, easing_fn = ease_in_out())
      )
    )
  )
```

<img src="man/figures/example_2.svg" align="center" />

Animations are done by reference to keyframe times. Each of these times
(in seconds) is like a transition point. Above, the rectangle is held at
its initial position and rotation state until `0.5` seconds has elapsed.
At `2.0` seconds, the second keyframe, the rectangle’s position is to be
moved from `{x = 100, y = 100}` to `{x = 500, y = 100}`, and, its
rotation state should change to `90` degrees. We can assign a `timing`
function that governs the tweening of the animation. For the position
and rotation changes, these are both using the `ease_in_out()` timing
function (where movement eases into a maximum speed and then decelerates
to a stop).

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("rich-iannone/omsvg")
```
