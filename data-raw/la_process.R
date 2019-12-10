library(tidyverse)
library(omsvg)
library(here)
library(usethis)

file_list <- list.files("data-raw/svg", full.names = TRUE)

icon_names <- list.files("data-raw/svg") %>% gsub(".svg", "", .)

svg_text <- rep(NA_character_, length(file_list))
for (i in seq(file_list)) {
  svg_text[i] <- readLines(file_list[i], warn = FALSE)
}

# Create a table of LA SVG data
la_svg_tbl <- dplyr::tibble(icon_names = icon_names, svg_text = svg_text)

# Save to internal data
usethis::use_data(la_svg_tbl, internal = TRUE, overwrite = TRUE)
