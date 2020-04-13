library(tidyverse)
library(omsvg)
library(usethis)

file_list <- list.files("data-raw/svg", full.names = TRUE)

solids <- file_list[stringr::str_detect(file_list, pattern = "-solid.svg$")]
solids_new <- gsub("-solid.svg", ".svg", solids)

file.rename(from = solids, to = solids_new)

file_list <- list.files("data-raw/svg", full.names = TRUE)
icon_names <- list.files("data-raw/svg") %>% gsub(".svg", "", .)

svg_text <- rep(NA_character_, length(file_list))
for (i in seq(file_list)) {
  svg_text[i] <- readLines(file_list[i], warn = FALSE)
}

# Create a table of LA SVG data
la_svg_tbl <- dplyr::tibble(icon_name = icon_names, svg_text = svg_text)

# Read in the lookup table for icon names and categories
categories_icons <- readr::read_csv("data-raw/categories_icons.csv", col_types = "cc")

# Join tables together to get a `category_column`
la_svg_tbl <-
  la_svg_tbl %>%
  dplyr::left_join(categories_icons, by = "icon_name")

# Save to internal data
usethis::use_data(la_svg_tbl, internal = TRUE, overwrite = TRUE)
