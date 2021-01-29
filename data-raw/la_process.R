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

# Normalize the SVG text
svg_text <- svg_text %>% gsub("<?xml version=\"1.0\"?>", "", ., fixed = TRUE)

# Create a table of LA SVG data
la_svg_tbl <- dplyr::tibble(icon_name = icon_names, svg_text = svg_text)

# Read in the lookup table for icon names and categories
categories_icons <-
  readr::read_csv("data-raw/categories_icons.csv", col_types = "cc") %>%
  dplyr::mutate(category = category %>% gsub("&", "and", ., fixed = TRUE))

# Join tables together to get a `category_column`
la_svg_tbl <-
  la_svg_tbl %>%
  dplyr::left_join(categories_icons, by = "icon_name")

#
# Generate a 6-column gt table based on `la_svg_tbl`
#

arranged_tbl <-
  la_svg_tbl %>%
  dplyr::arrange(category) %>%
  dplyr::select(category, svg_text, icon_name)

items_cat <-
  arranged_tbl %>%
  dplyr::group_by(category) %>%
  dplyr::count() %>%
  dplyr::ungroup() %>%
  dplyr::pull(n)

seq_1 <- seq(1, max(items_cat) + 10, by = 3)
seq_2 <- seq(2, max(items_cat) + 10, by = 3)
seq_3 <- seq(3, max(items_cat) + 10, by = 3)

tbl_1 <-
  arranged_tbl %>%
  dplyr::group_by(category) %>%
  dplyr::filter(dplyr::row_number() %in% seq_1) %>%
  dplyr::rename(icon_name_1 = icon_name) %>%
  dplyr::rename(svg_text_1 = svg_text) %>%
  dplyr::mutate(category = paste0(category, "__", dplyr::row_number())) %>%
  dplyr::ungroup()

tbl_2 <-
  arranged_tbl %>%
  dplyr::group_by(category) %>%
  dplyr::filter(dplyr::row_number() %in% seq_2) %>%
  dplyr::rename(icon_name_2 = icon_name) %>%
  dplyr::rename(svg_text_2 = svg_text) %>%
  dplyr::mutate(category = paste0(category, "__", dplyr::row_number())) %>%
  dplyr::ungroup()

tbl_3 <-
  arranged_tbl %>%
  dplyr::group_by(category) %>%
  dplyr::filter(dplyr::row_number() %in% seq_3) %>%
  dplyr::rename(icon_name_3 = icon_name) %>%
  dplyr::rename(svg_text_3 = svg_text) %>%
  dplyr::mutate(category = paste0(category, "__", dplyr::row_number())) %>%
  dplyr::ungroup()

wide_tbl <-
  tbl_1 %>%
  dplyr::left_join(tbl_2, by = "category") %>%
  dplyr::left_join(tbl_3, by = "category") %>%
  dplyr::mutate(category = category %>% gsub("__.*", "", .))

suppressWarnings(
  gt_tbl <-
    wide_tbl %>%
    gt::gt(groupname_col = "category") %>%
    gt::fmt_markdown(columns = gt::matches("text")) %>%
    gt::fmt_missing(columns = TRUE, missing_text = "") %>%
    gt::cols_width(gt::matches("text") ~ gt::px(80), gt::everything() ~ gt::px(150)) %>%
    gt::tab_style(
      style = gt::cell_text(size = gt::px(32), v_align = "bottom"),
      locations = gt::cells_row_groups()
    ) %>%
    gt::tab_options(
      column_labels.hidden = TRUE,
      row_group.padding = gt::px(20)
    ) %>%
    gt::opt_table_lines(extent = "none") %>%
    gt::opt_all_caps()
)

gt_tbl_raw <-
  gt_tbl %>%
  gt:::as_raw_html()

# Save both object to internal data
usethis::use_data(
  la_svg_tbl, gt_tbl_raw,
  internal = TRUE, overwrite = TRUE
)
