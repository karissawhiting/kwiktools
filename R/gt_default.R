
# GT Functions ---------------------------------------

gt_default <- function(df, custom_caption = NULL) {

  df %>%
    gt() %>%
    tab_options(
      table.font.size = "small",
      row.padding = px(1)) %>%
    tab_header(custom_caption)
}

gtsum_default <- function(tbl_obj, custom_caption = NULL) {

  tbl_obj %>%
    tab_style_bold_labels() %>%
    tab_style_italicize_levels() %>%
    as_gt() %>%
    tab_options(
      table.font.size = "small",
      row.padding = px(1)) %>%
    tab_header(custom_caption)
}
