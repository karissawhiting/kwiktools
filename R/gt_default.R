
#' GT Default Display
#'
#' @param df a data frame
#' @param custom_caption a table title
#' @export
#'
gt_default <- function(df, custom_caption = NULL) {

  df %>%
    gt::gt() %>%
    gt::tab_options(
      table.font.size = "small",
      data_row.padding = gt::px(1)) %>%
    gt::tab_header(custom_caption)
}


#' GT Default Display for Table Summary/Regression Obj
#'
#' @param tbl_obj a tbl_summary or tbl_regression object
#' @param custom_caption a table title
#' @export
#'
gtsum_default <- function(tbl_obj, custom_caption = NULL) {

  tbl_obj %>%
    gtsummary::bold_labels() %>%
    gtsummary::italicize_levels() %>%
    gtsummary::as_gt() %>%
    gt::tab_options(
      table.font.size = "small",
      data_row.padding = gt::px(1)) %>%
    gt::tab_header(custom_caption)
}
