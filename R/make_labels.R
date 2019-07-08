#' Make Labels
#'
#' @param names names vector of a data frame
#' @export
#'
#'
make_labels <- function(names) {
  str_replace_all(names, "_", " ") %>%
    str_to_title() %>%
    return
}
