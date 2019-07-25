
#' Convert DF names to labels
#'
#' @param df a data frame
#' @param caps a string of capitalization exceptions
#' @export
#'
#'
# labelling funs-------------------------------------------------------------

# function to transform names of a dataframe to friendly labels
names_to_labs <- function(df, caps = NULL) {
  names <- names(df)

  if (!is.null(caps)) {
    named_vec <- c(toupper(caps))
    names(named_vec) <- tolower(caps)
  }

  labels <- str_replace_all(names, "_", " ") %>%
    tolower(.) %>%
    tools::toTitleCase(.) %>%
    purrr::when(!is.null(caps), ~stringr::str_replace_all(tolower(.), named_vec))

  return(labels)
}
names_to_labs(df, caps = c("Sepal", "Petal"))

