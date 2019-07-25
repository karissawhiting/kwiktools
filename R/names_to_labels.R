
#' Convert DF names to labels
#'
#' @param df a data frame
#' @param caps a string of capitalization exceptions
#' @export
#'
#'
# labelling funs-------------------------------------------------------------

# function to transform names of a dataframe to friendly labels
names_to_labels <- function(df, caps = NULL) {

  names <- names(df)

  labels <- stringr::str_replace_all(names, "_", " ") %>%
    tolower(.) %>%
    tools::toTitleCase(.)

  if (!is.null(caps)) {
    named_vec <- toupper(caps)
    names(named_vec) <- tools::toTitleCase(tolower(caps))
    labels <- stringr::str_replace_all(tools::toTitleCase(labels), named_vec)
  }

  return(labels)
}


names_to_labels(iris)

