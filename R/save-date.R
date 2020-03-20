#' Wrapper for `save()` and `readr::write_csv()` that
#' saves a RData or csv with today's date appended to filename
#'
#' @param object the name of the object to be saved
#' @param path the path of the data as you would normally provide it to `save()`
#' or `readr::write_csv()`
#' @examples
#' save_date(df, here::here("data", "patients.RData"))
#' @export
#'

save_date <- function(object, path) {

  filename <- basename(path)

  # get extension
  pos <- regexpr("\\.([[:alnum:]]+)$", filename)
  ext <- ifelse(pos > -1L, substring(filename, pos), "")

  filename_no_ext <- filename %>%
    stringr::str_remove(., ext)

  # construct new file path
  formatted_date <- format(Sys.Date(),"%Y-%m-%d")
  new_filename <- paste0(filename_no_ext, "_", formatted_date, ext)
  new_path <- stringr::str_replace(path, filename, new_filename)

  if(tolower(ext) == ".rdata") {
    save(object, file = new_path)
  }

  if(tolower(ext) == ".csv") {
    readr::write_csv(object, path = new_path)
  }

}

