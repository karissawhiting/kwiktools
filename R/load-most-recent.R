

#' Wrapper for `load()` and `readr::read_csv()` that
#' saves a RData or csv with today's date appended to filename
#'
#' @param directory directory to look for most recent data files in
#' @param filename_keyword a file name (e.g. "data.RData") or
#' keyword (e.g. "patient-data"). If provided, function will only consider files
#'  in that directory with that name or keyword
#' @param date_in_filename If `TRUE`, function will use dates in filenames to sort.
#' If `FALSE` function will use the date the file was last updated to sort.
#' `TRUE` by default.
#'
#' @examples
#' save_date(df, here::here("data", "patients.RData"))
#' @export
#'

load_most_recent <- function(directory,
  filename_keyword = NULL,
  date_in_filename = TRUE) {

  filepaths <- list.files(directory, full.names = TRUE)

  if (!is.null(filename_keyword)) {

     filepaths <- filepaths %>%
       purrr::keep(.,  ~str_detect(basename(.x), filename_keyword))
  }

  if(date_in_filename == TRUE) {
    tryCatch(
      {
      order_index <- gsub("^[^0-9]+\\_|\\.[A-Za-z]+$", "",
        basename(filepaths)) %>%
        suppressWarnings(lubridate::ymd(.))
      },
      error = function(e) {
        message(glue::glue("{sum(is.na(order_index))} filenames in {basename(directory)} directory did not have parsable dates"))
      })


    order_index <- order(order_index)
    filepaths = filepaths[order_index]
    most_recent <- filepaths[1]

  } else {

  # if specified not to use filename use most recent edited date
    details = file.info(filepaths)
    details = details[with(details, order(as.POSIXct(mtime), decreasing = TRUE)), ]
    most_recent = rownames(details)[1]
  }

 # get extension
  pos <- regexpr("\\.([[:alnum:]]+)$", most_recent)
  ext <- ifelse(pos > -1L, substring(most_recent, pos + 1L), "")

  if(tolower(ext) != "rdata") {
    stop(glue::glue("{most_recent} must be an .RData file"))
  }

  load(file = most_recent,
    envir = .GlobalEnv)

  message(glue::glue("{most_recent} was loaded"))

}

