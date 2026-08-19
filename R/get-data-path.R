#' Resolve a data file path to server or local location
#'
#' Checks whether the server directory is mounted and returns the
#' appropriate path. If the server is mounted, the server path is returned;
#' otherwise falls back to a local path via \code{here::here()}.
#'
#' The server base path is read from the \code{SERVER_BASE} environment variable
#' (e.g. set in \code{.Renviron} as \code{SERVER_BASE=/path/to/server/share}).
#' If the environment variable is not set, \code{server_base} must be supplied
#' directly.
#'
#' @param filename Name of the file (e.g. \code{"patients.RData"}).
#' @param subdir Subdirectory within the repo containing the file.
#'   Defaults to \code{"data"}.
#' @param repo_name Name of the repository folder on the server.
#'   Defaults to the current project root directory name via \code{here::here()}.
#' @param data_date Optional date string (e.g. \code{"2026-08-19"}) to insert
#'   into \code{filename} just before its extension, regardless of extension
#'   type (\code{.rds}, \code{.RData}, \code{.csv}, \code{.xlsx}, etc.).
#' @param server_base Base path to the server. Defaults to the value of the
#'   \code{SERVER_BASE} environment variable.
#'
#' @return A character string with the resolved file path.
#'
#' @examples
#' \dontrun{
#' # With SERVER_BASE set in .Renviron:
#' path <- get_data_path("patients.RData")
#'
#' # Or pass the base path directly:
#' path <- get_data_path("patients.RData", server_base = "/path/to/server/share")
#' }
#' @export

get_data_path <- function(
  filename,
  subdir = "data",
  repo_name = basename(here::here()),
  data_date = NULL,
  server_base = Sys.getenv("SERVER_BASE")
) {

  if (nzchar(server_base)) {
    server_base <- file.path(server_base, repo_name)
  }

  server_dir <- file.path(server_base, subdir)

  # insert data_date before the file extension, if specified
  if (!is.null(data_date)) {
    known_extensions <- c(
      "RData", "rds", "csv", "tsv", "txt", "md", "Rmd",
      "xlsx", "xls", "sav", "dta", "parquet", "feather", "json"
    )
    ext_pattern <- paste0("(\\.(", paste(known_extensions, collapse = "|"), "))$")

    if (grepl(ext_pattern, filename, ignore.case = TRUE)) {
      filename <- sub(ext_pattern, paste0("_", data_date, "\\1"), filename, ignore.case = TRUE)
    } else {
      warning("Unrecognized file extension in '", filename, "'. Appending date to the end of the filename.")
      filename <- paste0(filename, "_", data_date)
    }
  }

  if (dir.exists(server_dir)) {
    path <- file.path(server_dir, filename)
    message("Resolved to server path: ", path)
    return(path)
  }

  path <- here::here(subdir, filename)
  message("Server not mounted. Resolved to local path (here::here): ", path)
  return(path)
}
