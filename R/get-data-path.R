
#' Resolve a data file path to server or local location
#'
#' Checks whether the server directory is mounted and returns the
#' appropriate path. If the server is mounted, the server path is returned;
#' otherwise falls back to a local path via \code{here::here()}.
#'
#' The server base path is read from the \code{SERVER_BASE} environment variable
#' (e.g. set in \code{.Renviron} as \code{SERVER_BASE=/Volumes/BSTShared/...}).
#' If the environment variable is not set, \code{server_base} must be supplied
#' directly.
#'
#' @param filename Name of the file (e.g. \code{"patients.RData"}).
#' @param subdir Subdirectory within the repo containing the file.
#'   Defaults to \code{"data"}.
#' @param repo_name Name of the repository folder on the server.
#'   Defaults to the current project root directory name via \code{here::here()}.
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
#' path <- get_data_path("patients.RData", server_base = "/Volumes/BSTShared/...")
#' }
#' @export

get_data_path <- function(filename, subdir = "data", repo_name = basename(here::here()),
                          server_base = Sys.getenv("SERVER_BASE")) {
  if (nzchar(server_base)) {
    server_base <- file.path(server_base, repo_name)
  }
  server_dir  <- file.path(server_base, subdir)

  if (dir.exists(server_dir)) {
    path <- file.path(server_dir, filename)
    message("Resolved to server path: ", path)
    return(path)
  }

  path <- here::here(subdir, filename)
  message("Server not mounted. Resolved to local path (here::here): ", path)
  return(path)
}
