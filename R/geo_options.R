# Variable, global to package's namespace.
# This function is not exported to user space and does not need to be documented.
geo_pkg_options <-
  settings::options_manager(
    language = "en",
    theme = "pomological_red",
    mode = "light",
    opacity = 1,
    plot_full_width = 67.73,
    plot_full_height = 38.1
  )

# User function that gets exported:
#' Set or get options for my package
#'
#' @param ... Option names to retrieve option values or \code{[key]=[value]} pairs to set options.
#'
#' @section Supported options:
#' The following options are supported
#' \itemize{
#'  \item{\code{language}}{(\code{character};fr) The value of language }
#' }
#'
#' @export
geo_options <- function(...){
  # protect against the use of reserved words.
  settings::stop_if_reserved(...)
  geo_pkg_options(...)
}
