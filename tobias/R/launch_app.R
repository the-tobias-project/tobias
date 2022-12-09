#' Launch Tobias
#' @param launch.browser Launch in browser? Default TRUE
#' @param ... Additional parameters passed to \code{\link[shiny]{runApp}}
#' @export

launch_tobias <- function(browser = TRUE, ...) {
  fpath <- system.file("application", package = "tobias")
  shiny::runApp(fpath, launch.browser = browser, ...)
}
