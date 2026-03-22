#' Launch the WarsawTraffic Shiny Dashboard
#'
#' @description Opens the interactive disruption simulation dashboard
#'   in your browser.
#'
#' @return Does not return a value. Launches the Shiny application.
#' @export
#'
#' @examples
#' \dontrun{
#'   run_app()
#' }
run_app <- function() {
  app_dir <- system.file("shiny", package = "WarsawTraffic")
  if (app_dir == "") {
    stop("Could not find the Shiny app directory. Try re-installing the package.")
  }
  shiny::runApp(app_dir, display.mode = "normal")
}
