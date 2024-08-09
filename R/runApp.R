
#' Run the Shiny App
#'
#' This function runs the Shiny application.
#' @export
runApp <- function() {
  appDir <- system.file("app", package = "SonicPlots")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `SonicPlots`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
