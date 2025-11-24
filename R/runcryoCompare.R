#' importFrom shiny runApp

runcryoCompare <- function() {
  appDir <- system.file("shiny-scripts", package = "cryoCompare")
  actionShiny <- shiny::runApp(appDir, display.mode = "normal")

  return(actionShiny)
}

# [END]

