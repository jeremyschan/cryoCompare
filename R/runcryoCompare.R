#' Launch Shiny App For cryoCompare
#'
#' This function launches the Shiny application for cryoCompare.
#' The application allows a user-friendly interface to perform denoising and segmentation
#' on tomogram images using various algorithms provided in the cryoCompare package rather
#' than having to work directly in the R console. The code for the Shiny app has been
#' placed in \code{./inst/shiny-scripts/}.
#'
#' @return Launches the Shiny application for cryoCompare.
#'
#' @examples
#' \dontrun{
#' cryoCompare::runcryoCompare()
#' }
#'
#' @references
#' Tabsets. (n.d.) Shiny. https://shiny.posit.co/r/gallery/application-layout/tabsets/
#' conditionalPanel demo. (n.d.) Shiny. https://shiny.posit.co/r/gallery/dynamic-user-interface/conditionalpanel-demo/
#' File Upload. (n.d.) Shiny. https://shiny.posit.co/r/gallery/widgets/file-upload/
#'
#' @export
#' @importFrom shiny runApp

runcryoCompare <- function() {
  appDir <- system.file("shiny-scripts", package = "cryoCompare")
  actionShiny <- shiny::runApp(appDir, display.mode = "normal")

  return(actionShiny)
}

# [END]

