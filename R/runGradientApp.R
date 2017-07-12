#' Title
#'
#' @return
#' @export
#'
#' @examples
runGradientApp <- function() {

  appDir <- system.file("shiny-examples", "gradientApp", package = "illustrator")

  if (appDir == "") {
    stop("Could not find gradientApp directory. Try re-installing `illustrator`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")

}
