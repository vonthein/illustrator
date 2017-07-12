#' Title
#'
#' @return
#' @export
#'
#' @examples
runPlanApp <- function() {

  appDir <- system.file("shiny-examples", "planApp", package = "illustrator")

  if (appDir == "") {
    stop("Could not find planApp directory. Try re-installing `illustrator`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")

}
