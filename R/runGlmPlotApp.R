#' Title
#'
#' @return
#' @export
#'
#' @examples
runGlmPlotApp <- function() {

  appDir <- system.file("shiny-examples", "glmplotApp", package = "illustrator")

  if (appDir == "") {
    stop("Could not find glmplotApp directory. Try re-installing `illustrator`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")

}
