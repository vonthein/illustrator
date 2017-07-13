#' Title
#'
#' @return
#' @export
#'
#' @examples
runForestApp <- function() {

  appDir <- system.file("shiny-examples", "forestApp", package = "illustrator")

  if (appDir == "") {
    stop("Could not find forestApp directory. Try re-installing `illustrator`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")

}
