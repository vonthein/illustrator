#' Title
#'
#' @return
#' @export
#'
#' @examples
runTplotApp <- function() {

  appDir <- system.file("shiny-examples", "TplotApp", package = "illustrator")

  if (appDir == "") {
    stop("Could not find TplotApp directory. Try re-installing `illustrator`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")

}
