#' Title
#'
#' @return
#' @export
#'
#' @examples
runBattleshipApp <- function() {

  appDir <- system.file("shiny-examples", "battleshipApp", package = "illustrator")

  if (appDir == "") {
    stop("Could not find battleshipApp directory. Try re-installing `illustrator`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")

}
