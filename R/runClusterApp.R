#' Title
#'
#' @return
#' @export
#'
#' @examples
runClusterApp <- function() {

  appDir <- system.file("shiny-examples", "clusterApp", package = "illustrator")

  if (appDir == "") {
    stop("Could not find clusterApp directory. Try re-installing `illustrator`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")

}
