#' Lancement de l'app Shiny
#' @export
runApp <- function(){
  appDir <- system.file("app", package = "rgonomie")
  shiny::runApp(appDir, display.mode = "normal")
}
