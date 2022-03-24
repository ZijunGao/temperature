#' temp_app
#'
#' This function calls the visualization app.
#'
#' @param app the name of the app
#' @importFrom shiny runApp
#'
#' @export temp_app
temp_app = function (app)  {
  shiny::runApp(system.file("webapp", app, package = "temperature"))
}
