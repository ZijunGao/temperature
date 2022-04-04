#' temp_app
#'
#' This function calls the visualization app.
#' @importFrom shiny runApp
#' @param app the name of the app
#'
#' @export temp_app·
temp_app = function (app = "webapp")  {
  runApp(system.file(app, package = "temperature"))
}
