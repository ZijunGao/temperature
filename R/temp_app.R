#' temp_app
#'
#' This function calls the visualization app. The app takes in the covariates of a subject (gender, age, weight, height, temperature) and outputs the 1\%, 5\%, 25\%, 50\%, 75\%, 95\%, 99\% percentiles of the body temperature from 7:00 to 18:00.
#'
#' @import shiny
#' @import plotly
#' @import dplyr
#' @import hms
#'
#' @export temp_app
temp_app = function ()  {
    shiny::runApp(system.file("webapp", package = "temperature"))
}
