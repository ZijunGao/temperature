#' temperature.to.percentile
#'
#' This function computes the percentile corresponding to a body temperature for a given subject.
#'
#' @importFrom stats approx
#'
#' @param data a data.frame recording covariates of subjects, and each row represents a subject. The \code{data} should contain at least six named columns:
#' \itemize{
#' \item temperature: temperature of the subject (in Fahrenheit). The \code{temperature} should be numeric.
#' \item time: the time when the temperature is taken. The \code{time} should be a character of the format hh:mm, e.g., "08:30" and "12:00". The \code{time} should be no earlier than 07:00 or later than 18:00 within a day.
#' \item gender: gender of the subject. The \code{gender} should be a character. There are three possible values: "male", "female.pre" for ladies before menopause, and "female.post" for ladies after menopause. If the menopause condition in not available, a default rule regards female no younger than 40 as post-menopause.
#' \item age: age of the subject. The \code{age} should be numeric and will be rounded to the nearest integer. The \code{age} should be no smaller than 20 or larger than 80.
#' \item height: height of the subject (in meter). The \code{height} should be numeric and will be rounded to two decimal places. The \code{height} should be no smaller than 1.38 or larger than 2.13.
#' \item weight: weight of the subject (in kilogram). The \code{weight} should be numeric and will be rounded to the nearest integer. The \code{weight} should be no smaller than 30 or larger than 181.
#' }
#'
#' @return The function returns of a vector representing the percentiles corresponding to the given temperatures. We use 101 or -1 to denote a temperature that is above the 99% percentile or below the 1% percentile.
#'
#' @examples
#' data = data.frame(temperature = c(90, 98.5, 105),
#' time = c("08:00", "12:00", "14:00"),
#' gender = c("male", "female.pre", "female.post"),
#' age = c(30, 30, 70),
#' height = c(1.8, 1.78, 1.6),
#' weight = c(70, 60, 50.5))
#' temperature.to.percentile(data)
#'
#' @export
temperature.to.percentile = function(data){
  # preprocess
  if(!"data.frame" %in% class(data)){stop("The input should be a data frame.")}
  if(min(c("temperature", "time", "gender", "age", "height", "weight") %in% colnames(data)) == 0){stop("The data should contain columns with names 'temperature', 'time', 'gender', 'age', 'height', and 'weight'.")}
  # rounding
  data$age = round(data$age)
  data$height = round(data$height, digits = 2)
  data$weight = round(data$weight)

  # temperature to percentile
  quantileCurves = temperature::baseline[data$time,] + temperature::gender[data$gender,] + temperature::age[as.character(data$age),] + temperature::height[as.character(data$height),] + temperature::weight[as.character(data$weight),]
  result = apply(cbind(data$temperature, quantileCurves), 1, function(input){approx(x = input[-1], y = seq(1,99), xout = input[1], method = "linear", rule = 1, ties = mean)$y})

  # extreme values
  result[data$temperature < quantileCurves[,1]] = -1 # smaller than 1%
  result[data$temperature > quantileCurves[,99]] = 101 # larger than 99%

  names(result) = NULL; result = round(result)
  return(result)
}
