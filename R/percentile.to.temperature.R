#' percentile.to.temperature
#'
#' This function computes the body temperature corresponding to a percentile for a given subject.
#'
#' @param data a data.frame recording covariates of subjects, and each row represents a subject. The \code{data} should contain at least six named columns:
#' \itemize{
#' \item percentile: the percentile to compute the temperature at. The \code{percentile} should be numeric and will be rounded to the nearest integer. The \code{percentile} should be no smaller than 1 or larger than 99.
#' \item time: the time to compute the temperature at. The \code{time} should be a character of the format hh:mm, e.g., "08:30" and "12:00". The \code{time} should be no earlier than 07:00 or later than 18:00 within a day.
#' \item gender: gender of the subject. The \code{gender} should be a character. There are three possible values: "male", "female.pre" for ladies before menopause, and "female.post" for ladies after menopause. If the menopause condition in not available, a default rule regards female no younger than 40 as post-menopause.
#' \item age: age of the subject. The \code{age} should be numeric and will be rounded to the nearest integer. The \code{age} should be no smaller than 20 or larger than 80.
#' \item height: height of the subject (in meter). The \code{height} should be numeric and will be rounded to two decimal places. The \code{height} should be no smaller than 1.38 or larger than 2.13.
#' \item weight: weight of the subject (in kilogram). The \code{weight} should be numeric and will be rounded to the nearest integer. The \code{weight} should be no smaller than 30 or larger than 181.
#' }
#'
#' @return The function returns of a vector representing the temperatures in Fahrenheit corresponding to the given percentiles.
#'
#' @examples
#' data = data.frame(percentile = c(50, 95, 99), time = c("08:00", "12:00", "14:00"), gender = c("male", "female.pre", "female.post"), age = c(30, 30, 70), height = c(1.8, 1.78, 1.6), weight = c(70, 60, 50.5))
#' percentile.to.temperature(data)
#'
#' @export
percentile.to.temperature = function(data){
  # preprocess
  if(!"data.frame" %in% class(data)){stop("The input should be a data frame.")}
  if(min(c("percentile", "time", "gender", "age", "height", "weight") %in% colnames(data)) == 0){stop("The data frame should contain named columns 'percentile', 'time', 'gender', 'age', 'height', and 'weight'.")}
  if(max(data$percentile) > 99){stop("The maximal percentile allowed is 99%.")}
  if(min(data$percentile) < 1){stop("The minimal percentile allowed is 1%.")}
  # rounding
  data$percentile = round(data$percentile)
  data$age = round(data$age)
  data$height = round(data$height, digits = 2)
  data$weight = round(data$weight)

  # percentile to temperature
  quantileCurves = baseline[data$time,] + gender[data$gender,] + age[as.character(data$age),] + height[as.character(data$height),] + weight[as.character(data$weight),]
  result = quantileCurves[cbind(seq(1,dim(data)[1]), data$percentile)]

  names(result) = NULL
  return(result)
}

