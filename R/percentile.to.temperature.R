#' percentile.to.temperature
#'
#' This function computes the body temperature corresponding to a percentile for a given subject.
#' @method compute the body temperature corresponding to a percentile for a given subject.
#'
#' @importFrom utils read.csv
#'
#' @param data data.frame recording the information of subjects, and each row represents a subject. The \code{data} should contain at least six columns:
#' \itemize{
#' \item percentile: percentile to compute temperature at. The \code{percentile} should be numeric. The \code{percentile} should be rounded to the nearest integer. The \code{percentile} should be no smaller than 1 or larger than 99.
#' \item time: the time when the \code{temperature} is computed at. The \code{time} should be a character. The format should be hh:mm, e.g., 08:30 and 12:00. The \code{time} should be no earlier than 07:00 or later than 18:00 within a day.
#' \item gender: genders of subjects. The \code{gender} should be a character. The are three possible values: "male", "female.pre" for ladies before the ceasing of menstruation, and "female.post" for ladies after the ceasing of menstruation.
#' \item age: ages of subjects. The \code{age} should be numeric. The \code{age} should be rounded to the nearest integer. The \code{age} should be no smaller than 20 or larger than 80.
#' \item height: heights of subjects (m). The \code{height} should be numeric. The \code{height} should contain two decimals and trailing zeros should be removed, e.g., 1.78 and 1.6. The \code{height} should be no smaller than 1.38 or larger than 2.13.
#' \item weight: weights of subjects (kg). The \code{weight} should be numeric. The \code{weight} should be rounded to the nearest integer. The \code{weight} should be no smaller than 30 or larger than 181.
#' }
#'
#' @return The function returns of a vector representing the temperatures in Fahrenheit corresponding to the given percentiles.
#'
#' @example
#' data = data.frame(matrix(nrow = 0, ncol = 6))
#' colnames(data) = c("percentile", "time", "gender", "age", "height", "weight")
#' data[1,] = c(50, "08:00", "male", 30, 1.8, 70)
#' data[2,] = c(95, "12:00", "female.pre", 30, 1.7, 60)
#' data[3,] = c(99, "14:00", "female.post", 70, 1.6, 50)
#' percentile.to.temperature(data)
#'
#' @export
#'
percentile.to.temperature = function(data){
  # preprocess
  if(!"data.frame" %in% class(data)){stop("The input should be a data frame.")}
  if(min(c("percentile", "time", "gender", "age", "height", "weight") %in% colnames(data)) == 0){stop("The data should contain columns with names 'percentile', 'time', 'gender', 'age', 'height', and 'weight'.")}
  if(max(data$percentile) > 99){stop("The maximal percentile allowed is 99%.")}
  if(min(data$percentile) < 1){stop("The minimal percentile allowed is 1%.")}
  n = dim(data)[1]

  # read in reference data
  baseline = read.csv(file = "./R/data/baselineFull.csv", row.names = "time")
  age = read.csv(file = "./R/data/ageFull.csv", row.names = "age")
  gender = read.csv(file = "./R/data/genderFull.csv", row.names = "gender")
  height = read.csv(file = "./R/data/heightFull.csv", row.names = "height")
  weight = read.csv(file = "./R/data/weightFull.csv", row.names = "weight")

  # percentile to temperature
  quantileCurves = baseline[data$time,] + gender[data$gender,] + age[as.character(data$age),] + height[as.character(data$height),] + weight[as.character(data$weight),]
  result = quantileCurves[cbind(seq(1,n), as.numeric(data$percentile))]

  names(result) = NULL
  return(result)
}

