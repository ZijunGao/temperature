#' Baseline distribution of body temperature within a day
#'
#' A dataset describing the baseline distribution (quantiles) of body temperature within a day.
#'
#' @format A data frame with 661 rows and 99 columns. Each row stands for a time stamp (from 7:00 to 18:00) and each column stands for a quantile level (from 1\% to 99\%). The number corresponding to time stamp \code{X} and quantile \code{Y} denotes the \code{Y} quantile of body temperature at time \code{X} for a male individual of age 30, height 1.8m, and weight 70kg.
#'
#' #' @source the dataset of baseline distribution is extracted from an additive quantile regression model of body temperature.
"baseline"
