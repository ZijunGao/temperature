#' Weight effect on body temperature
#'
#' A dataset describing how weight affects the distribution (quantiles) of body temperature.
#'
#' @format A data frame with 152 rows and 99 columns. Each row stands for a weight (from 30kg to 181kg) and each column stands for a quantile level (from 1\% to 99\%). The number corresponding to weight \code{X} and quantile \code{Y} denotes the deviation from the baseline (stored in the dataset _baseline_) of a subject with weight \code{X} in the \code{Y} quantile of his or her body temperature.
#'
#' #' @source the dataset of weight effect is extracted from an additive quantile regression model of body temperature.
"weight"
