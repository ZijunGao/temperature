#' Gender effect on body temperature
#'
#' A dataset describing how gender affects the distribution (quantiles) of body temperature.
#'
#' @format A data frame with 3 rows and 99 columns. Each row stands for a gender group (male, female.pre for female before menopause, and female.post for female after menopause) and each column stands for a quantile level (from 1\% to 99\%). The number corresponding to gender \code{X} and quantile \code{Y} denotes the deviation from the baseline (stored in the dataset _baseline_) of a subject with gender \code{X} in the \code{Y} quantile of his or her body temperature.
#'
#' #' @source the dataset of gender effect is extracted from an additive quantile regression model of body temperature.
"gender"
