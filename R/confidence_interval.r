#' Get confidence interval for mean, standard devation and number of
#' observations
#'
#' @param m mean.
#' @param s standard deviation of the mean.
#' @param n number of observations.
#' @param conf.int confidence interval (defaults to 0.95).
#'
#' @return a list with mean, sd, error, confidence interval, lower bound of
#' confidence interval (lower), and upper bound of confidence interval (upper).
#' @export
#'
#' @examples
confidence_interval <- function(m, s, n, conf.int=0.95) {
  p <- (1 - (1 - conf.int) / 2)
  error <- qnorm(p) * s / sqrt(n)
  lower <- m - error
  upper <- m + error
  return(list(mean = m,
              sd = s,
              conf.int = conf.int,
              error = error,
              lower = lower,
              upper = upper))
}
