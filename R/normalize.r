#' Normalize vector
#'
#' @param x vector to normalize.
#' @param na.rm whether or not to remove NA values. Default is TRUE.
#'
#' @return a vector with the values normalized.
#' @export
#'
#' @examples
#'\donotrun{
#'x <- rnorm(50, 100)
#'normalize(x)
#'}
normalize <- function(x, na.rm=TRUE) {
  return((x - min(x, na.rm=na.rm)) / (max(x, na.rm=na.rm) - min(x, na.rm=na.rm)))
}