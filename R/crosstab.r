#' Cross tabulation  of two variables
#'
#' Prints or returns a frequency table from a vector or a data frame.
#'
#' @param x vector.
#' @param y vector (grouping variable).
#' @param digits round percentage to this number of digits.
#' @param na.rm a logical value indicating whether NA values should be
#' stripped before the computation proceeds.
#'
#' @return a tibble is returned if the input (`x`) is a vector. If the input is
#' a data frame, then frequencies for each column is printed on the screen.
#' @export
#'
#' @examples
#' \donotrun{
#' crosstab(iris$Petal.Width, iris$Species)
#' }
crosstab <- function(x, y, digits=1, na.rm=FALSE, labels=TRUE) {
  if (!(is.numeric(x) | is.factor(x) | is.character(x) | is.data.frame(x))) {
    stop(paste0("Cannot handle x variable of type ", class(x), "."),
         call. = FALSE)
  }
  if (!(is.numeric(y) | is.factor(y) | is.character(y) | is.data.frame(y))) {
    stop(paste0("Cannot handle y variable of type ", class(y), "."),
         call. = FALSE)
  }
  if (na.rm) {
    # Remove NA's.
    x <- x[!is.na(x)]
  }
  return(data.frame(x = x, y = y) %>%
           dplyr::group_by(y) %>%
           dplyr::summarise(n = n()) %>%
           dplyr::mutate(cum_n = (cumsum(n)),
                         percent = round((n / sum(n) * 100), digits),
                         cum_percent = round(cumsum(freq = n / sum(n) * 100),
                                             digits)))
}
