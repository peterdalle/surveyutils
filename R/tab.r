#' Frequency of a variable
#'
#' Prints or returns a frequency table from a vector or a data frame.
#'
#' Similar to the frequency table in SPSS, STATA (`tab` command), or
#' `pd.value_counts()` in Pandas for Python.
#'
#' @param x vector or data frame.
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
#' tab(iris)
#' tab(iris$Species)
#'
#' frequency <- tab(iris$Species, digits = 0)
#' frequency$percent
#' }
tab <- function(x, digits=1, na.rm=FALSE, labels=TRUE) {
  if (!(is.numeric(x) | is.factor(x) | is.character(x) | is.data.frame(x))) {
    stop(paste0("Cannot handle variable of type ", class(x), "."),
         call. = FALSE)
  }
  # Return frequencies for vector.
  tab_vector <- function(x, digits, na.rm, labels) {
    if (na.rm) {
      # Remove NA's.
      x <- x[!is.na(x)]
    }
    #if (!labels & is.factor(x)) {
      # Use values instead of factor labels.
    #  x <- as.integer(x)
    #} else {
      #x <- paste0(as.integer(x), ". ", x)
    #}
    return(data.frame(value = x) %>%
             dplyr::group_by(value) %>%
             dplyr::summarise(n = n()) %>%
             dplyr::mutate(cum_n = (cumsum(n)),
                           percent = round((n / sum(n) * 100), digits),
                           cum_percent = round(cumsum(freq = n / sum(n) * 100),
                                               digits)))
  }
  if (is.data.frame(x)) {
    # For a data frame, print frequencies one column at a time,
    num_columns <- length(colnames(x))
    for (i in seq.int(num_columns)) {
      cat(paste0(deparse(substitute(x)), "$", names(x[i]), "\n"))
      print(tab_vector(x[, i], digits, na.rm, labels))
      if (i < num_columns) {
        cat("\n")
      }
    }
  } else {
    # For a vector, return frequencies.
    return(tab_vector(x, digits, na.rm, labels))
  }
}
