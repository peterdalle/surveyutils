#' Reverse code a numeric vector
#'
#' Both minwarn and maxwarn are useful to constrain the variable to not
#' overflow its boundaries, and throw an error if it occurs.
#'
#' @param x a numeric vector.
#' @param minwarn a minimum threshold of vector where the function will break.
#' @param maxwarn a maximum threshold of vector where the function will break.
#'
#' @return a vector with values reversed.
#' @export
#'
#' @examples
reverse_code <- function(x, minwarn=NULL, maxwarn=NULL) {
  x_new <- min(x, na.rm=TRUE) + max(x, na.rm=TRUE) - x

  # Check for errors.
  if(!is.null(minwarn)) {
    if(min(x_new, na.rm=TRUE) < minwarn) {
      stop(paste0("You set the minimum threshold to ", minwarn,
                  ", but the value is below it: ", min(x_new, na.rm=TRUE) ,"."))
    }
  }
  if(!is.null(maxwarn)) {
    if(max(x_new, na.rm=TRUE) > maxwarn) {
      stop(paste0("You set the maximum value to ", maxawn,
                  ", but the variable value is above it: ",
                  max(x_new, na.rm=TRUE) ,"."))
    }
  }
  return(x_new)
}
