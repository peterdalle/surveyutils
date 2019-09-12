#' Convert value on a scale to the equivalent of a scale with different range
#'
#' This is useful during analysis to compare scales with different ranges.
#' For example, the value 3.5 on a 5-point Likert scale will be converted to
#' the equivalent of 2.5 on a 7-point Likert scale.
#'
#' @param value value of the original scale (decimal/float).
#' @param from_scale number of points on the original scale (integer).
#' @param to_scale number of points on the target scale (integer).
#' @param digits decimal digits to round the new value (defaults to 3).
#'
#' @return a list of new value, old value, from scale, and to scale.
#' @export
#'
#' @examples
#' # Convert the value 0.66 from a 5-point scale to a 7-point scale.
#' scaleconverter(0.66, from=5, to=7)
#'
#' # And convert the value on 7-point scale back to the 5-point scale.
#' scaleconverter(0.924, from=7, to=5)
#'
#' # Access the values.
#' s = scaleconverter(0.66, from=5, to=7)
#' print(s$newvalue)
convert_scale <- function(value, from_scale, to_scale, digits=3) {
  if(!is.numeric(value) | !is.numeric(from_scale) | !is.numeric(to_scale) | !is.numeric(digits)) {
    stop("All input values must be numeric.")
  }
  return(list(new_value = round(value * (to_scale / from_scale), digits=digits),
              old_value = value,
              from_scale = from_scale,
              to_scale = to_scale))
}
