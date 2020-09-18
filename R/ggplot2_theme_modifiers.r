#' Modify existing theme by making the text size larger
#'
#' @param font_size base font size of all text..
#' @param ... additional parameters passed to ggplot2 theme.
#'
#' @return ggplot2 theme.
#' @export
#'
#' @examples
theme_larger <- function(font_size = 20, ...) {
  t <- ggplot2::theme(text = ggplot2::element_text(size = font_size), ...)
  return(t)
}

#' Modify existing theme by making the text size smaller
#'
#' @param font_size base font size of all text..
#' @param ... additional parameters passed to ggplot2 theme.
#'
#' @return ggplot2 theme.
#' @export
#'
#' @examples
theme_smaller <- function(font_size = 10, ...) {
  t <- ggplot2::theme(text = ggplot2::element_text(size = font_size), ...)
  return(t)
}
