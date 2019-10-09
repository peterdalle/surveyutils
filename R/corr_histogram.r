#' Create histogram of correlations
#'
#' Create histogram of correlations between variables in a data frame.
#'
#' @param df data frame with numeric variables that should be correlated.
#' @param use (defalt is "complete.obs").
#'
#' @return a ggplot2 plot with a histogram with correlations.
#' @export
#'
#' @examples
corr_histogram <- function(df, use="complete.obs") {
  # Make correlations from .
  df <- df %>%
    cor(use=use) %>%
    .[lower.tri(.)] %>%
    matrix() %>%
    as.data.frame()
  df %>%
    ggplot2::ggplot(aes(V1)) +
    ggplot2::geom_histogram(fill="#D3CE97", bins = 100) +
    ggplot2::scale_x_continuous(breaks = seq(-1, 1, by=0.5),
                                limits = c(-1, 1)) +
    ggplot2::labs(title="Histogram of correlations",
         subtitle = paste0("mean r = ", round(mean(df$V1), 2),"
                           (", round(min(df$V1), 2), " to ",
                           round(max(df$V1), 2), ")"),
         x = "r",
         y = "Frequency")
}
