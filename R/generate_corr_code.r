#' Generate R code that creates a correlation matrix
#'
#' Generate R code from a data frame or matrix.
#' This is useful, for example, when you need to create a correlation
#' matrix that was used by a structural equation model for reproducable
#' research.
#'
#' You supply a data frame with numeric variables (or a matrix with
#' correlation/covariance). The function then creates code that
#' can reproduce the data as a correlation matrix or a covariance matrix.
#'
#' @param x a data frame with numeric variables, or a matrix with correlations
#' or covariances.
#' @param digits number of decimal digits. Default (NULL) will show all
#' available digits as specified by R options.
#' @param early_line_break use early line break so that each value is shown
#' on a single line instead of several. Defaults to FALSE.
#' @param method method for creating the matrix, either "cor" for correlations
#' (default) or "cov" for covariances. This argument is only used when
#' @param target_variable target variable name for the code. Defaults to "df".
#' "x" is a data frame.
#' @return R code that reproduce the correlation matrix of the data frame.
#' @export
#'
#' @examples
#' \dontrun{
#' # Create random data.
#' set.seed(5543)
#' data <- data.frame(x = rnorm(100),
#'                    y = rnorm(100),
#'                    z = rnorm(100),
#'                    w = rnorm(100),
#'                    q = rnorm(100))
#'
#' # Generate R code for the data.
#' cat(generate_corr_code(data))
#'
#' # Generate R code for the data, with 3 decimal digits.
#' cat(generate_corr_code(data, digits=3))
#'
#' # Generate the same code, but keep each value on one line.
#' cat(generate_corr_code(data, digits=3, early_line_break=TRUE))
#'
#' # Save R code to a file.
#' code <- generate_corr_code(data)
#' fileConn <- file("create_data.r")
#' writeLines(code, fileConn)
#' close(fileConn)
#' }
generate_corr_code <- function(x, digits=NULL, early_line_break=FALSE, method="cor",
                               target_variable="df") {
  if ("matrix" %in% class(x)) {
    cor_mat <- x
  } else if ("data.frame" %in% class(x)) {
  } else {
    stop("x must be of type 'matrix' or 'data.frame'.")
  }

  # Create correlation matrix or covariance matrix.
  if ("data.frame" %in% class(x)) {
    if (method == "cor") {
      cor_mat <- cor(x, use="complete.obs")
    } else if (method == "cov") {
      cor_mat <- cov(x, use="complete.obs")
    } else {
      stop("Method must be 'cor' or 'cov'.")
    }
  }

  # Code line indent values.
  indent <- paste0(rep(" ", 14), collapse = "")

  # Generate variable names.
  var_names <- ""
  for (variable in row.names(cor_mat)) {
    var_names <- stringi::stri_c(var_names, "~", variable, ", ")
  }
  var_names <- base::substr(var_names, 1, nchar(var_names) - 2)

  # Generate values.
  values <- ""
  for (i in seq.int(ncol(cor_mat))) {
    if (!early_line_break) {
      values <- stringi::stri_c(values, indent, sep="")
    }
    for (j in seq(ncol(cor_mat))) {
      if (is.null(digits)) {
        value <- cor_mat[i, j]
      } else {
        value <- round(cor_mat[i, j], digits=digits)
      }
      values <- stringi::stri_c(values, value, ", ", sep="")
      if (early_line_break) {
        values <- stringi::stri_c(values, "\n")
      }
    }
    if (!early_line_break) {
      values <- stringi::stri_c(values, indent, "\n")
    }
  }
  if (early_line_break) {
    values <- substr(values, 1, nchar(values) - 3)
  } else {
    values <- substr(values, 1, nchar(values) - 17)
  }

  # Code template.
  code <- "library(tibble)
%%TARGET%% <- tribble(%%VARIABLES%%,
%%VALUES%%)"

  # Replace placeholders with variable names and variable values.
  code <- sub("%%TARGET%%", target_variable, code)
  code <- sub("%%VARIABLES%%", var_names, code)
  code <- sub("%%VALUES%%", values, code)
  return(code)
}
