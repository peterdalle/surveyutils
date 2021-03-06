% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tab.r
\name{tab}
\alias{tab}
\title{Frequency of a variable}
\usage{
tab(x, digits = 1, na.rm = FALSE, labels = TRUE)
}
\arguments{
\item{x}{vector or data frame.}

\item{digits}{round percentage to this number of digits.}

\item{na.rm}{a logical value indicating whether NA values should be
stripped before the computation proceeds.}
}
\value{
a tibble is returned if the input (`x`) is a vector. If the input is
a data frame, then frequencies for each column is printed on the screen.
}
\description{
Prints or returns a frequency table from a vector or a data frame.
}
\details{
Similar to the frequency table in SPSS, STATA (`tab` command), or
`pd.value_counts()` in Pandas for Python.
}
\examples{
\donotrun{
tab(iris)
tab(iris$Species)

frequency <- tab(iris$Species, digits = 0)
frequency$percent
}
}
