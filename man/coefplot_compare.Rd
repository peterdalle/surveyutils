% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coefplot.r
\name{coefplot_compare}
\alias{coefplot_compare}
\title{Coefficient plot for comparison of two regression models}
\usage{
coefplot_compare(
  model1,
  model2,
  modelnames = NULL,
  intercept = FALSE,
  sort = FALSE,
  title = "Predictors",
  subtitle = NULL,
  caption = NULL,
  labels1 = NULL,
  labels2 = NULL,
  reverse_labels = FALSE,
  nudge_x = 0,
  nudge_y = 0.3,
  dodge = 0.5,
  conf.level = 0.95,
  highlight.significant = FALSE
)
}
\arguments{
\item{model1}{model 1.}

\item{model2}{model 2.}

\item{modelnames}{vector (of length 2) with model names.}

\item{intercept}{whether the intercept should be included in the plots
(default is FALSE).}

\item{sort}{whether the estimates should be sorted in descending order
(default is FALSE).}

\item{title}{title of the plot.}

\item{subtitle}{sub title of the plot.}

\item{caption}{caption of the plot.}

\item{labels1}{vector with label names for model 1. Should be the same length
as the number of estimates in the model, including the intercept.}

\item{labels2}{vector with label names for model 2. Should be the same length
as the number of estimates in the model, including the intercept.}

\item{reverse_labels}{whether or not to reverse the label ordering.}

\item{nudge_x}{nudge the point label estimates on the X axis so that they do
not overlap the point.}

\item{nudge_y}{nudge the point label estimates on the Y axis so that they do
not overlap the point.}

\item{dodge}{numeric value that separate the two models from each other.}

\item{conf.level}{confidence level of the error bars (defaults to 0.95).}

\item{highlight.significant}{whether or not significant findings should be
highlighted.}
}
\value{
a ggplot2 plot with one coefficient plot with the two models.
}
\description{
Works and is tested with ordinary lm and mixed-effect lme4 models.
}
