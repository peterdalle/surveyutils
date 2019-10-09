# surveyutils

R package with various utility and helper functions for managing survey data.

*Note*: This package is for internal use, and may not be developed or maintained further and is expected to change haphazardly.

## Install

```r
devtools::install_github("peterdalle/surveyutils")
```

## Public functions

Function | Description
:-------------- | :----------------------------
`coefplot_compare()` | Coefficient plot that compare two regression models
`confidence_interval()` | Get confidence interval for mean, standard devation and number of observations
`convert_scale()` | Converts a scale range to another scale range (e.g., 1-7 to 1-100)
`corr_histogram()` | Create histogram of correlations
`normalize()` | Normalize vector
`reverse_code()` | Reverse code values in a numeric vector (e.g., 1-7 to 7-1)