# surveyutils

R package with various utility and helper functions for managing survey data.

- convert scales
- reverse code scales
- create regression coefficient plots
- R code generator that can create R code correlations from a data frame in order to fast and easy reproduce SEM models

*Note*: This package is for internal use, and may not be developed or maintained further.

## Install

```r
devtools::install_github("peterdalle/surveyutils")
```

## Functions

Only major functions described here.

Function | Description
:-------------- | :----------------------------
`coefplot_compare()` | Coefficient plot that compare two regression models
`convert_scale()` | Converts a scale range to another scale range (e.g., 1-7 to 1-100)
`generate_corr_code()` | Takes a data frame and generate R code that create a data frame with correlations/covariances
`reverse_code()` | Reverse code values in a numeric vector (e.g., 1-7 to 7-1)