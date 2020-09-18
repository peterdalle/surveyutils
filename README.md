# surveyutils

R package with various utility and helper functions for managing survey data.

*Note*: This package is for internal use. It may or may not be developed or maintained in the future.

## Install

```r
devtools::install_github("peterdalle/surveyutils")
```

## Public functions

Function | Description
:-------------- | :----------------------------
`coefplot()` | Coefficient plot
`coefplot_compare()` | Coefficient plot that compare two regression models
`confidence_interval()` | Get confidence interval for mean, standard devation and number of observations
`convert_scale()` | Converts a scale range to another scale range (e.g., 1-7 to 1-100)
`corr_histogram()` | Create histogram of correlations
`normalize()` | Normalize vector
`reverse_code()` | Reverse code values in a numeric vector (e.g., 1-7 to 7-1)
`crosstab()` | tabulate two variables
`tab()` | tabulate a single variable

### ggplot themes

Function | Description
:-------------- | :----------------------------
`theme_powerpoint()` | 
`theme_powerpoint_black()` |
`theme_publish()` |
`theme_stata()` |
`theme_economist()` |
`theme_pinkygray()` |
`theme_green()` |
`theme_boldandblue()` |
`theme_template()` |
`plot_theme_chart()` |
`plot_theme_modifier_chart()` |

### ggplot theme modifiers

Function | Description
:-------------- | :----------------------------
`theme_larger()` | Make the font size larger
`theme_smaller()` | Make the font size smaller