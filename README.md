# utile.tools
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/utile.tools)](https://CRAN.R-project.org/package=utile.tools)
[![Total Downloads](https://cranlogs.r-pkg.org/badges/grand-total/utile.tools)](https://CRAN.R-project.org/package=utile.tools)

## Overview
Convenience utilities for formatting and summarizing data for outcomes research.

### paste_
Functions such as `paste_freq()` and `paste_mean()`, which return formatted statistics for writing.

### calc_
Convenience functions for frequently used calculations, such as the duration of time between two date objects with `calc_duration()`.

### cusum_
Functions which take a dichotomous procedure outcome and return prepared data for producing CUSUM curves. Available options range from simple cumulative sum of failures with `cusum_failure()` to risk-adjusted sequential probability ratio tests with `cusum_sprt()`.

### test_
Simple null hypothesis testing of stratified continuous or nominal data with the `test_hypothesis()` function. Returns a list containing test results.
