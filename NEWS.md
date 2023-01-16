# utile.tools (Development)

## Breaking Changes

* `test_hypothesis()` has been overhauled to allow selection of the test to be used
and return of a list which includes the test name.

## New Features

* P-value formatting has been moved to an exported function, `paste_pval()`.

## Fixes & Minor Changes

* `paste_efs` now sets the names of the returned vector using the times provided.

* Created a `pkgdown` website for the package.


# utile.tools 0.2.8

* Transitioned to `inherits` inside `paste_efs`.

* Fix to `calc_duration` casting for upcoming purrr 1.0.0 release (Thanks Hadley!)


# utile.tools 0.2.7

* Fixed faulty class check in `test_hypothesis` and updated documentation for clarity.

* Added `calc_cumsum` which provides additional NA handling when calculating cummulative sums.

* Fixed LazyData CRAN error.

* Added `dplyr` to suggests.


# utile.tools 0.2.6

* The final tabulate function, `tabulate_at_risk`, has been removed after being internalized by `utile.visuals` package.

* Unused `dplyr` dependency removed.

* `vctrs` dependency added.

* Fixed incorrect estimate rounding in <1 checks for `paste_median` and `paste_mean`.

* Fixed issue where `test_hypothesis` would not accept logical stratification data.

* Added more reliable type checking and recycling to `calc_duration`.

* All functions now use a consistent x/y argument style.


# utile.tools 0.2.5

* Added `paste()` & `paste0()`. Base functions with added option for NA removal. Default behavior 
identical to base functions.

* Removed `tabulate_model()`. Feature parity has been provided in `utile.tables::build_table()`.

* Pruned dependency list.

* Overhauled documentation.


# utile.tools 0.2.4

* `calc_duration()`: Now supports the return of a `lubridate::duration()` object when the units parameter is left unspecified.


# utile.tools 0.2.3

* Improved NA value handling of `calc_duration()`.

* `paste_freq()`:
  - Added support for tallying factors.
  - Removed default rounding of numerators <1 to 0.
  - Calculated percentages may now return as infinity.

* `paste_` functions: Fixed parameter naming [BREAKING CHANGE]

* Consolidated `test_` functions into `test_hypothesis()` which has S3 methods for numeric, factor, and logical data. Numeric methods now support >2 strata. [BREAKING CHANGE]

  
# utile.tools 0.2.2

* Added `calc_chunks()` & `chunk_data_()`. Tools for breaking tibbles, data frames, and vectors into smaller, usable chunks of data.

* `tabulate_model()`: Formatted tables now combine the point estimate and confidence interval into a single column to be more consistent with the output of `utile.tables::` functions.

* `paste_freq()`: Non-numeric data is now tallied and documentation has been updated.

* `paste_mean()`/`paste_median()`/`paste_efs()`: Added checks for numeric data.


# utile.tools 0.2.1

* `calc_duration()`: Added support for multiple calculations against a single timepoint.


# utile.tools 0.2.0

* `tabulate_model()`: Rethought the purpose of this function.
  - Added `format=` option to return a human-readable, ready-to-export table.
  - Added `tabulate_model.glm()` which only supports the 'binomial' family of glm functions for now.
  - Added `tabulate_model.lm()` for linear regression model support.
  - Removed `tabulate_model.survfit()` as other packages (i.e. 'broom') already provide this functionality.
  - Refactored function design to make adding new methods easier.

* `tabulate_logit()`: Removed now that support has been added to `tabulate_model()`.

* `tabulate_at_risk()`: Now returns the strata column as factor instead of character type.

* `paste_freq()`: Switched from an S3 object to a regular function.
  - Fixed NA handling.
  - Added support for vectorized data (i.e. `dplyr::mutate()`).
  - Removed support for logical data. This seemed to be an edge case and was interfering with NA handling. May reconsider in future.

* `paste_median()`/`paste_mean()`: Fixed logic for returning '<1' if a statistic rounds to 0 and `less.than.one=TRUE`.

* `calc_duration()`:
  - Fixed NA handling.
  - Added hard stops for data types and vectors lengths.
  - Improved handling of vectorized data (i.e. `dplyr::mutate()`).
  - Added logic to ignore timestamps if mixed data types given (i.e. start = POSIXt, end = Date).
  - Removed rounding of negative numbers to 0.


# utile.tools 0.1.2
* First public release
