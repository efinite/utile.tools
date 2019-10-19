# utile.tools 0.2.2
* Added `calc_chunks()` & `chunk_data_()`. Tools for breaking tibbles, data frames, and vectors into smaller, usable chunks of data.
* Formatted tables created with `tabulate_model()` now have the point estimate and confidence interval in a single column to be more consistent with the output of `utile.tables::`

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
