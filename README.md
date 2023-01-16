# utile.tools
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/utile.tools)](https://CRAN.R-project.org/package=utile.tools)
[![Total Downloads](https://cranlogs.r-pkg.org/badges/grand-total/utile.tools)](https://CRAN.R-project.org/package=utile.tools)

## Overview
Tools for formatting and summarizing data for publication.

## Verbs
### Paste
- `paste()` & `paste0()`: The base functions with an 'na.rm' parameter for nested NA removal.
- `paste_freq()`: Pastes a formatted frequency from count(able) data.
- `paste_median()`: Pastes a formatted median with inter-quartile range from numeric data.
- `paste_mean()`: Pastes a formatted mean with standard deviation from numeric data.
- `paste_efs()`: Pastes a formatted event-free survival from a survfit object.
- `paste_pval()`: Pastes a formatted p-value from a numeric.

### Calc
- `calc_duration()`: Calculates the duration between two date objects. This is a
macro of `lubridate::` functions with additional logic.
- `calc_chunks()`: Calculates mapped "chunk" indices for a data object given a
specified chunk size (e.g. number of rows in a data.frame).

### Test
- `test_hypothesis()`: Facilitates null hypothesis testing of continuous or nominal
data and returns a list containing test results.

### Chunk
- `chunk_data_()`: Creates a factory function which returns chunks of a given
data object (table, vector) with successive function calls.
