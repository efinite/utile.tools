# utile.tools
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/utile.tools)](https://CRAN.R-project.org/package=utile.tools)
[![Total Downloads](https://cranlogs.r-pkg.org/badges/grand-total/utile.tools)](https://CRAN.R-project.org/package=utile.tools)

## Overview
Tools for preparing and summarizing data for publication purposes. Includes functions for tabulating models, producing human-readable summary statistics from raw data, macros for calculating duration of time, and simplistic hypothesis testing tools.

## Verbs
### Tabulate
- `tabulate_model()`: Converts parameters from a model object into a usable table for publication purposes. By default, formats the table into a human-readable/exportable form.
- `tabulate_at_risk()`: Returns a risk table from a model object and specified time points.

### Paste
- `paste()` & `paste0`: The base functions with an added 'na.rm' parameter for nested NA removal. Default function behavior is identical to base counterparts.
- `paste_freq()`: Returns a human-readable frequency from count(able) data. Handily has methods for several types of data.
- `paste_median()`: Returns a human-readable median with inter-quartile range from numeric data.
- `paste_mean()`: Returns a human-readable mean with standard deviation from numeric data.
- `paste_efs()`: Returns a human-readable event-free-survival from a survfit object and a specified time point.

### Calc
- `calc_duration()`: Returns the duration of time between two provided date objects. Essentially a macro of `lubridate::` functions with extra logic built in.
- `calc_chunks()`: Returns mapped "chunk" indices for a data object given a specified chunk size (e.g. number of rows in a tibble).

### Test
- `test_hypothesis()`: Returns a p-value from null hypothesis testing of stratified continuous or categorical data. Provides parametric and non-parametric testing options (see docs).

### Chunk
- `chunk_data_()`: Returns a factory function which returns chunks of a given data object (table, vector) with successive function calls.
