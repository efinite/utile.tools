# utile.tools
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/utile.tools)](https://CRAN.R-project.org/package=utile.tools)
[![Total Downloads](https://cranlogs.r-pkg.org/badges/grand-total/utile.tools)](https://CRAN.R-project.org/package=utile.tools)

## Overview
A variety of tools for preparing and summarizing data for publication purposes. Function verbs include 'tabulate' for creating usable tabulated data from models, 'paste' for generating human-readable statistics from a variety of summarizable data types, 'calc' for reliably calculating differences between data points, and 'test' for conducting simple statistical tests which return human-readable results.

## Functions
### > tabulate_
- `tabulate_model()`: Converts parameters from a model object into a usable table for publication purposes. By default, formats the table into a human-readable/exportable form.
- `tabulate_at_risk()`: Returns a risk table from a model object and specified time points.

### > paste_
- `paste_freq()`: Returns a human-readable frequency from count(able) data. Handily has methods for several types of data.
- `paste_median()`: Returns a human-readable median with inter-quartile range from numeric data.
- `paste_mean()`: Returns a human-readable mean with standard deviation from numeric data.
- `paste_efs()`: Returns a human-readable event-free-survival from a survfit object and a specified time point.

### > calc_
- `calc_duration()`: Returns the duration of time between two provided date objects. Essentially a macro of `lubridate::` functions with extra logic built in.
- `calc_chunks()`: Returns mapped "chunk" indices for a data object given a specified chunk size (e.g. number of rows in a tibble).

### > test_
- `test_numeric()`: Returns a p-value from parametric or non-parametric testing of stratified continuous (numeric) data. Supports parametric and non-parametric testing (see docs).
- `test_factor()`: Returns a p-value from parametric or non-parametric testing of stratified categorical (factor) data. Supports parametric and non-parametric testing (see docs).

### > chunk_
- `chunk_data_()`: Returns a factory function which returns chunks of a given data object with successive function calls.
