#' @title Paste frequency
#' @description
#' Creates a human-readable frequency from count(able) data. Automatically
#' tallies non-numeric data types (nrow or length) and supports vectorized data
#' methods.
#' @param x A data.frame, numeric, or non-numeric. The numerator.
#' @param y A data.frame, numeric, or non-numeric. The denominator. A single
#' denominator may be used for multiple numerators or one denominator for each
#' numerator.
#' @param na.rm A logical. Whether to ignore NA's when tallying non-numeric data.
#' @param percent.sign A logical. Indicates percent sign should be printed
#' with frequencies.
#' @param digits An integer. Number of digits to round to.
#' @return A character vector of count(s) with frequencies.
#' @examples
#' # Numeric
#' paste_freq(20, 100)
#'
#' # data.frame
#' df <- data.frame(x = c(1:100), y = TRUE)
#' paste_freq(df[1:20,], df)
#'
#' # Mixed data types
#' paste_freq(20, df)
#'
#' # Single denominator for multiple numerators
#' paste_freq(c(10,20,30), 100)
#' @export
paste_freq <- function (x, y, na.rm = TRUE, percent.sign = TRUE, digits = 1) {

  # Data aggregation
  x <- .count_items(x, na.rm = na.rm)
  y <- .count_items(y, na.rm = na.rm)

  # Recycle single counts or throw error for mixed types
  common_counts <- vctrs::vec_recycle_common(x = x, y = y)

  # Create frequencies
  purrr::pmap_chr(
    common_counts,
    ~ {
      if (any(is.na(c(.x,.y))) | !any(is.numeric(c(.x,.y)))) as.character(NA)
      else {
        paste0(
          .x,
          ' (',
          round((.x / .y) * 100, digits = digits),
          if (percent.sign & !is.infinite(.x / .y)) '%',
          ')'
        )
      }
    }
  )

}


#' @title Paste median
#' @description
#' Creates a human-readable median with inter-quartile range from numeric data.
#' @param x A numeric. Data to summarize.
#' @param less.than.one A logical. Indicates a median that rounds to 0 should
#' be printed as <1.
#' @param digits An integer. Number of digits to round to.
#' @return A character vector of the median(s) with interquartile range(s).
#' @examples
#' paste_median(mtcars$mpg)
#' @export
paste_median <- function (x, less.than.one = FALSE, digits = 1) {
  if (all(is.na(x)) | !all(is.numeric(x))) as.character(NA)
  else {
    estimate <- round(stats::median(x, na.rm = TRUE), digits = digits)
    precision <- round(
      stats::quantile(x, probs = c(0.25, 0.75), na.rm = TRUE),
      digits = digits)

    if (round(estimate, digits = 0) == 0 & less.than.one) estimate <- '<1'

    paste0(estimate, ' [', paste0(precision, collapse = '-'), ']')
  }
}


#' @title Paste mean
#' @description
#' Creates a human-readable mean with standard deviation
#' from numeric data.
#' @param x A numeric. Data to summarize.
#' @param less.than.one A logical. Indicates a mean that rounds to 0 should
#' be printed as <1.
#' @param digits An integer. Number of digits to round to.
#' @return A character vector of the mean(s) with standard deviation(s).
#' @examples
#' paste_mean(mtcars$mpg)
#' @export
paste_mean <- function (x, less.than.one = FALSE, digits = 1) {
  if (all(is.na(x)) | !all(is.numeric(x))) as.character(NA)
  else {
    estimate <- round(mean(x, na.rm = TRUE), digits = digits)
    precision <- round(stats::sd(x, na.rm = TRUE), digits = digits)

    if (round(estimate, digits = 0) == 0 & less.than.one) estimate <- '<1'

    paste0(estimate, ' \u00B1', precision)
  }
}


#' @title Paste event-free survival
#' @description
#' Creates a human-readable event-free-survival from a survfit object
#' and a specified time point.
#' @param x A \code{\link[survival]{survfit}} object. The survival model.
#' @param times A numeric. Indicates time-points of interest. Units are whatever
#' was used to create the survival fit.
#' @param percent.sign A logical. Indicates percent sign should be printed
#' for frequencies.
#' @param digits Integer. Number of digits to round to.
#' @return A named character vector of event free survival(s).
#' @examples
#' library(survival)
#'
#' fit <- survfit(Surv(time, status) ~ 1, data = diabetic)
#' paste_efs(fit, c(1, 3, 5))
#' @export
paste_efs <- function (x, times, percent.sign = TRUE, digits = 1) {
  if (!all(is.numeric(times)) | vctrs::vec_is_empty(times)) {
    stop('\'times\' not <numeric> or is empty.')
  } else if (!inherits(x, 'survfit') | !(x$type %in% c('right', 'left', 'interval'))) {
    stop('\'x\' not <survfit> or fit does not estimate survival.')
  } else {
    res <- summary(x, times = times)[c('surv', 'lower', 'upper')]
    res <- map(res, ~ round(.x * 100, digits = digits))
    res <- pmap_chr(
      res,
      \(surv, lower, upper) {
        paste0(surv, if (percent.sign) '%', ' [', lower, '-', upper, ']')
      }
    )
    setNames(res, times)
  }
}


#' @title Paste p-value
#' @description
#' Creates a human-readable p.value using sensible defaults for `format.pval()`.
#' @param x A numeric. P-value to format.
#' @param digits A numeric. Number of significant digits to round to.
#' @param p.digits A numeric. Minimum number of digits to right of the decimal
#' point.
#' @example paste_pval(6.112687e-10)
#' @export
paste_pval <- function (x, digits = 1, p.digits = 4) {
  format.pval(pv = x,
              digits = digits,
              eps = 1e-04,
              nsmall = p.digits,
              scientific = F)
}


#' @name paste
#' @title Concatenate strings
#' @description
#' An augmented version of \code{\link[base:paste]{base::paste()}} with options to
#' manage NA values.
#' @param ... R objects to be converted to character vectors.
#' @param sep A character. A string to separate the terms.
#' @param collapse A character. An string to separate the results.
#' @param na.rm A logical. Whether to remove NA values from 'x'.
#' @return Character vector of concatenated values.
#' @seealso \code{\link[base]{paste}}
#' @examples
#' # Base paste() NA handling behavior
#' paste(
#'   'The', c('red', NA, 'orange'), 'fox jumped', NA, 'over the fence.',
#'   collapse = ' '
#' )
#'
#' # Removal of NA values
#' paste(
#'   'The', c('red', NA, 'orange'), 'fox jumped', NA, 'over the fence.',
#'   collapse = ' ',
#'   na.rm = TRUE
#' )

#' @rdname paste
#' @export
paste <- function(..., sep = ' ', collapse = NULL, na.rm = FALSE) {
  x <- list(..., sep = sep, collapse = collapse)
  if (na.rm) x <- purrr::map(x[!is.na(x)], ~ .x[!is.na(.x)])
  do.call(base::paste, x)
}

#' @rdname paste
#' @export
paste0 <- function(..., collapse = NULL, na.rm = FALSE) {
  x <- list(..., collapse = collapse)
  if (na.rm) x <- purrr::map(x[!is.na(x)], ~ .x[!is.na(.x)])
  do.call(base::paste0, x)
}

