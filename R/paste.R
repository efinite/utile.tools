#' @title Paste frequency
#' @description
#' Creates a human-readable frequency from count(able) data. Automatically
#' tallies non-numeric data types (nrow or length) and supports vectorized data
#' methods.
#' @param x Data.frame, numeric, or non-numeric. The numerator.
#' @param y Data.frame, numeric, or non-numeric. The denominator. A single
#' denominator may be used for multiple numerators or one denominator for each
#' numerator.
#' @param na.rm Logical. Whether to ignore NA's when tallying non-numeric data.
#' @param percent.sign Logical. Indicates percent sign should be printed
#' with frequencies.
#' @param digits Integer. Number of digits to round to.
#' @return A character vector of count(s) with frequencies.
#' @examples
#' # Numeric
#' paste_freq(20, 100)
#'
#' # Tibble
#' df <- data.frame(x = c(1:100), y = TRUE)
#' paste_freq(df[1:20,], df)
#'
#' # Mixed data types
#' paste_freq(20, df)
#'
#' # Single denominator for multiple numerators
#' paste_freq(c(10,20,30), 100)
#' @export
paste_freq <- function(x, y, na.rm = TRUE, percent.sign = TRUE, digits = 1) {

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
#' @param x Required. Numeric. Data to summarize.
#' @param less.than.one Optional. Logical. Indicates a median that rounds to 0 should
#' be printed as <1.
#' @param digits Optional. Integer. Number of digits to round to.
#' @return A character vector of the median(s) with interquartile range(s).
#' @examples
#' paste_median(mtcars$mpg)
#' @export
paste_median <- function(x = NA, less.than.one = FALSE, digits = 1) {
  if (all(is.na(x)) | !all(is.numeric(x))) as.character(NA)
  else {
    estimate <- round(x = stats::median(x, na.rm = TRUE), digits = digits)
    if (round(estimate, digits = digits) == 0 & less.than.one) estimate <- '<1'
    precision <- round(x = stats::quantile(x, probs = c(0.25, 0.75), na.rm = TRUE), digits = digits)
    paste0(estimate, ' [', paste0(precision, collapse = '-'), ']')
  }
}


#' @title Paste mean
#' @description
#' Creates a human-readable mean with standard deviation
#' from numeric data.
#' @param x Required. Numeric. Data to summarize.
#' @param less.than.one Optional. Logical. Indicates a mean that rounds to 0 should
#' be printed as <1.
#' @param digits Optional. Integer. Number of digits to round to.
#' @return A character vector of the mean(s) with standard deviation(s).
#' @examples
#' paste_mean(mtcars$mpg)
#' @export
paste_mean <- function(x = NA, less.than.one = FALSE, digits = 1) {
  if (all(is.na(x)) | !all(is.numeric(x))) as.character(NA)
  else {
    estimate <- round(x = mean(x, na.rm = TRUE), digits = digits)
    if (round(estimate, digits = digits) == 0 & less.than.one) estimate <- '<1'
    precision <- round(x = stats::sd(x, na.rm = TRUE), digits = digits)
    paste0(estimate, ' \u00B1', precision)
  }
}


#' @title Paste event-free survival
#' @description
#' Creates a human-readable event-free-survival from a survfit object
#' and a specified time point.
#' @param fit Required. An object of class '\code{\link[survival]{Surv}}'. The
#' time-to-event model of interest.
#' @param times Required. Numeric. Indicates duration of time-points of interest.
#' Units are whatever was used to create the time-to-event model.
#' @param percent.sign Optional. Logical. Indicates percent sign should be printed
#' for frequencies.
#' @param digits Optional. Integer. Number of digits to round to.
#' @return A character vector of event free survival(s).
#' @examples
#' library(survival)
#'
#' fit <- survfit(Surv(time, status) ~ 1, data = diabetic)
#' paste_efs(fit, c(1, 3, 5))
#' @export
paste_efs <- function(fit = NA, times = NA, percent.sign = TRUE, digits = 1) {
  if (all(is.na(times)) | class(fit) != 'survfit' | !all(is.numeric(times))) as.character(NA)
  else {
    results <- summary(fit, times = times)
    estimate <- round(results$surv * 100, digits = digits)
    lower <- round(results$lower * 100, digits = digits)
    upper <- round(results$upper * 100, digits = digits)
    paste0(estimate, if (percent.sign) '%' else NULL, ' [', lower, '-', upper, ']')
  }
}

#' @name paste
#' @title Concatenate strings
#' @description
#' An augmented version of \code{\link[base:paste]{base::paste()}} with options to
#' manage NA values.
#' @param ... Required. R objects to be converted to character vectors.
#' @param sep Optional. Character. A string to separate the terms.
#' @param collapse Optional. Character. An string to separate the results.
#' @param na.rm Optional. Logical. Whether to remove NA values from 'x'.
#' Note that NA values are also removed from vectors.
#' @details The \code{\link[base:paste]{base::paste()}} function is intentionally
#' designed to coarce NA values to characters that appear in the concatenated
#' character output. This behavior is not always desirable (i.e. when programatically
#' calling paste) and there is currently no means of opting out of this behavior.
#' These augmented functions address this deficit.
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

