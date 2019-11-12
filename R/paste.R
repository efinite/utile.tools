#' @title Paste Frequency
#' @description Returns a human-readable frequency from count(able) data. Supports
#' vectorized data (i.e. dplyr::mutate()).
#' @param num Optional. Tibble, Numeric, or Non-Numeric. The numerator.
#' Tibbles and non-numeric data are automatically tallied (nrow or length).
#' @param den Optional. Tibble, Numeric, or Non-Numeric. The denominator.
#' Tibbles and non-numeric data are automatically tallied (nrow or length). May use
#' a single denominator for multiple numerators or one denominator for each
#' numerator.
#' @param percent.sign Optional. Logical. Indicates percent sign should be printed
#' for frequencies. Defaults to TRUE.
#' @param digits Optional. Integer. Number of digits to round to. Defaults to 1.
#' @examples
#' library(tibble)
#'
#' # Numeric
#' paste_freq(20, 100)
#'
#' # Tibble
#' data_tibble <- tibble(column = c(1:100))
#' paste_freq(data_tibble[1:20,], data_tibble)
#'
#' # Mixed data types
#' paste_freq(20, data_tibble)
#'
#' # Single denominator for multiple numerators
#' paste_freq(c(10,20,30), 100)
#' @export
paste_freq <- function(num = NA, den = NA, percent.sign = TRUE, digits = 1) {
  # Data aggregation
  if ('data.frame' %in% class(num)) num <- nrow(num)
  if ('data.frame' %in% class(den)) den <- nrow(den)
  if (!all(is.numeric(num)) & (is.vector(num) | is.factor(num)) & !all(is.na(num))) num <- length(stats::na.omit(num))
  if (!all(is.numeric(den)) & (is.vector(den) | is.factor(den)) & !all(is.na(den))) den <- length(stats::na.omit(den))

  # Hard stops
  if (length(num) != length(den) & length(den) != 1)
    stop('Numerator(s) and denominator(s) of incompatable lengths. [param: num, den]')

  # Create frequencies
  purrr::map2_chr(
    num, den,
    function(x, y)
      if (any(is.na(c(x,y))) | !any(is.numeric(c(x,y)))) NA
      else paste0(x, ' (', round((x / y) * 100, digits = digits), if (percent.sign & !is.infinite(x / y)) '%' else NULL, ')')
  )
}


#' @title Paste Median
#' @description Returns a human-readable median with inter-quartile
#' range from numeric data.
#' @param x Required. Numeric. Data to summarize.
#' @param less.than.one Optional. Logical. Indicates a median that rounds to 0 should
#' be printed as <1. Defaults to FALSE (0).
#' @param digits Optional. Integer. Number of digits to round to. Defaults to 1.
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


#' @title Paste Mean
#' @description Returns a human-readable mean with standard deviation
#' from numeric data.
#' @param x Required. Numeric. Data to summarize.
#' @param less.than.one Optional. Logical. Indicates a mean that rounds to 0 should
#' be printed as <1. Defaults to FALSE (0).
#' @param digits Optional. Integer. Number of digits to round to. Defaults to 1.
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


#' @title Paste Event-Free-Survival
#' @description Returns a human-readable event-free-survival from a survfit object
#' and a specified time point.
#' @param fit Required. survival::Surv() object. The time-to-event model of interest.
#' @param times Required. Numeric. Indicates duration of time-points of interest.
#' Units are whatever was used to create the time-to-event model.
#' @param percent.sign Optional. Logical. Indicates percent sign should be printed
#' for frequencies. Defaults to TRUE.
#' @param digits Optional. Integer. Number of digits to round to. Defaults to 1.
#' @examples
#' library(survival)
#' fit <- survfit(Surv(time, status) ~ 1, data = diabetic)
#' paste_efs(fit, c(1, 3, 5))
#' @export
paste_efs <- function(fit = NA, times = NA, percent.sign = TRUE, digits = 1) {
  if (all(is.na(times)) | class(fit) != 'survfit' | !all(is.numeric(times))) NA
  else {
    results <- summary(fit, times = times)
    estimate <- round(results$surv * 100, digits = digits)
    lower <- round(results$lower * 100, digits = digits)
    upper <- round(results$upper * 100, digits = digits)
    paste0(estimate, if (percent.sign) '%' else NULL, ' [', lower, '-', upper, ']')
  }
}
