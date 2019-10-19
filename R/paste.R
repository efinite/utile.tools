#' @title Paste Frequency
#' @description Returns a human-readable frequency from count(able) data. Supports
#' vectorized data (i.e. dplyr::mutate()).
#' @param count Optional. Tibble, Numeric, or Non-Numeric. The numerator.
#' Tibbles and non-numeric data are automatically tallied (nrow or length).
#' @param total Optional. Tibble, Numeric, or Non-Numeric. The denominator.
#' Tibbles and non-numeric data are automatically tallied (nrow or length).
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
#' @export
paste_freq <- function(count = NA, total = NA, percent.sign = TRUE, digits = 1) {
  if ('data.frame' %in% class(count)) count <- nrow(count)
  if ('data.frame' %in% class(total)) total <- nrow(total)
  if (!all(is.numeric(count)) & is.vector(count)) count <- length(count)
  if (!all(is.numeric(total)) & is.vector(total)) total <- length(total)
  purrr::map2_chr(
    count, total,
    function(x, y) {
      if (is.na(x) | is.na(y)) NA
      else if (y == 0) '--'
      else if (x < 1) paste0('0 (0', if (percent.sign) '%' else NULL, ')')
      else paste0(x, ' (', round((x / y) * 100, digits = digits), if (percent.sign) '%' else NULL, ')')
    }
  )
}

#' @title Paste Median
#' @description Returns a human-readable median with inter-quartile
#' range from numeric data.
#' @param col Required. Vector/Column (numeric). Data to summarize.
#' @param less.than.one Optional. Logical. Indicates a median that rounds to 0 should
#' be printed as <1. Defaults to FALSE (0).
#' @param digits Optional. Integer. Number of digits to round to. Defaults to 1.
#' @examples
#' paste_median(mtcars$mpg)
#' @export
paste_median <- function(col = NA, less.than.one = FALSE, digits = 1) {
  if (all(is.na(col))) NA
  else {
    col_median <- round(x = stats::median(col, na.rm = TRUE), digits = digits)
    if (round(col_median, digits = digits) == 0 & less.than.one) col_median <- '<1'
    col_iqr <- round(x = stats::quantile(col, probs = c(0.25, 0.75), na.rm = TRUE), digits = digits)
    paste0(col_median, ' [', paste0(col_iqr, collapse = '-'), ']')
  }
}

#' @title Paste Mean
#' @description Returns a human-readable mean with standard deviation
#' from numeric data.
#' @param col Required. Vector/Column (numeric). Data to summarize.
#' @param less.than.one Optional. Logical. Indicates a mean that rounds to 0 should
#' be printed as <1. Defaults to FALSE (0).
#' @param digits Optional. Integer. Number of digits to round to. Defaults to 1.
#' @examples
#' paste_mean(mtcars$mpg)
#' @export
paste_mean <- function(col = NA, less.than.one = FALSE, digits = 1) {
  if (all(is.na(col))) NA
  else {
    col_mean <- round(x = mean(col, na.rm = TRUE), digits = digits)
    if (round(col_mean, digits = digits) == 0 & less.than.one) col.mean <- '<1'
    col_sd <- round(x = stats::sd(col, na.rm = TRUE), digits = digits)
    paste0(col_mean, ' \u00B1', col_sd)
  }
}


#' @title Paste Event-Free-Survival
#' @description Returns a human-readable event-free-survival from a survfit object
#' and a specified time point.
#' @param fit Required. survival::Surv() object. The time-to-event model of interest.
#' @param time Required. Numeric. Indicates duration of time. Units are whatever was used to
#' create the time-to-event model.
#' @param percent.sign Optional. Logical. Indicates percent sign should be printed
#' for frequencies. Defaults to TRUE.
#' @param digits Optional. Integer. Number of digits to round to. Defaults to 1.
#' @examples
#' library(survival)
#' fit <- survfit(Surv(time, status) ~ 1, data = diabetic)
#' paste_efs(fit, c(1, 3, 5))
#' @export
paste_efs <- function(fit = NA, time = NA, percent.sign = TRUE, digits = 1) {
  if (all(is.na(time)) | class(fit) != 'survfit') NA
  else {
    results <- summary(fit, times = time)
    estimate <- round(results$surv * 100, digits = digits)
    lower <- round(results$lower * 100, digits = digits)
    upper <- round(results$upper * 100, digits = digits)
    paste0(estimate, if (percent.sign) '%' else NULL, ' [', lower, '-', upper, ']')
  }
}
