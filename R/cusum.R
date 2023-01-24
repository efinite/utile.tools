#' @title Cumulative Sum of Failures
#' @description Calculates the cumulative sum of failures for a series of
#' procedures which can be used to create CUSUM charts.
#' @param xi An integer. The dichotomous outcome variable (1 = Failure, 0 = Success)
#' for the i-th procedure.
#' @param p0 A double. The acceptable event rate.
#' @param p1 A double. The unacceptable event rate.
#' @param by A factor. Optional variable to stratify procedures by.
#' @param alpha A double. The Type I Error rate. Probability of rejecting the
#' null hypothesis when `p0` is the true rate.
#' @param beta A double. The Type II Error rate. Probability of failing to reject
#' null hypothesis when it is false.
#' @return An object of class \code{data.frame}.
#' @references
#' Rogers, C. A., Reeves, B. C., Caputo, M., Ganesh, J. S., Bonser, R. S., & Angelini, G. D. (2004). Control chart methods for monitoring cardiac surgical performance and their interpretation. The Journal of Thoracic and Cardiovascular Surgery, 128(6), 811-819.
#' @examples
#' library(purrr)
#' library(ggplot2)
#'
#' # Data
#' df <- data.frame(
#'   xi = simplify(
#'     map(
#'       c(.1,.08,.05,.1,.13,.14,.14,.09,.25),
#'       ~ rbinom(50,1,.x))),
#'    p0 = simplify(
#'     map(
#'       c(.1,.1,.1,.1,.1,.1,.1,.15,.2),
#'       ~ rnorm(50,.x,.03))),
#'    by = rep(
#'     factor(paste('Subject', c('A','B','C'))),
#'     times = c(150,150,150))
#'  )
#'
#' # Overall event rate
#' p0 <- sum(df$xi) / nrow(df)
#'
#' # Create CUSUM plot
#' cusum_failure(
#'  xi = df$xi,
#'  p0 = p0,
#'  p1 = p0 * 1.5,
#'  by = df$by
#' ) |>
#' ggplot(aes(y = cusum, x = i)) +
#'   geom_step() +
#'   geom_line(mapping = aes(y = l0), linetype = 2) +
#'   geom_line(mapping = aes(y = l1), linetype = 2) +
#'   ylab("Cumulative Failures") +
#'   xlab("Case Number") +
#'   facet_wrap(~ by) +
#'   theme_bw()
#' @export
cusum_failure <- function (
    xi,
    p0,
    p1,
    by = NULL,
    alpha = 0.05,
    beta = 0.05
) {

  # Variable calculations
  OR <- (p1 * (1 - p0)) / (p0 * (1 - p1))
  s  <- log((1 - p0) / (1 - p1)) / log(OR)
  h0 <- log((1 - alpha) / beta) / log(OR)
  h1 <- log((1 - beta) / alpha) / log(OR)

  # Function for column creation
  append_cols <- function (x) {
    x$i <- 1:nrow(x)
    x$cusum <- cumsum(x$xi)
    x$l0 <- (1:nrow(x) * s) - h0
    x$l1 <- (1:nrow(x) * s) + h1
    x
  }

  # Create data table
  col_names <- c('i', 'xi', 'cusum', 'l0', 'l1')
  cols <- vctrs::data_frame(xi = xi)

  # Create columns
  if (!is.null(by)) {
    col_names <- c(col_names, 'by')
    cols$by <- by
    cols <- purrr::list_rbind(
      purrr::map(
        unique(by),
        function (.x) append_cols(cols[cols$by %in% .x,])
      )
    )
  } else cols <- append_cols(cols)

  # Return arranged data
  cols[,col_names]
}


#' @title Cumulative Sum of Log-Likelihood Ratio
#' @description Calculates the cumulative log likelihood ratio of failure for a
#' series of procedures which can be used to create CUSUM charts.
#' @param xi An integer. The dichotomous outcome variable (1 = Failure, 0 = Success)
#' for the i-th procedure.
#' @param p0 A double. The acceptable event rate.
#' @param p1 A double. The unacceptable event rate.
#' @param by A factor. Optional variable to stratify procedures by.
#' @param alpha A double. The Type I Error rate. Probability of rejecting the
#' null hypothesis when `p0` is true.
#' @param beta A double. The Type II Error rate. Probability of failing to reject
#' null hypothesis when it is false.
#' @return An object of class \code{data.frame}.
#' @references
#' Rogers, C. A., Reeves, B. C., Caputo, M., Ganesh, J. S., Bonser, R. S., & Angelini, G. D. (2004). Control chart methods for monitoring cardiac surgical performance and their interpretation. The Journal of Thoracic and Cardiovascular Surgery, 128(6), 811-819.
#' @examples
#' library(purrr)
#' library(ggplot2)
#'
#' # Data
#' df <- data.frame(
#'   xi = simplify(
#'     map(
#'       c(.1,.08,.05,.1,.13,.14,.14,.09,.25),
#'       ~ rbinom(50,1,.x))),
#'    p0 = simplify(
#'     map(
#'       c(.1,.1,.1,.1,.1,.1,.1,.15,.2),
#'       ~ rnorm(50,.x,.03))),
#'    by = rep(
#'     factor(paste('Subject', c('A','B','C'))),
#'     times = c(150,150,150))
#'  )
#'
#' # Overall event rate
#' p0 <- sum(df$xi) / nrow(df)
#'
#' # Create CUSUM plot
#' cusum_loglike(
#'   xi = df$xi,
#'   p0 = p0,
#'   p1 = p0 * 1.5,
#'   by = df$by
#' ) |>
#' ggplot(aes(y = cusum, x = i)) +
#'   geom_step() +
#'   geom_hline(aes(yintercept = h0), linetype = 2) +
#'   geom_hline(aes(yintercept = h1), linetype = 2) +
#'   ylab("Cumulative Log-likelihood Ratio") +
#'   xlab("Case Number") +
#'   facet_wrap(~ by) +
#'   theme_bw()
#' @export
cusum_loglike <- function (
    xi,
    p0,
    p1,
    by = NULL,
    alpha = 0.05,
    beta = 0.05
) {

  # Variable calculations
  OR <- (p1 * (1 - p0)) / (p0 * (1 - p1))
  s  <- log((1 - p0) / (1 - p1)) / log(OR)
  h0 <- log((1 - alpha) / beta) / log(OR)
  h1 <- log((1 - beta) / alpha) / log(OR)

  # Function for column creation
  append_cols <- function (x) {
    x$i <- 1:nrow(x)
    x$cusum <- cumsum(x$xi - s)
    x
  }

  # Create data table
  col_names <- c('i', 'xi', 'cusum', 'h0', 'h1')
  cols <- vctrs::data_frame(
    xi = xi,
    h0 = -rep(h0, times = length(xi)),
    h1 = rep(h1, times = length(xi))
  )

  # Create columns
  if (!is.null(by)) {
    col_names <- c(col_names, 'by')
    cols$by <- by
    cols <- purrr::list_rbind(
      purrr::map(
        unique(by),
        function (.x) append_cols(cols[cols$by %in% .x,])
      )
    )
  } else cols <- append_cols(cols)

  # Return arranged data
  cols[,col_names]
}


#' @title Cumulative Sum of Observed Minus Expected Outcome
#' @description Calculates the cumulative observed-minus-expected failure for a
#' series of procedures which can be used to create CUSUM charts.
#' @param xi An integer. The dichotomous outcome variable (1 = Failure, 0 = Success)
#' for the i-th procedure.
#' @param p0 A double. The acceptable event rate.
#' @param by A factor. Optional variable to stratify procedures by.
#' @return An object of class \code{data.frame}.
#' @references
#' Rogers, C. A., Reeves, B. C., Caputo, M., Ganesh, J. S., Bonser, R. S., & Angelini, G. D. (2004). Control chart methods for monitoring cardiac surgical performance and their interpretation. The Journal of Thoracic and Cardiovascular Surgery, 128(6), 811-819.
#' @examples
#' library(purrr)
#' library(ggplot2)
#'
#' # Data
#' df <- data.frame(
#'   xi = simplify(
#'     map(
#'       c(.1,.08,.05,.1,.13,.14,.14,.09,.25),
#'       ~ rbinom(50,1,.x))),
#'    p0 = simplify(
#'     map(
#'       c(.1,.1,.1,.1,.1,.1,.1,.15,.2),
#'       ~ rnorm(50,.x,.03))),
#'    by = rep(
#'     factor(paste('Subject', c('A','B','C'))),
#'     times = c(150,150,150))
#'  )
#'
#' # Create CUSUM plot
#' cusum_ome(
#'  xi = df$xi,
#'  p0 = df$p0,
#'  by = df$by
#' ) |>
#' ggplot(aes(x = i, y = cusum)) +
#' geom_hline(yintercept = 0, linetype = 6, linewidth = 0.5) +
#'   geom_step() +
#'   ylab("Cumulative Observed Minus Expected Failures") +
#'   xlab("Case Number") +
#'   facet_wrap(~ by) +
#'   theme_bw()
#' @export
cusum_ome <- function (
    xi,
    p0,
    by = NULL
) {

  # Recycle p0 if length 1
  if (length(p0) == 1) p0 <- rep(p0, times = length(xi))

  # Function for column creation
  append_cols <- function (x) {
    x$i <- 1:nrow(x)
    x$cusum <- (function (y, z) {
      res <- double()
      for (i in 1:length(y)) {
        ref <- if (i == 1) 0 else res[i - 1]
        res[i] <- if (y[i] == 0) ref - z[i]
        else ref + (1 - z[i])
      }
      res
    })(x$xi, x$p0)
    x
  }

  # Create data table
  col_names <- c('i', 'xi', 'p0', 'cusum')
  cols <- vctrs::data_frame(
    xi = xi,
    p0 = p0
  )

  # Create columns
  if (!is.null(by)) {
    col_names <- c(col_names, 'by')
    cols$by <- by
    cols <- purrr::list_rbind(
      purrr::map(
        unique(by),
        function (.x) append_cols(cols[cols$by %in% .x,])
      )
    )
  } else cols <- append_cols(cols)

  # Return arranged data
  cols[,col_names]

}


#' @title Risk-adjusted Sequential Probability Ratio Test (SPRT)
#' @description Calculates the risk-adjusted sequential probability ratio test
#' for a series of procedures which can be used to create CUSUM charts.
#' @param xi An integer. The dichotomous outcome variable (1 = Failure, 0 = Success)
#' for the i-th procedure.
#' @param p0 A double. The individual acceptable event rate for each individual
#' procedure (adjusted).
#' @param OR A double. An odds-ratio reflecting the increase in relative risk of
#' failure.
#' @param by A factor. Optional variable to stratify procedures by.
#' @param alpha A double. The Type I Error rate. Probability of rejecting the
#' null hypothesis when `p0` is true.
#' @param beta A double. The Type II Error rate. Probability of failing to reject
#' null hypothesis when it is false.
#' @return An object of class \code{data.frame}.
#' @references
#' Rogers, C. A., Reeves, B. C., Caputo, M., Ganesh, J. S., Bonser, R. S., & Angelini, G. D. (2004). Control chart methods for monitoring cardiac surgical performance and their interpretation. The Journal of Thoracic and Cardiovascular Surgery, 128(6), 811-819.
#' @examples
#' library(purrr)
#' library(ggplot2)
#'
#' # Data
#' df <- data.frame(
#'   xi = simplify(
#'     map(
#'       c(.1,.08,.05,.1,.13,.14,.14,.09,.25),
#'       ~ rbinom(50,1,.x))),
#'    p0 = simplify(
#'     map(
#'       c(.1,.1,.1,.1,.1,.1,.1,.15,.2),
#'       ~ rnorm(50,.x,.03))),
#'    by = rep(
#'     factor(paste('Subject', c('A','B','C'))),
#'     times = c(150,150,150))
#'  )
#'
#' # Create CUSUM plot
#' cusum_sprt(
#'   xi = df$xi,
#'   p0 = df$p0,
#'   OR = 1.5,
#'   by = df$by
#' ) |>
#' ggplot(aes(y = cusum, x = i)) +
#'   geom_step() +
#'   geom_hline(aes(yintercept = h0), linetype = 2) +
#'   geom_hline(aes(yintercept = h1), linetype = 2) +
#'   ylab("Cumulative Log-likelihood Ratio") +
#'   xlab("Case Number") +
#'   facet_wrap(~ by) +
#'   theme_bw()
#' @export
cusum_sprt <- function (
    xi,
    p0,
    OR,
    by = NULL,
    alpha = 0.05,
    beta = 0.05
) {

  # Variable calculations
  h0 <- log((1 - alpha) / beta) / log(OR)
  h1 <- log((1 - beta) / alpha) / log(OR)

  # Function for column creation
  append_cols <- function (x) {
    x$i <- 1:nrow(x)
    x$cusum <- cumsum(x$xi - (log((1 - x$p0) + (OR * x$p0)) / log(OR)))
    x
  }

  # Create data table
  col_names <- c('i', 'xi', 'p0', 'h0', 'h1', 'cusum')
  cols <- vctrs::data_frame(
    xi = xi,
    p0 = p0,
    h0 = -rep(h0, times = length(xi)),
    h1 = rep(h1, times = length(xi))
  )

  # Create columns
  if (!is.null(by)) {
    col_names <- c(col_names, 'by')
    cols$by <- by
    cols <- purrr::list_rbind(
      purrr::map(
        unique(by),
        function (.x) append_cols(cols[cols$by %in% .x,])
      )
    )
  } else cols <- append_cols(cols)

  # Return arranged data
  cols[,col_names]
}
