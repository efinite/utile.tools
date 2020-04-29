#' @title Tabulate numbers at risk
#' @description
#' Creates a risk table from a model object and specified time points.
#' @param fit Required. An object of class '\code{\link[survival:survfit]{survfit}}'.
#' @param times Required. Numeric. Vector of times to calculate for.
#' @return A \code{\link[tibble:tibble]{tibble}} containing numbers at risk.
#' @examples
#' library(survival)
#'
#' fit <- survfit(Surv(time, status) ~ 1, data = diabetic)
#' tabulate_at_risk(fit, c(1, 3, 5))
#' @export
tabulate_at_risk <- function(fit = NULL, times = NULL) {
  fit_summary <- summary(fit, times = times)
  tibble::tibble(
    strata = as.factor(
      if (is.null(fit$strata)) 'All'
      else {
        purrr::map_chr(
          as.character(fit_summary$strata),
          ~ strsplit(.x, '=')[[1]][2]
        )
      }
    ),
    time = fit_summary$time,
    n.risk = fit_summary$n.risk,
  )
}
