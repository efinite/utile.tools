#' @title Tabulate Model
#' @description Converts parameters from a model object into a usable
#' table for publication purposes. By default, formats the table into
#' a human-readable/exportable form.
#' @param fit Required. Model object. See S3 methods below.
#' @param format Optional. Logical. Rounds numbers and formats text for a
#' cleaner, readable output. Defaults to TRUE.
#' @param percent.sign Optional. Logical. Indicates percent sign should be printed
#' for frequencies. Defaults to TRUE.
#' @param digits Optional. Integer. Number of digits to round to. Defaults to 1.
#' @param p.digits Optional. Integer. Number of p-value digits to print. Note that
#' p-values are still rounded using 'digits'. Defaults to 4.
#' @return Returns tibble containing summarizing statistics and tests.
#' @seealso
#' \code{\link{tabulate_model.lm}},
#' \code{\link{tabulate_model.coxph}},
#' \code{\link{tabulate_model.glm}}
#' @export
tabulate_model <- function(fit, format, percent.sign, digits, p.digits) {
  UseMethod('tabulate_model')
}

#' @export
tabulate_model.default <- function(fit = NULL, format = NULL, percent.sign = NULL, digits = NULL, p.digits = NULL)
  warning(paste0('Object of class \'', class(fit), '\' not supported.'))

.tabulate_model <- function(
  fit, coefficients, levels,
  estimate, tests, counts = NA
) {

  # Create and return summary table
  purrr::map2_df(
    names(fit$assign), # Parameter names
    fit$assign, # Parameter positions in coefficients
    function(parameter, positions) {

      # Check if parameter categorical
      categorical <- parameter %in% names(levels)

      dplyr::bind_rows(

        # Reference rows for factor w/ >2 levels
        if (categorical & length(positions) > 1)
          dplyr::bind_rows(
            dplyr::bind_cols(
              variable = parameter,
              level = NA,
              if (all(!is.na(counts))) tibble::tibble(
                subjects = as.integer(counts[1]),
                events = as.integer(counts[2])
              ),
              p = as.double(tests[parameter, 1])
            ),
            dplyr::bind_cols(
              variable = parameter,
              level = levels[[parameter]][1]
            )
          ),

        # Create row(s) for each parameter/level
        purrr::imap_dfr(
          positions,
          function(position, index) {
            row <- coefficients[position,]
            dplyr::bind_cols(
              variable = as.character(parameter),
              level = as.character(if (categorical) levels[[parameter]][index + 1] else NA),
              if (all(!is.na(counts))) {
                tibble::tibble(
                  subjects = as.integer(if (length(positions) == 1) counts[1] else NA),
                  events = as.integer(if (length(positions) == 1) counts[2] else NA)
                )
              },
              estimate = as.double(if (!is.na(row[1])) estimate(row[1]) else NA),
              conf.lower = as.double(
                if (!any(is.na(c(row[1], row[2]))))
                  estimate(row[1] - (1.95 * row[2]))
                else NA
              ),
              conf.upper = as.double(
                if (!any(is.na(c(row[1], row[2]))))
                  estimate(row[1] + (1.95 * row[2]))
                else NA
              ),
              p = as.double(
                if (length(positions) == 1 & !is.na(tests[parameter, 1]))
                  tests[parameter, 1]
                else NA
              )
            )
          }
        )
      )
    }
  )

}

#' @title Tabulate Model: Cox PH
#' @description Converts parameters from a cox parametric hazards model
#' into a usable table for publication purposes.
#' @param fit Required. survival::coxph() object.
#' @param format Optional. Logical. Rounds numbers and formats text for a
#' cleaner, readable output. Defaults to TRUE.
#' @param percent.sign Optional. Logical. Indicates percent sign should be printed
#' for frequencies. Defaults to TRUE.
#' @param digits Optional. Integer. Number of digits to round to. Defaults to 1.
#' @param p.digits Optional. Integer. Number of p-value digits to print. Note that
#' p-values are still rounded using 'digits'. Defaults to 4.
#' @seealso \code{\link{tabulate_model}}
#' @examples
#' library(survival)
#' library(dplyr)
#'
#' # Survival data
#' data_lung <- lung %>%
#'   as_tibble() %>%
#'   mutate_at(vars(inst, status, sex), as.factor) %>%
#'   mutate(status = case_when(status == 1 ~ 0, status == 2 ~ 1))
#'
#' tabulate_model(
#'   fit = coxph(Surv(time, status) ~ sex + meal.cal + inst, data = data_lung)
#'  )
#' @export
tabulate_model.coxph <- function(fit, format = TRUE, percent.sign = TRUE, digits = 1, p.digits = 4) {

  # Tabulate
  res <- .tabulate_model(
    fit = fit,
    coefficients = matrix(
      summary(fit)$coefficients[, c(1, 3)],
      ncol = 2,
      dimnames = list(
        row.names(summary(fit)$coefficients),
        1:2
      )
    ),
    levels = fit$xlevels,
    tests = matrix(
      stats::anova(fit)[-1,4],
      ncol = 1,
      dimnames = list(names(fit$assign), c('p'))
    ),
    counts = c(fit$n, fit$nevent),
    estimate = function (x) exp(x)
  )

  # Return completed table
  if (format) .format_table(res, estimate = 'HR', percent.sign = percent.sign, digits = digits, p.digits = p.digits)
  else res
}

#' @title Tabulate Model: GLM
#' @description Converts parameters from a generalized linear model into a usable
#' table for publication purposes.
#' @param fit Required. MASS::glm(family = 'binomial') object.
#' @param format Optional. Logical. Rounds numbers and formats text for a
#' cleaner, readable output. Defaults to TRUE.
#' @param percent.sign Optional. Logical. Indicates percent sign should be printed
#' for frequencies. Defaults to TRUE.
#' @param digits Optional. Integer. Number of digits to round to. Defaults to 1.
#' @param p.digits Optional. Integer. Number of p-value digits to print. Note that
#' p-values are still rounded using 'digits'. Defaults to 4.
#' @seealso \code{\link{tabulate_model}}
#' @examples
#' library(dplyr)
#' library(MASS)
#'
#' # glm() Object
#' logit_data <- MASS::birthwt %>%
#'   mutate_at(c('race'), as.factor) %>%
#'   mutate_at(c('low', 'smoke', 'ht', 'ui'), as.logical)
#
#' tabulate_model(
#'   fit = stats::glm(
#'     low ~ race + smoke + age,
#'     data = logit_data,
#'     family = 'binomial'
#'    )
#' )
#' @export
tabulate_model.glm <- function(fit,  format = TRUE, percent.sign = TRUE, digits = 1, p.digits = 4) {

  # Hard stops
  if (!(fit$family$family %in% c('binomial'))) stop(paste0('Unsupported GLM family \'', fit$family$family, '\'. See docs.'))

  if (fit$family$family == 'binomial') {

    # Determine Assignments
    fit$assign <- attr(
      stats::model.matrix(object = fit$formula, data = fit$data),
      'assign'
    )
    fit$assign <- purrr::map(
      0:length(all.vars(fit$formula)[-1]),
      ~ which(.x == fit$assign)
    )
    names(fit$assign) <- c('(Intercept)', all.vars(fit$formula)[-1])

    # Tabulate
    res <- .tabulate_model(
        fit = fit,
        coefficients =  matrix(
          summary(fit)$coefficients[, 1:2],
          ncol = 2,
          dimnames = list(
            row.names(summary(fit)$coefficients),
            1:2
          )
        ),
        levels = fit$xlevels,
        tests = matrix(
          stats::drop1(fit, test="LRT")[,5],
          ncol = 1,
          dimnames = list(names(fit$assign), 1)
        ),
        counts = c(length(fit$y), sum(fit$y)),
        estimate = function (x) exp(x)
      )

    # Return completed table
    if (format) .format_table(res, estimate = 'OR', percent.sign = percent.sign, digits = digits, p.digits = p.digits)
    else res
  }
}

#' @title Tabulate Model: Linear Regression (LM)
#' @description Converts parameters from a linear regression model into a usable
#' table for publication purposes.
#' @param fit Required. lm() object.
#' @param format Optional. Logical. Rounds numbers and formats text for a
#' cleaner, readable output. Defaults to TRUE.
#' @param percent.sign Optional. Logical. Indicates percent sign should be printed
#' for frequencies. Defaults to TRUE.
#' @param digits Optional. Integer. Number of digits to round to. Defaults to 1.
#' @param p.digits Optional. Integer. Number of p-value digits to print. Note that
#' p-values are still rounded using 'digits'. Defaults to 4.
#' @seealso \code{\link{tabulate_model}}
#' @examples
#' library(dplyr)
#'
#' data_mtcars <- datasets::mtcars %>%
#'   dplyr::as_tibble() %>%
#'   dplyr::mutate_at(dplyr::vars('vs', 'am'), as.logical) %>%
#'   dplyr::mutate_at(dplyr::vars('gear', 'carb', 'cyl'), as.factor)
#'
#' tabulate_model(fit = lm(mpg ~ vs + drat + cyl, data = data_mtcars))
#' @export
tabulate_model.lm <- function(fit, format = TRUE, percent.sign = TRUE, digits = 1, p.digits = 4) {

  # Determine Assignments
  fit$assign <- purrr::map(
    0:length(all.vars(stats::formula(fit))[-1]),
    ~ which(.x == fit$assign)
  )
  names(fit$assign) <- c('(Intercept)', all.vars(stats::formula(fit))[-1])

  # Tabulate
  res <- .tabulate_model(
    fit = fit,
    coefficients =  matrix(
      summary(fit)$coefficients[, 1:2],
      ncol = 2,
      dimnames = list(
        row.names(summary(fit)$coefficients),
        1:2
      )
    ),
    levels = fit$xlevels,
    tests = matrix(
      stats::drop1(fit, test="Chisq")[,5],
      ncol = 1,
      dimnames = list(names(fit$assign), 1)
    ),
    estimate = function (x) x
  )

  # Return completed table
  if (format) .format_table(res, estimate = 'Estimate', percent.sign = percent.sign, digits = digits, p.digits = p.digits)
  else res
}

#' @title Tabulate At Risk
#' @description Returns a risk table from a model object and specified time points.
#' @param fit Required. survival::survfit() object.
#' @param times Required. Numeric. One or vector of times to calculate for.
#' @return Tibble risk table.
#' @examples
#' library(survival)
#' fit <- survfit(Surv(time, status) ~ 1, data = diabetic)
#' tabulate_at_risk(fit, c(1, 3, 5))
#' @export
tabulate_at_risk <- function(fit = NULL, times = NULL) {
  fit_summary <- summary(fit, times = times)
  tibble::tibble(
    strata = as.factor(
      if (is.null(fit$strata)) 'All'
      else
        purrr::map_chr(
          fit_summary$strata,
          function(x) stringr::str_split(x, '=')[[1]][2]
        )
    ),
    time = fit_summary$time,
    n.risk = fit_summary$n.risk,
  )
}
