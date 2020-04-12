#' @title Tabulate model coefficients
#' @description
#' Converts parameters from a model object into a usable
#' table for publication purposes.
#'
#' This function is fundamentally similar to broom::tidy with
#' some important differences that enable creation of more human-readable
#' outputs (i.e. exportable tables). All model variables and their respective
#' levels, not simply model terms, are reported including the reference level
#' for factors with 3 or more levels. A test statistic is also calculated for
#' each factor variable with multiple "dummy variables". Variable statistics
#' can also be overwritten with model \code{\link[stats]{drop1}} test
#' statistics. Methods are also provided for the return of a fully-formatted
#' export-ready table.
#' @param fit Required. Model object. See S3 methods below.
#' @param level Optional. Double. Confidence level.
#' @param test Optional. Character. Test to use for stats::drop1 testing of
#' model parameters.
#' @param format Optional. Logical. Round numbers and format text for a
#' cleaner, readable output.
#' @param digits Optional. Integer. Number of digits to round to.
#' @param p.digits Optional. Integer. Number of p-value digits to print. Note that
#' p-values are still rounded according to 'digits' value.
#' @return A \code{\link[tibble:tibble]{tibble}} containing summarizing statistics and tests.
#' @seealso
#' \code{\link{tabulate_coef.lm}},
#' \code{\link{tabulate_coef.coxph}},
#' \code{\link{tabulate_coef.glm}}
#' @export
tabulate_coef <- function(fit, level, test, format, digits, p.digits)
  UseMethod('tabulate_coef')


#' @rdname tabulate_coef
#' @export
tabulate_coefficients <- tabulate_coef


#' @export
tabulate_coef.default <- function(fit = NULL, ...)
  warning(paste0('Object of class \'', class(fit), '\' not supported.'))


# Main tabulate function
.tabulate_coef <- function (
  assigns = NA, table = NA, levels = NA,
  tests = NA, counts = NA, prefer.tests = FALSE
) {

  res <- purrr::imap_dfr(
    assigns,
    function (index, parameter) {

      # Starter variables
      has_levels <- parameter %in% names(levels)
      table <- table[index,]
      tests <- tests[tests$variable == parameter,]

      # Revise variable names
      table <- dplyr::mutate(table, variable = parameter)

      # Handle factors
      if (has_levels) {

        # Append levels
        table <- dplyr::mutate(
          table,
          level = levels[[parameter]][-1]
        )

        # Append variable & reference rows & statistics from "tests"
        if(length(levels[[parameter]]) > 2) {
          table <- dplyr::bind_rows(
            dplyr::bind_cols(
              tibble::tibble(
                variable = parameter,
                level = c(NA, levels[[parameter]][1]),
                test = c(tests$test, NA),
                statistic = c(tests$statistic, NA),
                p = c(tests$p, NA)
              ),
              if (is.list(counts)) lapply(counts, function(x) c(x, NA))
            ),
            table
          )
        }

      }

      # Append count columns
      if (
        is.list(counts) &
        (!has_levels | (has_levels & length(levels[[parameter]]) < 3))
      ) table <- dplyr::bind_cols(table, counts)

      # Overwrite statistics from 'tests'
      if (
        prefer.tests &
        parameter %in% tests$variable &
        (!has_levels | (has_levels & length(levels[[parameter]]) < 3))
      ) {
        table <- dplyr::mutate(
          table,
          test = tests$test,
          statistic = tests$statistic,
          p = tests$p
        )
      }

      # Return data
      table

    }
  )

  # Arrange columns and return
  dplyr::select(
    .data = res,
    variable, tidyselect::any_of(c('level')), tidyselect::any_of(c('subjects', 'events')),
    estimate, conf.low, conf.high, test, statistic, p
  )

}

#' @rdname tabulate_coef.coxph
#' @title Tabulate coxph coefficients
#' @description
#' Converts terms from a cox parametric hazards model
#' into a usable table for publication purposes.
#'
#' This function is fundamentally similar to broom::tidy with
#' some important differences that enable creation of more human-readable
#' outputs (i.e. exportable tables). All model variables and their respective
#' levels, not simply model terms, are reported including the reference level
#' for factors with 3 or more levels. A test statistic is also calculated for
#' each factor variable with multiple "dummy variables". Variable statistics
#' can also be overwritten with model \code{\link[stats]{drop1}} test
#' statistics. Methods are also provided for the return of a fully-formatted
#' export-ready table.
#' @param fit Required. An object of class
#' '\code{\link[survival:coxph.object]{coxph}}'.
#' @inheritParams tabulate_coef
#' @return A \code{\link[tibble:tibble]{tibble}} of model
#' terms with the columns:
#' \item{variable}{The name of regression variable.}
#' \item{level}{The level of the regression varaible, if the
#' variable is a factor.}
#' \item{observations}{The number of observations used in the fit.}
#' \item{events}{The number of events used in the fit.}
#' \item{estimate}{The hazard ratio of the regression term.}
#' \item{conf.low}{The low end of the confidence interval for the
#' regression term hazard ratio.}
#' \item{conf.high}{The low end of the confidence interval for the
#' regression term hazard ratio.}
#' \item{test}{The test used to produce the statistic and p-value.}
#' \item{statistic}{The value of a statistic used in testing the null
#' hypothesis.}
#' \item{p}{The two-sided p-value associated with the statistic.}
#' @examples
#' library(survival)
#' library(dplyr)
#'
#' data_lung <- lung %>%
#'   as_tibble() %>%
#'   mutate_at(vars(inst, status, sex), as.factor) %>%
#'   mutate(status = case_when(status == 1 ~ 0, status == 2 ~ 1))
#'
#' fit_coxph <- coxph(Surv(time, status) ~ sex + meal.cal + inst, data = data_lung)
#'
#' tabulate_coef(fit_coxph) # Format ready
#' tabulate_coef(fit_coxph, format = TRUE) # Formatted
#' @seealso \code{\link{tabulate_coef}}
#' @export
tabulate_coef.coxph <- function (
  fit, level = 0.95, test = c('LRT', 'Chisq'),
  format = FALSE, digits = 1, p.digits = 4
) {

  # Check test argument
  prefer.tests <- methods::hasArg(test)
  test <- match.arg(test)

  res <- .tabulate_coef(

    # Variable assignments
    assigns = fit$assign,

    # Coefficient table
    table = .tidy_table(
      table = summary(fit)$coefficients,
      cols = c(1,3:5), # c(estimate, s/e, statistic, p)
      level = level,
      test = 'Wald',
      func = exp
    ),

    # Factor levels
    levels = fit$xlevels,

    # Model testing
    tests = .tidy_tests(
      stats::drop1(

        # Remove NA values from model data
        stats::update(
          fit,
          data = stats::na.omit(
            get(
              as.character(stats::getCall(fit)$data),
              environment(stats::formula(fit))
            )[all.vars(stats::formula(fit))]
          )
        ),

        test = 'Chisq'
      ),
      3:4,
      'LRT'
    ),

    counts = list(subjects = fit$n, events = fit$nevent),

    # Overwrite w/ model testing
    prefer.tests = prefer.tests
  )

  # Return completed table
  if (format)
    .format_table(
      res, estimate = 'OR', level = level,
      digits = digits, p.digits = p.digits
    )
  else res

}


#' @rdname tabulate_coef.glm
#' @title Tabulate glm coefficients
#' @description
#' Converts terms from a generalized linear model
#' into a usable table for publication purposes.
#'
#' This function is fundamentally similar to broom::tidy with
#' some important differences that enable creation of more human-readable
#' outputs (i.e. exportable tables). All model variables and their respective
#' levels, not simply model terms, are reported including the reference level
#' for factors with 3 or more levels. A test statistic is also calculated for
#' each factor variable with multiple "dummy variables". Variable statistics
#' can also be overwritten with model \code{\link[stats]{drop1}} test
#' statistics. Methods are also provided for the return of a fully-formatted
#' export-ready table.
#' @param fit Required. An object of class '\code{\link[stats]{glm}}'.
#' @inheritParams tabulate_coef
#' @return A \code{\link[tibble:tibble]{tibble}} of model
#' terms with the columns:
#' \item{variable}{The name of regression variable.}
#' \item{level}{The level of the regression varaible, if the
#' variable is a factor.}
#' \item{observations}{The number of observations used in the fit.}
#' \item{events}{The number of events used in the fit.}
#' \item{estimate}{The odds ratio of the regression term.}
#' \item{conf.low}{The low end of the confidence interval for the
#' regression term odds ratio.}
#' \item{conf.high}{The low end of the confidence interval for the
#' regression term odds ratio.}
#' \item{test}{The test used to produce the statistic and p-value.}
#' \item{statistic}{The value of a statistic used in testing the null
#' hypothesis.}
#' \item{p}{The two-sided p-value associated with the statistic.}
#' @examples
#' library(dplyr)
#' library(MASS)
#'
#' logit_data <- MASS::birthwt %>%
#'   mutate_at(c('race'), as.factor) %>%
#'   mutate_at(c('low', 'smoke', 'ht', 'ui'), as.logical)
#'
#' fit_glm <- stats::glm(
#'   low ~ race + smoke + age,
#'   data = logit_data,
#'   family = 'binomial')
#'
#' tabulate_coef(fit_glm) # Format ready
#' tabulate_coef(fit_glm, format = TRUE) # Formatted
#' @seealso \code{\link{tabulate_coef}}
#' @export
tabulate_coef.glm <- function (
  fit, level = 0.95, test = c('LRT', 'Chisq', 'Rao'),
  format = FALSE, digits = 1, p.digits = 4
) {

  # Hard stop for GLM family
  if (!(fit$family$family %in% c('binomial'))) stop(paste0('Unsupported GLM family \'', fit$family$family, '\'. See docs.'))

  # Check test argument
  prefer.tests <- methods::hasArg(test)
  test <- match.arg(test)

  res <- .tabulate_coef(

    # Variable assignments
    assigns = .tidy_assigns(
      c('(Intercept)', attr(stats::terms(fit), 'term.labels')),
      attr(
        stats::model.matrix(object = stats::formula(fit), data = fit$data),
        'assign'
      )
    ),

    # Coefficient table
    table = .tidy_table(
      table = summary(fit)$coefficients,
      cols = 1:4, # c(estimate, s/e, statistic, p)
      level = level,
      test = 'z-Stat',
      func = exp
    ),

    # Factor levels
    levels = fit$xlevels,

    # Model testing
    tests = .tidy_tests(
      stats::drop1(fit, test = test),
      4:5,
      dplyr::case_when(test %in% c('LRT', 'Chisq') ~ 'LRT', TRUE ~ test)
    ),

    # Overwrite w/ model testing
    prefer.tests = prefer.tests
  )

  # Return completed table
  if (format)
    .format_table(
      res, estimate = 'OR', level = level,
      digits = digits, p.digits = p.digits
    )
  else res

}


#' @rdname tabulate_coef.lm
#' @title Tabulate lm coefficients
#' @description
#' Converts terms from a linear regression model
#' into a usable table for publication purposes.
#'
#' This function is fundamentally similar to broom::tidy with
#' some important differences that enable creation of more human-readable
#' outputs (i.e. exportable tables). All model variables and their respective
#' levels, not simply model terms, are reported including the reference level
#' for factors with 3 or more levels. A test statistic is also calculated for
#' each factor variable with multiple "dummy variables". Variable statistics
#' can also be overwritten with model \code{\link[stats]{drop1}} test
#' statistics. Methods are also provided for the return of a fully-formatted
#' export-ready table.
#' @param fit Required. An object of class '\code{\link[stats]{lm}}'.
#' @inheritParams tabulate_coef
#' @return A \code{\link[tibble:tibble]{tibble}} of model
#' terms with the columns:
#' \item{variable}{The name of regression variable.}
#' \item{level}{The level of the regression varaible, if the
#' variable is a factor.}
#' \item{observations}{The number of observations used in the fit.}
#' \item{events}{The number of events used in the fit.}
#' \item{estimate}{The estimate of the regression term.}
#' \item{conf.low}{The low end of the confidence interval for the
#' regression term estimate.}
#' \item{conf.high}{The low end of the confidence interval for the
#' regression term estimate.}
#' \item{test}{The test used to produce the statistic and p-value.}
#' \item{statistic}{The value of a statistic used in testing the null
#' hypothesis.}
#' \item{p}{The two-sided p-value associated with the statistic.}
#' @seealso \code{\link{tabulate_coef}}
#' @examples
#' library(dplyr)
#'
#' data_mtcars <- datasets::mtcars %>%
#'   dplyr::as_tibble() %>%
#'   dplyr::mutate_at(dplyr::vars('vs', 'am'), as.logical) %>%
#'   dplyr::mutate_at(dplyr::vars('gear', 'carb', 'cyl'), as.factor)
#'
#' fit_lm <- lm(mpg ~ vs + drat + cyl, data = data_mtcars)
#'
#' tabulate_coef(fit = fit_lm) # Format ready
#' tabulate_coef(fit = fit_lm, format = TRUE) # Formatted
#' @export
tabulate_coef.lm <- function (
  fit, level = 0.95, test = c('F', 'Chisq'),
  format = FALSE, digits = 1, p.digits = 4
) {

  # Check test argument
  prefer.tests <- methods::hasArg(test)
  test <- match.arg(test)

  # Generate table
  res <- .tabulate_coef(

    # Variable assignments
    assigns = .tidy_assigns(
      c('(Intercept)', attr(stats::terms(fit), 'term.labels')),
      fit$assign
    ),

    # Coefficient table
    table = .tidy_table(
      table = summary(fit)$coefficients,
      cols = 1:4, # c(estimate, s/e, statistic, p)
      level = level,
      test = 't-Stat'
    ),

    # Factor levels
    levels = fit$xlevels,

    # Model testing
    tests = .tidy_tests(
      stats::drop1(fit, test = test),
      dplyr::case_when(
        test == 'F' ~ 5:6,
        test == 'Chisq' ~ as.integer(c(NA, 5))
      ),
      dplyr::if_else(test == 'F', 'F-Test', test)
    ),

    # Overwrite w/ model testing
    prefer.tests = prefer.tests
  )

  # Return completed table
  if (format)
    .format_table(
      res, estimate = 'Estimate', level = level,
      digits = digits, p.digits = p.digits
    )
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
