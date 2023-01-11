#' @title Test the null hypothesis
#' @description Tests the null hypothesis that there is no difference between
#' grouped data.
#' @param x A numeric, factor, or logical. Observations.
#' @param y A factor or logical. Categorical "by" grouping variable.
#' @param test A character. Name of the statistical test to use. See note.
#' @param digits An integer. Number of digits to round to.
#' @param p.digits An integer. Minimum number of p-value digits to the right of
#' the decimal point. Note that p-values are still rounded using 'digits'.
#' @param ... Additional arguments passed to the appropriate S3 method.
#' @return A list containing the statistical test performed, test statistic,
#' and p-value.
#' @details Statistical testing used is dependent on type of 'x' data. Supported
#' testing for numeric data includes ANOVA ('anova'), Kruskal-Wallis ('kruskal'),
#' and Wilcoxon Rank Sum ('wilcoxon') tests. For categorical data, supported
#' testings includes Pearson's Chi-squared ('chisq') and Fisher's Exact Test
#' ('fisher').
#' @seealso \code{\link{test_hypothesis.numeric}},
#' \code{\link{test_hypothesis.factor}},
#' \code{\link{test_hypothesis.logical}}
#' @examples
#' strata <- as.factor(mtcars$cyl)
#'
#' test_hypothesis(mtcars$mpg, strata) # numeric
#' test_hypothesis(as.logical(mtcars$vs), strata) # logical
#' test_hypothesis(as.factor(mtcars$carb), strata) # factor
#' @export
test_hypothesis <- function (
    x,
    y,
    test,
    digits,
    p.digits,
    ...
  ) {
    UseMethod('test_hypothesis')
  }


# Default response
#' @export
test_hypothesis.default <- function (...) NA_character_


#' @title Test Hypothesis: Numeric
#' @param x A numeric. Observations.
#' @param y A factor or logical. Categorical "by" grouping variable.
#' @param test A character. Name of the statistical test to use. Supported tests
#' include ANOVA linear model ('anova'), kruskal-wallis ('kruskal'), and wilcoxon
#' rank sum tests ('wilcoxon').
#' @param digits An integer. Number of digits to round to.
#' @param p.digits An integer. Minimum number of p-value digits to the right of
#' the decimal point. Note that p-values are still rounded using 'digits'.
#' @param ... Additional arguments passed to S3 method.
#' @return A list containing the statistical test performed, test statistic,
#' and p-value.
#' @seealso \code{\link{test_hypothesis}}
#' @export
test_hypothesis.numeric <-
  function (
    x,
    y,
    test = c('anova', 'kruskal', 'wilcoxon'),
    digits = 1,
    p.digits,
    ...
  ) {

  # Check for valid test
  test <- match.arg(test)

  # Set reference variables
  res <- list(test = 'None', statistic = NA_real_, p = NA_real_)
  tab_na <- table(is.na(x), y)

  # Return conditions
  if (any(tab_na[1,] == 0) || any(colSums(tab_na) == 0)) {
    warning('No empty groups.')
    return(res)
  }
  if (test == 'Wilcoxon' & ncol(tab_na) != 2) {
    warning('Wilcoxon Rank Sum must compare 2 groups.')
    return(res)
  }

  # Set test name
  res$test <- switch(
    test,
    'anova' = 'ANOVA linear model',
    'kruskal' = 'Kruskal-Wallis rank sum test',
    'wilcoxon' = 'Wilcoxon rank sum test'
  )

  # Run statistical test
  test_obj <- switch(
    test,
    'anova' = stats::anova(stats::lm(x ~ y)),
    'kruskal' = stats::kruskal.test(x ~ as.factor(y)),
    'wilcoxon' = suppressWarnings(stats::wilcox.test(x ~ as.factor(y)))
  )

  # Set test statistic
  res$statistic <-
    if (test == 'anova') test_obj[1, ncol(test_obj)-1]
    else unname(test_obj$statistic)
  res$statistic <- round(res$statistic, digits = digits)

  # Set p-value
  res$p <-
    if (test == 'anova') test_obj[1, ncol(test_obj)]
    else test_obj$p.value
  if (!missing(p.digits)) res$p <- paste_pval(x = res$p,
                                              digits = digits,
                                              p.digits = p.digits)

  # Return
  res

}


#' @title Test Hypothesis: Factor
#' @param x A factor. Observations.
#' @param y A factor or logical. Categorical "by" grouping variable.
#' @param test A character. Name of the statistical test to use. Supported tests
#' include Pearson's Chi-squared Test ('chisq') and Fisher's Exact Test ('fisher').
#' @param digits An integer. Number of digits to round to.
#' @param p.digits An integer. Minimum number of p-value digits to the right of
#' the decimal point. Note that p-values are still rounded using 'digits'.
#' @param simulate.p.value A logical. Whether p-values in nominal variable testing
#' should be computed with Monte Carlo simulation.
#' @param B An integer. Number of replicates to use in Monte Carlo simulation.
#' @param workspace An integer. Size of the workspace used for the Fisher's Exact
#' Test network algorhythm.
#' @param ... Additional arguments passed to S3 method.
#' @return A list containing the statistical test performed, test statistic,
#' and p-value.
#' @seealso \code{\link{test_hypothesis}}
#' @export
test_hypothesis.factor <-
  function (
    x,
    y,
    test = c('chisq', 'fisher'),
    digits = 1,
    p.digits,
    simulate.p.value = FALSE,
    B = 2000,
    workspace = 2e7,
    ...
  ) {

    # Check for valid test
    test <- match.arg(test)

    # Set reference variables
    res <- list(test = 'None', statistic = NA_real_, p = NA_real_)
    tab <- table(x, y, exclude = NA)
    rs <- rowSums(tab)
    cs <- colSums(tab)

    # Chisq
    if (test == 'chisq') {

      # Early return condition
      if ((any(rs == 0) || any(cs == 0)) && ncol(tab) > 1 && nrow(tab) > 1) {
        warning('Chi-squared test cannot be run with groups provided (counts).')
        return(res)
      }

      # Run statistical test
      if (length(cs) > 1) tab <- tab[rs > 0, , drop = FALSE]
      if (length(rs) > 1) tab <- tab[, cs > 0, drop = FALSE]
      test_obj <- suppressWarnings(
        stats::chisq.test(x = tab, simulate.p.value = simulate.p.value, B = B)
      )

      # Set test statistic
      res$statistic <- round(unname(test_obj$statistic), digits = digits)

    # Fisher
    } else {

      # Early return condition
      if ((any(rs == 0) || any(cs == 0)) || ncol(tab) == 1 || nrow(tab) == 1) {
        warning('Fisher\'s Exact Test cannot be run with groups provided (counts).')
        return(res)
      }

      # Run statistical test
      test_obj <- stats::fisher.test(x = tab,
                                     simulate.p.value = simulate.p.value,
                                     B = B,
                                     workspace = workspace)

    }

    # Set test name
    res$test <- switch(
      test,
      'chisq' = 'Pearson\'s Chi-squared Test',
      'fisher' = 'Fisher\'s Exact Test'
    )

    # Set p-value
    res$p <- test_obj$p.value
    if (!missing(p.digits)) res$p <- paste_pval(x = res$p,
                                                digits = digits,
                                                p.digits = p.digits)

    # Return
    res
  }


#' @title Test Hypothesis: Logical
#' @param x A logical. Observations.
#' @param y A factor or logical. Categorical "by" grouping variable.
#' @param test A character. Name of the statistical test to use. Supported tests
#' include Pearson's Chi-squared Test ('chisq') and Fisher's Exact Test ('fisher').
#' @param digits An integer. Number of digits to round to.
#' @param p.digits An integer. Minimum number of p-value digits to the right of
#' the decimal point. Note that p-values are still rounded using 'digits'.
#' @param simulate.p.value A logical. Whether p-values in nominal variable testing
#' should be computed with Monte Carlo simulation.
#' @param B An integer. Number of replicates to use in Monte Carlo simulation.
#' @param workspace An integer. Size of the workspace used for the Fisher's Exact
#' Test network algorhythm.
#' @param ... Additional arguments passed to S3 method.
#' @return A list containing the statistical test performed, test statistic,
#' and p-value.
#' @seealso \code{\link{test_hypothesis}}
#' @export
test_hypothesis.logical <- test_hypothesis.factor
