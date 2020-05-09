#' @title Test the null hypothesis
#' @description
#' Produces a p-value from parametric or non-parametric testing of the
#' null hypothesis for stratified data.
#' @param x A numeric or factor. Observations.
#' @param y A factor. Factor to stratify by.
#' @param parametric A logical. Indicates parametric testing should be used.
#' See note detailing statistical tests.
#' @param digits An integer. Number of digits to round to.
#' @param p.digits An integer. Number of p-value digits to print. Note that
#' p-values are still rounded using 'digits'.
#' @return A character formatted p-value.
#' @note Statistical testing used is dependent on type of 'x' data, number of
#' levels in the factor 'y', and whether parametric/non-parametric testing is
#' selected. For continuous 'x' data, parametric testing is Student's t-test
#' (y, 2 lvls) or one-way ANOVA (y, >2 lvls). Non-parametric testing is
#' Wilcoxon Rank Sum/Mann Whitney U (y, 2 lvls) or Kruskal Wallis (y, >2 lvls).
#' For factor or logical 'x' data, "parametric" testing is Chi-squared with
#' (x, 2 lvls) and without (x, >2 lvls) Monte Carlo simulation. Non-parametric
#' testing is the Fisher's exact test with (x, 2 lvls) and without (x, >2 lvls)
#' Monte Carlo simulation.
#' @examples
#' # Numeric data
#' test_hypothesis(mtcars$mpg, as.factor(mtcars$gear))
#'
#' # Logical data
#' test_hypothesis(as.logical(mtcars$vs), as.factor(mtcars$gear))
#'
#' # Factor data
#' test_hypothesis(as.factor(mtcars$carb), as.factor(mtcars$gear))
#' @export
test_hypothesis <- function (x, y, parametric, digits, p.digits) {
  UseMethod('test_hypothesis')
}


# Default response
#' @export
test_hypothesis.default <- function (...) as.character(NA)


# Numeric testing methods
#' @export
test_hypothesis.numeric <- function(x, y, parametric = FALSE, digits = 1, p.digits = 4) {
  unique_lvl <- vctrs::vec_size(stats::na.omit(unique(y[!is.na(x)])))
  if (is.factor(y) & unique_lvl >= 2) {
    pv <- if (unique_lvl == 2) {
      if (parametric) stats::t.test(x ~ y, alternative = 'two.sided')$p.value
      else stats::wilcox.test(x ~ y, alternative = 'two.sided')$p.value
    } else if (parametric) summary(stats::aov(x ~ y))[[1]][[1,"Pr(>F)"]]
    else stats::kruskal.test(x ~ y)$p.value
    format.pval(pv = pv, digits = digits, eps = 0.0001, nsmall = p.digits, scientific = F)
  } else as.character(NA)
}


# Categorical testing methods
#' @export
test_hypothesis.factor <- function(x, y, parametric = FALSE, digits = 1, p.digits = 4) {
  contTable <- table(x, y)
  contTable <- contTable[rowSums(contTable) > 0, colSums(contTable) > 0, drop = FALSE]
  if (
    (is.factor(y) | is.logical(y)) &
    as.integer(nrow(contTable)) > 1L &
    as.integer(ncol(contTable)) > 1L &
    vctrs::vec_size(stats::na.omit(unique(x))) > 1 &
    vctrs::vec_size(stats::na.omit(unique(y))) > 1
  ) {
    pv <-
      if (vctrs::vec_size(stats::na.omit(unique(x))) == 2) {
        if (parametric) stats::chisq.test(x, y)$p.value
        else stats::fisher.test(x, y)$p.value
      } else {
        if (parametric) stats::chisq.test(x, y, simulate.p.value = TRUE)$p.value
        stats::fisher.test(x, y, simulate.p.value = TRUE)$p.value
      }
    format.pval(pv = pv, digits = digits, eps = 1e-04, nsmall = p.digits, scientific = F)
  } else as.character(NA)
}

#' @export
test_hypothesis.logical <- test_hypothesis.factor
