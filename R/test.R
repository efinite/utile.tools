#' @title Test Numeric Data
#' @description Returns p-value from parametric or non-parametric testing of stratified
#' continuous (numeric) data.
#' @param col Required. Character. Name of numeric column containing observations.
#' @param by Required. Character. Name of logical or factor column to stratify by.
#' @param data Required. Tibble. Data being used.
#' @param parametric Optional. Logical. Indicates parametric testing should be used.
#' (Student's T-Test). Defaults to FALSE (non-parametric; Wilcox).
#' @param digits Optional. Integer. Number of digits to round to. Defaults to 1.
#' @param p.digits Optional. Integer. Number of p-value digits to print. Note that
#' p-values are still rounded using 'digits'. Defaults to 4.
#' @export
test_numeric <- function(col = NULL, by = '.by', data = NULL, parametric = FALSE, digits = 1, p.digits = 4) {
  if (!is.null(col) & !is.null(data) & (length(unique(dplyr::filter(data, !is.na(!!rlang::sym(col)))[[by]]))) == 2) {
    f <- stats::formula(paste("`", col, "` ~ `", by, "`", sep = ""))
    pv <- if (!parametric) stats::wilcox.test(formula = f, alternative = 'two.sided', data = data)$p.value
    else stats::t.test(formula = f, alternative = 'two.sided', data = data)$p.value
    format.pval(pv = pv,digits = digits, eps = 0.0001, nsmall = p.digits, scientific = F)
  } else '--'
}


#' @title Test Factor Data
#' @description Returns p-value from parametric or non-parametric testing of stratified categorical
#' (factor) data.
#' @param col Required. Character. Name of logical or factor column containing observations.
#' @param by Required. Character. Name of logical or factor column to stratify by.
#' @param data Required. Tibble. Data being used.
#' @param parametric Optional. Logical. Indicates parametric testing should be used.
#' (Chisquared). Defaults to FALSE (non-parametric; Fisher's Exact).
#' @param digits Optional. Integer. Number of digits to round to. Defaults to 1.
#' @param p.digits Optional. Integer. Number of p-value digits to print. Note that
#' p-values are still rounded using 'digits'. Defaults to 4.
#' @export
test_factor <- function(col = NULL, by = '.by', data = NULL, parametric = TRUE, digits = 1, p.digits = 4) {
  contTable <- table(data[[col]], data[[by]])
  contTable <- contTable[rowSums(contTable) > 0, colSums(contTable) > 0, drop = FALSE]
  if (
    !is.null(col) &
    !is.null(data) &
    as.integer(nrow(contTable)) > 1L &
    as.integer(ncol(contTable)) > 1L &
    length(stats::na.omit(unique(data[[col]]))) > 1 &
    length(stats::na.omit(unique(data[[by]]))) > 1
  ) {
    pv <-
      if (stats::na.omit(length(unique(data[[col]]))) == 2) {
        if (!parametric) stats::fisher.test(data[[col]], data[[by]])$p.value
        else stats::chisq.test(data[[col]], data[[by]])$p.value
      } else {
        if (!parametric) stats::fisher.test(data[[col]], data[[by]], simulate.p.value = TRUE)$p.value
        else stats::chisq.test(data[[col]], data[[by]], simulate.p.value = TRUE)$p.value
      }
    format.pval(pv = pv, digits = digits, eps = 1e-04, nsmall = p.digits, scientific = F)
  } else '--'
}
