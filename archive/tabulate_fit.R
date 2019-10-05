#' @title tabulate_fit
#' @description Extracts fit data from a fit/model object for use in
#' graphing packages (i.e. ggplot2).
#' @param fit Required. survival::survfit() object.
#' @return Tibble containing graphable fit data.
#' @note survfit() methods adapted from the 'survminer' package function
#' surv_summary() by Alboukadel Kassambara [GLP-2].
#' @examples
#' library(dplyr)
#' library(survival)
#'
#' data_cgd <- as_tibble(cgd)
#'
#' # Survfit Object
#' tabulate_fit(
#'    fit = survfit(
#'       Surv(tstart, tstop, status) ~ sex,
#'       data = data_cgd
#'    )
#' )
#' @export
tabulate_fit <- function(fit) {
  UseMethod('tabulate_fit')
}

#' @export
tabulate_fit.default <- function(fit = NULL)
  warning(paste0('Object of class \'', class(fit), '\' not supported.'))

#' @export
tabulate_fit.survfit <- function(fit) {

  # Data preparation
  data_source <- eval(fit$call$data)
  data_surv <- dplyr::bind_cols(
    dplyr::filter_all(
      .tbl = tibble::as_tibble(unclass(fit)[c("time", "n.risk", "n.event", "n.censor")]),
      function(x) !is.na(x)
    ),
    tibble::as_tibble(unclass(fit)[c('surv', 'upper', 'lower')])
  )

  # Prepare strata data (if there is any)
  if (!is.null(fit$strata)) {
    data_surv$strata <- rep(names(fit$strata), fit$strata)
    variables <- intersect(
      unique(
        purrr::map_chr(
          data_surv$strata,
          function (x) {
            x <- unlist(stringr::str_split(x, '=|,\\s+'))
            x[seq(1, length(x), 2)]
          }
        )
      ),
      colnames(data_source)
    )
    for (variable in variables) {
      strata <- purrr::map_chr(
        data_surv$strata,
        function(x) {
          x <- unlist(stringr::str_split(x, "=|(\\s+)?,\\s+"))
          index <- grep(paste0("^", variable, "$"), x)
          stringr::str_trim(x[index+1])
        }
      )
      var_levels <- levels(data_source[, variable])
      if(!is.null(var_levels)) data_surv[[variable]] <- factor(strata, levels = var_levels)
      else data_surv[[variable]] <- as.factor(strata)
    }
  }

  # Connect to origin
  if("n.risk" %in% colnames(data_surv)) data_surv <- dplyr::arrange(data_surv, dplyr::desc(n.risk))
  if ('strata' %in% names(data_surv)) origin <- dplyr::distinct(.data = data_surv, strata, .keep_all = TRUE)
  else origin <- data_surv[1,]
  origin[intersect(c('time', 'n.censor', 'std.err', "n.event"), colnames(origin))] <- 0
  origin[c('surv', 'upper', 'lower')] <- 1.0
  data_surv <- dplyr::bind_rows(origin, data_surv)
  data_surv
}
