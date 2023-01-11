#' @title Calculate durations of time
#' @description
#' Calculates the duration of time between two provided date objects.
#' Supports vectorized data (i.e. \code{\link[dplyr:mutate]{dplyr::mutate()}}).
#' @param x A date or datetime. The start date(s)/timestamp(s).
#' @param y A date or datetime. The end date(s)/timestamp(s).
#' @param units A character. Units of the returned duration
#' (i.e. 'seconds', 'days', 'years').
#' @return If 'units' specified, returns numeric. If 'units' unspecified,
#' returns an object of class '\code{\link[lubridate:Duration-class]{Duration}}'.
#' @note Supports multiple calculations against a single time point (i.e.
#' multiple start dates with a single end date). Note that start and end
#' must otherwise be of the same length.
#'
#' When the start and end dates are of different types (i.e. x = date,
#' y = datetime), a lossy cast will be performed which strips the datetime data
#' of its time components. This is done to avoid an assumption of more time
#' passing that would otherwise come with casting the date data to datetime.
#' @examples
#' library(lubridate)
#' library(purrr)
#'
#' # Dates -> duration in years
#' calc_duration(
#'   x = mdy(map_chr(sample(1:9, 5), ~ paste0('01/01/199', .x))),
#'   y = mdy(map_chr(sample(1:9, 5), ~ paste0('01/01/200', .x))),
#'   units = 'years'
#' )
#'
#' # datetimes -> durations
#' calc_duration(
#'   x = mdy_hm(map_chr(sample(1:9, 5), ~ paste0('01/01/199', .x, ' 1', .x, ':00'))),
#'   y = mdy_hm(map_chr(sample(1:9, 5), ~ paste0('01/01/200', .x, ' 0', .x, ':00')))
#' )
#'
#' # Mixed date classes -> durations
#' calc_duration(
#'   x = mdy(map_chr(sample(1:9, 5), ~ paste0('01/01/199', .x))),
#'   y = mdy_hm(map_chr(sample(1:9, 5), ~ paste0('01/01/200', .x, ' 0', .x, ':00')))
#' )
#' @export
calc_duration <- function (x, y, units = NULL) {

  # Input type check
  if (
    !all(lubridate::is.timepoint(x), na.rm = TRUE) |
    !all(lubridate::is.timepoint(y), na.rm = TRUE)
  ) {
    stop('\'x\' and/or \'y\' not <date> or <datetime>.')
  }

  # Recycle single timepoint or throw error for mismatched sizes
  common_dates <- vctrs::vec_recycle_common(x = x, y = y)

  # Remove timestamp if one variable is a Date object
  if (any(class(x) != class(y), na.rm = TRUE)) {
    common_dates <- purrr::map(common_dates, as.Date)
  }

  # Calculate duration
  duration <- lubridate::as.duration(lubridate::interval(x, y))

  # Return data as appropriate type
  if (!is.null(units)) as.numeric(duration, units)
  else duration

}


#' @title Calculate data chunk indices
#' @description
#' Calculates chunk indices of a data object
#' for a given chunk size (number of items per chunk).
#' @param x A data frame or vector.
#' @param size An integer. The number of items (e.g. rows in a tibble)
#' that make up a given chunk. Must be a positive integer. Caps out at data
#' maximum.
#' @param reverse A logical. Calculate chunks from back to front.
#' @return An iterable list of row indices for each chunk of data.
#' @examples
#' # Create chunk map for a data frame
#' chunks <- calc_chunks(mtcars, size = 6)
#'
#' # Iterate through chunks of data
#' for (chunk in chunks) print(paste0(rownames(mtcars[chunk,]), collapse = ', '))
#' @export
calc_chunks <- function (x, size = 10, reverse = FALSE) {

  # Hard stops
  if (!is.data.frame(x) & !is.vector(x))
    stop('\'x\' not a <data.frame> or vector.')
  if (!is.numeric(size) | size < 1)
    stop('\'size\' not <numeric> or less than 1.')

  # Variables
  item_cnt <- vctrs::vec_size(x)
  if (size > item_cnt) size <- item_cnt

  # Calculate and return chunks
  if (!reverse) purrr::map(1:ceiling(item_cnt / size), ~ ((.x-1)*size+1):min(item_cnt, (.x*size)))
  else purrr::map(1:ceiling(item_cnt / size), ~ (item_cnt-(.x-1)*size):max(1, item_cnt-(.x*size)+1))

}


#' @title Calculate cumulative summation of vector
#' @description
#' Calculate the cumulative summation of a numeric vector with
#' revised NA handling compared to `base::cumsum()`.
#' @param x A numeric.
#' @param na.fill A logical. Impute forward for NA values.
#' @return A vector of the same length and type as x.
#' @examples
#' x <- 1:10
#' x[3:4] <- NA
#'
#' # Calculate cumsum & replace NA values
#' calc_cumsum(x, na.fill = TRUE)
#' @export
calc_cumsum <- function (x, na.fill = FALSE) {
  x <- replace(x, !is.na(x), cumsum(x[!is.na(x)]))
  if (na.fill) x <- c(NA_real_, x[!is.na(x)])[cumsum(!is.na(x)) + 1]
  x
}
