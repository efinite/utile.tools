#' @title Break data into chunks
#' @description
#' Creates a factory function which returns a different chunk
#' of a given data object with each function call.
#' @param x A data frame or vector.
#' @param size An integer. The number of items (e.g. rows in a tibble)
#' that make up a given chunk. Must be a positive integer.
#' @param reverse A logical. Calculate chunks from back to front.
#' @return A factory function which returns a chunk of data from the provided
#' object with each call. Once all data has been returned, function returns
#' NULL perpetually.
#' @examples
#' # Create chunk factory function
#' chunked_data <- chunk_data_(mtcars, size = 6)
#'
#' # Chunk #1 (rows 1-6)
#' paste0(rownames(chunked_data()), collapse = ', ')
#'
#' # Chunk #2 (rows 7-12)
#' paste0(rownames(chunked_data()), collapse = ', ')
#' @export
chunk_data_ <- function (x, size = 10, reverse = FALSE) {

  # Calculate chunks & check hard stops
  chunks <- calc_chunks(x = x, size = size, reverse = reverse)

  # Return factory function
  index <- 0
  function () {
    if ((index <<- index + 1) <= vctrs::vec_size(chunks)) {
      vctrs::vec_slice(x, chunks[[index]])
    } else NULL
  }

}
