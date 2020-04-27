# Aggregate certain object types into a count
.count_items <- function (x, na.rm) {
  if (is.data.frame(x)) nrow(x)
  else if (!all(is.numeric(x))) length(if (na.rm) stats::na.omit(x) else x)
  else if (length(x) == 0) 0
  else x
}
