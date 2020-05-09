# Aggregate certain object types into a count
.count_items <- function (x, na.rm) {
  if (is.data.frame(x)) vctrs::vec_size(x) # data frame
  else if (!all(is.numeric(x))) {
    vctrs::vec_size(if (na.rm) stats::na.omit(x) else x) # Non-numeric
  } else if (vctrs::vec_is_empty(x)) 0 # empty numeric
  else x # Usable numeric
}
