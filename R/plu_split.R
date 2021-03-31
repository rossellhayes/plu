plu_split <- function(x, pattern, ...) {
  x      <- strsplit(x, pattern, ...)
  max_ln <- max(lengths(x))
  split  <- vapply(
    x, function(x) {c(x, character(max_ln - length(x)))}, character(max_ln)
  )
  matrix(split, ncol = length(x))
}
