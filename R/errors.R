is_t_or_f <- function(x) {is.logical(x) && !is.na(x)}

assert_t_or_f <- function(x, arg = NULL) {
  if (is.null(arg))  {arg <- deparse(substitute(x))}
  if (!is_t_or_f(x)) {stop("`", arg, "` must be TRUE or FALSE", call. = FALSE)}
}

assert_length_1 <- function(x, arg = NULL) {
  if (is.null(arg))   {arg <- deparse(substitute(x))}
  if (length(x) != 1) {stop("`", arg, "` must be length one", call. = FALSE)}
}

assert_length_1_or_null <- function(x, arg = NULL) {
  if (is.null(arg)) {arg <- deparse(substitute(x))}
  if (!is.null(x) && length(x) != 1) {
    stop("`", arg, "` must be length 1 or NULL", call. = FALSE)
  }
}

assert_type <- function(x, type, arg = NULL) {
  if (is.null(arg))      {arg  <- deparse(substitute(x))}
  if (type == "numeric") {type <- c("numeric", "integer")}

  if (!inherits(x, type)) {
    stop("`", arg, "` must be of type ", type[[1]], call. = FALSE)
  }
}
