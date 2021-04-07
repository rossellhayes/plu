error <- function(...) {
  stop(..., call. = FALSE)
}

style <- function(x, quote, color) {
  if (!is.null(quote)) {x <- encodeString(x, quote = quote)}
  if (requireNamespace("crayon", quietly = TRUE)) {
    x <- do.call(color, list(x), envir = asNamespace("crayon"))
  }
  x
}

code  <- function(x) {style(x, "`", "silver")}
value <- function(x) {style(x, '"', "blue")}

is_t_or_f <- function(x) {is.logical(x) && !is.na(x)}

assert_t_or_f <- function(x, arg = NULL) {
  if (is.null(arg))  {arg <- code(deparse(substitute(x)))}
  if (!is_t_or_f(x)) {error(arg, " must be TRUE or FALSE")}
}

assert_length_1 <- function(x, arg = NULL) {
  if (is.null(arg))   {arg <- code(deparse(substitute(x)))}
  if (length(x) != 1) {error(arg, " must be length one")}
}

assert_length_1_or_null <- function(x, arg = NULL) {
  if (is.null(arg))                  {arg <- code(deparse(substitute(x)))}
  if (!is.null(x) && length(x) != 1) {error(arg, " must be length 1 or NULL")}
}

assert_type <- function(x, type, arg = NULL) {
  if (is.null(arg))      {arg  <- code(deparse(substitute(x)))}
  if (type == "numeric") {type <- c("numeric", "integer")}
  if (inherits(x, "array") || inherits(x, "matrix")) {x <- x[1]}

  if (!inherits(x, type)) {error(arg, " must be of type ", value(type[[1]]))}
}
