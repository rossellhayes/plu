# @staticimports pkg:stringstatic
#   str_squish

#' Informatively display a maximum number of elements
#'
#' @param x A vector or list.
#' @param max The maximum number of items to list.
#'     Additional arguments are replaced with "{n} more".
#'     Defaults to `5`.
#'     If `max` if [`Inf`], [`NULL`], [`FALSE`], or [`NA`], all elements are
#'     preserved.
#' @param type A [logical] or [character].
#'   * If a character, `type` is passed to [plu::ral()] and pasted after the
#'   number of elements.
#'   * If [`TRUE`], the default, the first [class] of `x` is used as the type.
#'     - If `x` is a [list] with different classes of element, "element" is used
#'     in place of a class name.
#'   * If [`FALSE`] or [`NA`], nothing is pasted after the number of elements.
#' @param fn A function to apply to the number of additional elements.
#'   Default to [`NULL`], which applies no function.
#' @param ... Additional arguments to `fn`.
#' @param det A determiner to place before the number of additional elements.
#'   Defaults to "more".
#'
#' @return If `x` is a vector, a character vector with a length of `max` + 1
#'   or less.
#'   If `x` is a list, a list with `max + 1` or fewer elements.
#' @export
#'
#' @example examples/plu_more.R

plu_more <- function(x, max = 5, type = TRUE, fn = NULL, ..., det = "more") {
  assert_length_1_or_null(max)
  if (is.null(max) || isFALSE(max) || is.na(max)) {max <- Inf}
  assert_type(max, "numeric")

  n    <- length(x)
  type <- format_type(type, x)
  fn   <- get_fun(fn)

  if (max < 1 || n < 1) {
    return(str_squish(paste(fn(n, ...), plu_ral(type, n = n))))
  }

  n <- min(ceiling(n - max), n)

  if (n <= 0) {return(x)}

  n_more <- str_squish(paste(fn(n, ...), det, plu_ral(type, n = n)))

  c(x[seq_len(max)], n_more)
}

#' @rdname plu_more
#' @export

more <- plu_more

format_type <- function(type, x) {
  assert_length_1(type)

  if (is.character(type) && length(type) == 1)       {return(type)}
  if (is.null(type) || isFALSE(type) || is.na(type)) {return("")}

  if (isTRUE(type)) {
    class <- unique(lapply(x, class))

    if (length(class) == 1) {return(class[[1]][[1]])}

    return("element")
  }

  error(code(type), " must be a character string, a logical, or NULL")
}
