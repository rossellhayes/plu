#' Collapse a vector into a natural language string with a maximum number
#' of elements
#'
#' @inheritParams plu_stick
#' @param max The maximum number of items to list.
#'     Additional arguments are replaced with "{n} more".
#'     Defaults to `5`.
#' @param type A [logical] or [character].
#'   * If a character, `type` is pasted after the number of elements.
#'   * If `TRUE`, the default, the first [class] of `x` is pasted after the
#'   number of elements.
#'   * If `x` is not [atomic] (e.g. a [list]), "element" is used in place of a
#'   class name.
#'   * If `FALSE` or `NA`, nothing is pasted after the number of elements.
#' @param fn A function to apply to the number of additional elements.
#'   Default to [`NULL`], which applies no function.
#' @param ... Additional arguments to `fn`.
#' @param det A determiner to place before the number of additional elements.
#'   Defaults to "more".
#'
#' @return A character vector of length 1.
#' @export
#'
#' @example examples/plu_more.R

plu_more <- function(
  x, max = 5, type = TRUE, fn = NULL, ..., det = "more",
  sep = ", ", conj = " and ", oxford = getOption("plu.oxford_comma", FALSE)
) {
  if (!is.null(max) && length(max) != 1) {
    stop("`max` must be length one or NULL.")
  }

  if (!is.null(type) && length(type) != 1) {
    stop("`type` must be length one or NULL.")
  }

  if (!is.null(fn) && !is.function(fn)) {
    stop("`fn` must be an unquoted function name or `NULL`")
  }

  n <- length(x)

  if (is.null(max)) {max <- Inf}

  if (isTRUE(type)) {
    if (is.atomic(x)) {
      type <- paste0(" ", class(x)[[1]])
    } else {
      type <- " element"
    }
  } else if (is.null(type) || isFALSE(type) || is.na(type)) {
    type <- NULL
  } else {
    type <- paste0(" ", type)
  }

  if (is.null(fn)) {fn <- identity}

  if (max < 1 || n < 1) {return(paste0(fn(n, ...), plu_ral(type, n = n)))}

  n <- min(ceiling(n - max), n)

  if (n <= 0) {return(plu_stick(x, sep = sep, conj = conj, oxford = oxford))}

  if (oxford && length(x) > 1) {conj <- plu_nge(paste0(sep, conj))}

  if (!is.null(det)) {paste0(" ", det)}

  paste0(
    plu_stick(x[seq_len(max)], sep = sep, conj = NULL),
    conj, fn(n, ...), paste0(" ", det), plu_ral(type, n = n)
  )
}

#' @rdname plu_more
#' @export

more <- plu_more
