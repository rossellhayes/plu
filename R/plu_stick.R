#' Collapse character vectors into natural language strings
#'
#' @param x A character vector (or a vector coercible to character)
#' @param fn A function to apply to all items in the list
#' @param ... Additional arguments to `fn`
#' @param max The maximum number of items to list.
#'     Additional arguments are replaced with "{n} more".
#'     Defaults to `Inf`, which prints all items.
#' @param fn_overflow Whether to apply `fn` to the overflow message when `x`
#'     contains more items than `max`.
#'     Defaults to `FALSE`.
#' @param sep The mark to place between list items. Defaults to `", "`
#' @param conj A conjunction to place between list items. Defaults to `"and"`.
#' @param syndeton Whether to place the conjunction before the `"last"` list
#'     items, between `"all"` list items, or between `"none"`.
#'     Defaults to `"last"`.
#' @param oxford A logical value indicating whether to place `sep` before the
#'     last list item (x, y, and z) or not (x, y and z) in lists of length three
#'     or more where `syndeton` is `"last"`.
#'     Defaults to `TRUE` if R's locale is set to the United States and `FALSE`
#'     otherwise.
#'     The default can be changed by setting `options(plu.oxford_comma)`.
#'
#' @return A character vector of length 1
#'
#' @export
#'
#' @example examples/plu_stick.R

plu_stick <- function(
  x, fn = NULL, ..., max = Inf, fn_overflow = FALSE, sep = ", ", conj = " and ",
  syndeton = c("last", "all", "none"),
  oxford   = getOption("plu.oxford_comma")
) {
  if (!length(x))               return(character(0))
  if (length(max) != 1)         stop("`max` must be length one")
  if (max < 1)                  stop("`max` must be greater than or equal to 1")
  if (length(sep) != 1)         stop("`sep` must be length one")
  if (!is.character(sep))       stop("`sep` must be a character string")
  if (length(conj) != 1)        stop("`conj` must be length one")
  if (!is.character(conj))      stop("`conj` must be a character string")
  if (length(fn_overflow) != 1) stop("`fn_overflow` must be length one")
  if (!is.logical(fn_overflow) | is.na(fn_overflow))
    stop("`fn_overflow` must be `TRUE` or `FALSE`")
  if (!is.null(fn) & !is.function(fn))
    stop("`fn` must be an unquoted function name")
  if (!is.null(oxford)) {
    if (length(oxford) != 1) stop("`oxford` must be length one")
    if (!is.logical(oxford) | is.na(oxford))
      stop("`oxford` must be TRUE or FALSE")
  }

  overflow <- length(x) - max

  if (overflow <= 0) {
    overflow <- 0
  } else {
    x <- c(x[seq_len(max)], paste(overflow, "more"))
  }

  if (!is.null(fn)) {
    if (fn_overflow | overflow == 0) {
      x <- lapply(x, fn, ...)
    } else {
      x <- as.list(x)
      x[-length(x)] <- lapply(x[-length(x)], fn, ...)
    }
  }

  phrase                       <- character(length(x) * 2 - 1)
  phrase[seq_along(x) * 2 - 1] <- x

  syndeton <- match.arg(syndeton)

  if (syndeton == "last") {
    if (is.null(oxford)) {
      oxford <- grepl("United States|US", Sys.getlocale("LC_COLLATE"))
    }

    phrase[length(x) * 2 - 2] <- ifelse(
      length(x) > 2 & oxford, plu_nge(paste0(sep, conj)), conj
    )

    phrase[phrase == ""] <- sep
  } else if (syndeton == "all") {
    phrase[phrase == ""] <- conj
  } else {
    phrase[phrase == ""] <- sep
  }

  paste0(phrase, collapse = "")
}

#' @rdname plu_stick
#' @export

stick <- plu_stick
