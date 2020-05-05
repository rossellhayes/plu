#' Collapse character vectors into natural language strings
#'
#' @param vector A character vector (or a vector coercible to character)
#' @param fn A function to apply to all items in the list
#' @param ... Additional arguments to `fn`
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
#' @seealso [glue::glue_collapse()] for a generalized way to collapse vectors
#'     into a single string
#'
#' @importFrom rlang %||%
#' @export
#'
#' @example examples/plu_stick.R

plu_stick <- function(
  vector, fn = NULL, ..., sep = ", ", conj = "and",
  syndeton = c("last", "all", "none"),
  oxford   = getOption("plu.oxford_comma")
) {
  if (match.arg(syndeton) == "all") {
    sep  <- paste0(" ", conj, " ")
    last <- sep
  } else if (match.arg(syndeton) == "none") {
    last <- sep
  } else {
    oxford <- oxford %||% grepl("United States|US", Sys.getlocale("LC_COLLATE"))
    last   <- ifelse(
      length(vector) > 2 & oxford,
      paste0(sep, conj, " "),
      paste0(" ", conj, " ")
    )
  }

  if (!is.null(fn)) vector <- lapply(vector, fn, ...)

  glue::glue_collapse(vector, sep = sep, last = last)
}

#' @rdname plu_stick
#' @export

stick <- plu_stick
