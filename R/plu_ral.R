#' Pluralize a phrase based on the length of a vector
#'
#' @param x An English word or phrase to be pluralized.
#'     See details for special sequences which are handled differently.
#' @param vector A vector whose length determines `n`. Defaults to length 2.
#' @param n_fn A function to apply to the output of the special sequence
#'     `"n"`. See examples.
#'     Defaults to `identity`, which returns `n` unchanged.
#' @param ... Additional arguments passed to the function `n_fn`.
#' @param n The number which will determine the plurality of `x`.
#'     Defaults to `length(n)`. If specified, overrides `vector`.
#' @param pl A logical value indicating whether to use the plural form (if
#'     `TRUE`) or the singular form (if `FALSE`) of `x`.
#'     Defaults to `FALSE` when `n` is `1` or `-1` and `TRUE` for all other
#'     values.
#'     If specified, overrides `n`.
#' @param irregulars What level of irregularity to use in pluralization.
#'     `"moderate"` uses the most common pluralization.
#'     `"conservative"` uses the most common irregular plural if one exists,
#'     even if a regular plural is more common.
#'     `"liberal"` uses a regular plural if it exists, even if an irregular
#'     plural is more common.
#'     `"none"` attempts to apply regular noun pluralization rules to all words.
#'     Defaults to `"moderate"`.
#'     The default can be changed by setting `options(plu.irregulars)`.
#'     See examples in [plu::ralize()] for more details.
#' @param replace_n A logical indicating whether to use special handling for
#'     `"n"`.
#'     See details.
#'     Defaults to `TRUE`.
#'
#' @details Certain strings in `x` are treated specially.
#'
#' - By default, `"a"` and `"an"` are deleted in the plural
#' ("a word" to "words").
#'
#' - The string `"n"` will be replaced with the length of `vector` or the
#' number in `n`.
#'     - This output can be modified with `n_fn`.
#'
#' - Strings between braces separated by a pipe will be treated as a custom
#' plural (`"{a|some} word"` to "a word", "some words").
#'     - Three strings separated by pipes will be treated as a singular, dual,
#'     and plural form (`"{the|both|all} word"` to "the word" (1),
#'     "both words" (2), "all words" (3+)).
#'
#' - Any other string between braces will be treated as invariant
#' (`"attorney {general}"` to "attorneys general").
#'
#' @return The character vector `x` altered to match the number of `n`
#'
#' @seealso [plu::ralize()] to convert an English word to its plural form.
#'
#' @export
#'
#' @example examples/plu_ral.R

plu_ral <- function(
  x, vector = integer(2), n_fn = NULL, ...,
  n = length(vector), pl = abs(n) != 1,
  irregulars = c("moderate", "conservative", "liberal", "none"),
  replace_n = TRUE
) {
  if (!length(x))                  return(character(0))
  if (!is.character(x))            stop("`x` must be a character vector")
  if (length(n) != 1)              stop("`n` must be length one")
  if (!is.numeric(n))              stop("`n` must be numeric")
  if (length(pl) != 1)             stop("`pl` must be length one")
  if (!is.logical(pl) | is.na(pl)) stop("`pl` must be TRUE or FALSE")
  if (length(replace_n) != 1)      stop("`replace_n` must be length one")
  if (!is.logical(replace_n) | is.na(replace_n))
    stop("`replace_n` must be TRUE or FALSE")
  if (!is.null(n_fn) & !is.function(n_fn))
    stop("`n_fn` must be an unquoted function name")

  start_space <- substr(x, 1, 1) == " "
  end_space   <- substr(x, nchar_x <- nchar(x), nchar_x) == " "

  if (pl) {
    x <- unlist(strsplit(x, "(?=[^A-Za-z0-9'\\-{])(?![^{]*})", perl = TRUE))

    braced     <- grepl(paste0("\\{|\\}", ifelse(replace_n, "|\\bn\\b", "")), x)
    x[!braced] <- plu_ralize(x[!braced], irregulars = irregulars)
    x          <- paste(x, collapse = "")
  }

  x <- gsub(
    "\\{([^{}\\|]*?)\\|([^{}\\|]*?)\\|([^{}\\|]*?)\\}",
    ifelse(abs(n) == 1, "\\1", ifelse(abs(n) == 2, "\\2", "\\3")),
    x
  )
  x <- gsub(
    "\\{([^{}\\|]*?)\\|([^{}\\|]*?)\\}",
    ifelse(abs(n) == 1, "\\1", "\\2"),
    x
  )

  if (!is.null(n_fn)) n <- n_fn(n, ...)
  if (replace_n)      x <- gsub("\\bn\\b", n, x)

  x <- gsub("\\{([^{}]*)\\}", "\\1", x)
  x <- plu_nge(x, ends = TRUE)

  if (start_space) x <- paste0(" ", x)
  if (end_space)   x <- paste0(x, " ")

  x
}

#' @rdname plu_ral
#' @export

ral <- plu_ral
