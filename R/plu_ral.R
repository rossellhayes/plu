#' Pluralize a phrase based on the length of a vector
#'
#' @param x An English word or phrase to be pluralized.
#'     See details for special handling of certain words.
#' @param vector A vector whose length determines `n`. Defaults to length 2.
#' @param n The number which will determine the plurality of `x`.
#'     Defaults to `length(n)`.
#' @param pl A logical value indicating whether to use the plural form (if
#'     `TRUE`) or the singular form (if `FALSE`) of `x`.
#'     Defaults to `FALSE` when `n` is `1` or `-1` and `TRUE` for all other
#'     values.
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
#' @param max_english When using the special character `"[n]"`, the number will
#'     be printed in words when `n <= max_english` and numerically when
#'     `n > max_english` ("two words", "21 words"). Defaults to 20.
#'     The default can be changed by setting `options(plu.max_english)`.
#' @param big.mark When using the special characters `"[n]"` and `"[one]"`, a
#'     "big mark" will be placed between every three digits.
#'     Defaults to `","`.
#'     The default can be changed by setting `options(plu.big.mark)`.
#'
#' @details Certain strings in `x` are treated specially.
#'
#' - A string between braces will be treated as invariant
#' (`"attorney {general}"` to "attorneys general").
#'
#' - Strings between braces separated by a pipe will be treated as a custom
#' plural (`"{a|some} word"` to "a word", "some words").
#'
#'     - Three strings separated by pipes will be treated as a singular, dual,
#'     and plural form (`"{the|both|all} word"` to "the word" (1),
#'     "both words" (2), "all words" (3+)).
#'
#' - The number of `n` can be added to the phrase using strings in braces.
#'
#'     - `[1]` will be replaced with `n` printed numerically
#'     (`"[1] word"` to "2 words").
#'
#'     - `[one]` will be replaced with `n` printed in words
#'     (`"[one] word"` to "two words", `"[One] word"` to "Two words").
#'
#'     - `[n]` will dynamically choose between printing numerically and in words
#'     based on `max_english` (`"[n] word"` to "two words", "21 words").
#'
#' @return The character vector `x` altered to match the number of `n`
#'
#' @seealso [plu::ralize()] to convert an English word to its plural form.
#'
#' [english::as.english()] to print a number in English words.
#'
#' @importFrom rlang %||%
#' @export
#'
#' @example examples/plu_ral.R

plu_ral <- function(
  x, vector = integer(2), n = length(vector), pl = abs(n) != 1,
  irregulars  = c("moderate", "conservative", "liberal", "none"),
  max_english = getOption("plu.max_english"),
  big.mark    = getOption("plu.big.mark")
) {
  if (pl) {
    x <- stringr::str_split(
      x, " (?![^[\\[\\{]]*[\\]\\}])", simplify = TRUE
    )

    x[!grepl("\\{|\\}|\\[|\\]", x)] <- plu::ralize(
      x[!grepl("\\{|\\}|\\[|\\]", x)], irregulars = irregulars
    )

    x <- paste(x, collapse = " ")
  }

  if (any(grepl("\\{|\\[", x))) {
    num_n <- format(n, big.mark = big.mark %||% ",")
    eng_n <- ifelse(
      rlang::is_integerish(n),
      as.character(english::as.english(n)),
      num_n
    )

    x <- stringr::str_replace_all(
      x,
      c(
        "\\{(.*?)\\|(.*?)\\|(.*?)\\}" = ifelse(
          abs(n) == 1, "\\1", ifelse(abs(n) == 2, "\\2", "\\3")
        ),
        "\\{(.*?)\\|(.*?)\\}" = ifelse(abs(n) == 1, "\\1", "\\2"),
        "\\{(.*?)\\}"         = "\\1",
        "\\[n\\]"             = ifelse(n > max_english %||% 20, num_n, eng_n),
        "\\[1\\]"             = num_n,
        "\\[one\\]"           = eng_n,
        "\\[ONE\\]"           = toupper(eng_n),
        "\\[One\\]"           = stringr::str_to_sentence(eng_n)
      )
    )
  }

  stringr::str_squish(x)
}

#' @rdname plu_ral
#' @export

ral <- plu_ral

#' @rdname plu_ral
#' @export

plu <- plu_ral
