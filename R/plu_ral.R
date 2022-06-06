#' Pluralize a phrase based on the length of a vector
#'
#' @param x An character vector of English words or phrase to be pluralized.
#'     See details for special sequences which are handled differently.
#' @param vector A vector whose length determines `n`. Defaults to `NULL`.
#' @param n_fn A function to apply to the output of the special sequence
#'     `"n"`. See examples.
#'     Defaults to `identity`, which returns `n` unchanged.
#' @param ... Additional arguments passed to the function `n_fn`.
#' @param n The number which will determine the plurality of `x`.
#'     Defaults to `length(vector)`.
#'     If specified, overrides `vector`.
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
#'     See section "Irregular plurals" for more details.
#'     Defaults to `"moderate"`.
#'     The default can be changed by setting `options(plu.irregulars)`.
#'     See examples in [plu::ralize()] for more details.
#' @param replace_n A logical indicating whether to use special handling for
#'     `"n"`.
#'     See details.
#'     Defaults to `TRUE`.
#' @param open,close The opening and closing delimiters for special strings.
#'     See section "Special strings". Defaults to `"{"` and `"}"`.
#'
#' @section Irregular plurals:
#'
#' Many words in English have both regular and irregular plural forms.
#' For example, the word "person" can be pluralized as "persons" or "people",
#' and the word "formula" can be pluralized as "formulas" or "formulae".
#' `plu` offers several options for how to handle words with multiple plurals.
#'
#' - The `moderate` list attempts to apply the most common pluralization,
#' whether it is regular or irregular.
#' This chooses the irregular plural "people" but the regular plural "formulas".
#'
#' - The `conservative` list attempts to apply an irregular plural to every word
#' that has one.
#' This chooses "people" and "formulae", but still uses regular plurals for
#' words that have no irregular plural form.
#'
#' - The `liberal` list attempts to apply a regular plural to every word that
#' has one.
#' This chooses "persons" and "formulas", but still uses irregular plurals for
#' words that have no common regular plural, like "women".
#' Many words in English have invariant plurals that look exactly the same as
#' their singular forms, like "fish" or "deer".
#' The `liberal` list attempts to use regular plurals for these words,
#' producing "fishes" and "deers".
#'
#' - The `none` list applies regular pluralization rules to all words, even
#' those with no common regular plural.
#' This produces, for example, "womans" as a plural for "woman" even though this
#' is not a common English word.
#'
#' @section Special strings:
#'
#' Certain strings in `x` receive special treatment.
#'
#' - By default, `"a"` and `"an"` are deleted in the plural
#' ("a word" to "words").
#'
#' - The string `"n"` will be replaced with the length of `vector` or the
#' number in `n`.
#'     - This output can be modified with `n_fn`.
#'
#' - Strings between `open` and `close` separated by a pipe will be treated as a
#' custom plural (`"{a|some} word"` to "a word", "some words").
#'     - More than two strings separated by pipes will be treated as singular,
#'     dual, trial, ... and plural forms. For example, `"{the|both|all} word"`
#'     to "the word" (1), "both words" (2), "all words" (3+).
#'     - See examples for more.
#'
#' - Any other string between `open` and `close` without a pipe will be treated
#' as invariant.
#' For example, `"attorney {general}"` to "attorneys general" (notice
#' "general" is not pluralized).
#'
#' @return The character vector `x` altered to match the number of `n`
#'
#' @seealso [plu_ralize()] to convert an English word to its
#' plural form.
#'
#' @export
#' @example examples/plu_ral.R

plu_ral <- function(
  x, vector = NULL, n_fn = NULL, ..., n = NULL, pl = NULL,
  irregulars = c("moderate", "conservative", "liberal", "none"),
  replace_n = TRUE, open = "{", close = "}"
) {
  if (!length(x)) {return(character(0))}
  assert_type(x, "character")

  assert_length_1(replace_n)
  assert_t_or_f(replace_n)

  pl <- derive_pl(pl, n, vector)
  n  <- derive_n(pl, n, vector)

  validate_delimeters(open, close)
  open  <- escape(open)
  close <- escape(close)

  mat <- matrix(x, nrow = 1)

  # Split strings into individual words and punctuation marks
  boundaries <- paste0(
    "((?=[^[:alnum:]'\\-", open, "])", "|",
    "(?<=[^[:alnum:]'\\-", open, "]))",
    "(?![^", open, "]*", close, ")"
  )

  split_in <- plu_split(mat, boundaries, perl = TRUE)

  braced <- str_detect(split_in, paste0(open, ".*", close))

  if (replace_n) {replace_n <- str_detect(split_in, "\\bn\\b")}
  n_fn <- get_fun(n_fn)

  split_out <- split_in

  if (pl) {
    # Pluralize words that aren't wrapped in {braces}
    split_out[!braced & !replace_n] <- plu_ralize(
      split_in[!braced & !replace_n], irregulars = irregulars
    )

    # Find where "a" or "an" have been removed
    removed <- which(split_out == "" & split_in != "")
    # Exclude ends of lines
    removed <- removed[removed %% nrow(split_out) != 0]

    if (any(removed)) {
      # Capitalize word after removed "A" or "An"
      caps <- which(is_capitalized(split_in))
      caps <- caps[caps %in% removed]

      after_caps <- vapply(
        caps,
        function(i) {
          idx <- which(
            str_detect(
              split_out[seq(i, (i %/% nrow(split_out) + 1) * nrow(split_out))],
              "[[:alpha:]]"
            )
          )
          idx[1] + i - 1L
        },
        integer(1)
      )

      split_out[after_caps] <- capitalize(split_out[after_caps])

      # Remove spaces after removed "a" or "an"
      after_removed <- removed + 1
      split_out[after_removed][split_out[after_removed] == " "] <- ""
    }
  }

  # Select appropriate option for words wrapped in braces
  split_out[braced] <- pluralize_braces(split_out[braced], n, open, close)

  # replace "n" with `n`
  split_out[replace_n] <- str_replace_all(
    split_out[replace_n], "\\bn\\b", n_fn(n, ...)
  )

  mat[] <- apply(split_out, 2, paste, collapse = "")

  x[] <- apply(mat, 2, paste, collapse = "")
  x
}

#' @rdname plu_ral
#' @export

ral <- plu_ral

derive_pl <- function(pl, n, vector) {
  if (!is.null(pl)) {
    assert_length_1(pl)
    assert_t_or_f(pl)
    return(pl)
  }

  if (!is.null(n)) {
    assert_length_1(n)
    assert_type(n, "numeric")
    return(abs(n) != 1)
  }

  abs(length(vector)) != 1
}

derive_n <- function(pl, n, vector) {
  if (!is.null(n)) {
    assert_length_1(n)
    assert_type(n, "numeric")
    if (pl  && abs(n) == 1) {return(2)}
    if (!pl && abs(n) != 1) {return(1)}
    return(n)
  }

  if (pl  && abs(length(vector)) == 1) {return(2)}
  if (!pl && abs(length(vector)) != 1) {return(1)}
  length(vector)
}

validate_delimeters <- function(open, close) {
  arg <- paste(code("open"), "and", code("close"))

  for (delim in list(open, close)) {
    assert_type(delim, "character", arg)
    assert_length_1(delim, arg)
    assert_nchar(delim, arg)
  }

  if (open == close) {error(arg, " must not be the same")}
}

assert_nchar <- function(x, arg = NULL) {
  if (is.null(arg)) {arg <- code(deparse(substitute(x)))}
  if (!nzchar(x))   {error(arg, " must not be an empty string")}
}

escape <- function(x) {
  paste0("\\Q", x, "\\E")
}

pluralize_braces <- function(x, n, open, close) {
  brace_list <- list(
    pre = sub(paste0("(?s)(^.*?)", open, ".*$"),        "\\1", x, perl = TRUE),
    mid = sub(paste0("(?s)^.*?", open, "(.*?)", close), "\\1", x, perl = TRUE)
  )

  brace_list$mid <- vapply(
    strsplit(brace_list$mid, "\\|"),
    function(x) {
      if (abs(n) %in% seq_along(x)) {return(x[abs(n)])}
      x[length(x)]
    },
    character(1)
  )

  paste0(brace_list$pre, brace_list$mid)
}
