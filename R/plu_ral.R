# @staticimports pkg:stringstatic
#   regex str_detect str_replace_all

#' Pluralize a phrase based on the length of a vector
#'
#' @param x A character vector (or vector that can be coerced to character)
#'     of English words or phrase to be pluralized.
#'     See details for special sequences which are handled differently.
#' @param vector A vector whose length determines `n`. Defaults to `NULL`.
#' @param n A numeric vector which will determine the plurality of `x`.
#'     Defaults to `length(vector)`.
#'     If specified, overrides `vector`.
#' @param pl A logical vector indicating whether to use the plural form (if
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
#' @param replace_n A logical indicating whether to use special handling for `"n"`.
#'     See details.
#'     Defaults to `TRUE`.
#' @param open,close The opening and closing delimiters for special strings.
#'     See section "Special strings". Defaults to `"{"` and `"}"`.
#' @param n_fn \lifecycle{deprecated}
#' @param ... \lifecycle{deprecated}
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
  x,
  vector = NULL,
  n = NULL,
  pl = NULL,
  irregulars = c("moderate", "conservative", "liberal", "none"),
  replace_n = TRUE,
  open = "{",
  close = "}",
  n_fn = lifecycle::deprecated(),
  ...
) {
  if (length(x) == 0) {return(character(0))}
  mode(x) <- "character"

  derived_plurality <- derive_plurality(x, vector, n, pl)
  pl <- derived_plurality$pl
  n <- derived_plurality$n

  # In case `x` is length 1 and `n` is length > 1
  x <- recycle(x, n)

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

  braced <- str_detect(
    split_in,
    regex(paste0(open, ".*", close), multiline = TRUE, dotall = TRUE)
  )

  assert_length_1(replace_n)
  assert_t_or_f(replace_n)
  if (replace_n) {
    replace_n <- str_detect(split_in, "\\bn\\b")
  }

  if (lifecycle::is_present(n_fn)) {
    lifecycle::deprecate_warn(when = "0.2.4", what = "plu_ral(n_fn)")
  }

  split_out <- split_in

  if (any(pl)) {
    pl <- rep(pl, each = nrow(split_out))

    # Pluralize words that aren't wrapped in {braces}
    split_out[pl & !braced & !replace_n] <- plu_ralize(
      split_in[pl & !braced & !replace_n], irregulars = irregulars
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
  n <- rep(n, each = nrow(split_out))
  split_out[braced] <- pluralize_braces(
    split_out[braced], n[braced], open, close
  )

  # replace "n" with `n`
  split_out[replace_n] <- str_replace_all(
    split_out[replace_n],
    pattern = "\\bn\\b",
    replacement = n[replace_n]
  )

  mat[] <- apply(split_out, 2, paste, collapse = "")

  x[] <- apply(mat, 2, paste, collapse = "")
  x
}

#' @rdname plu_ral
#' @export

ral <- plu_ral

# @staticimports pkg:staticimports
#   %||%

derive_plurality <- function(x, vector, n, pl) {
  if (is.null(n)) {
    n <- n %||% length(vector)
  }

  assert_type(n, "numeric")

  if (is.null(pl)) {
    pl <- pl %||% (abs(n) != 1)
  }

  assert_t_or_f(pl)

  n <- recycle(n, along = x)
  pl <- recycle(pl, along = x)

  n[pl & abs(n) == 1] <- 0
  n[!pl & abs(n) != 1] <- 1

  list(pl = pl, n = n)
}

recycle <- function(x, along) {
  x_name <- code(deparse(substitute(x)))
  along_name <- code(deparse(substitute(along)))

  if (length(along) == 1) return(x)
  if (length(x) == length(along)) return(x)
  if (length(x) == 1) return(rep(x, length(along)))
  error(
    "Cannot recycle ", x_name, " (length ", length(x), ") ",
    "to match ", along_name, " (length ", length(along), ")."
  )
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

  brace_list$mid <- Map(
    function(x, n) {
      if (abs(n) %in% seq_along(x)) {return(x[abs(n)])}
      x[length(x)]
    },
    x = strsplit(brace_list$mid, "\\|"),
    n = n
  )

  paste0(brace_list$pre, brace_list$mid)
}
