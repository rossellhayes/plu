#' Pluralize a phrase based on the length of a vector
#'
#' @param x An English word or phrase to be pluralized.
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
#' @importFrom rlang %||%
#' @export
#'
#' @example examples/plu_ral.R

plu_ral <- function(
  x, vector = NULL, n_fn = NULL, ..., n = NULL, pl = NULL,
  irregulars = c("moderate", "conservative", "liberal", "none"),
  replace_n = TRUE
) {
  if (!length(x))       {return(character(0))}
  if (!is.character(x)) {rlang::abort("`x` must be a character vector")}

  if (length(replace_n) != 1) {rlang::abort("`replace_n` must be length one")}
  if (!is_t_or_f(replace_n)) {rlang::abort("`replace_n` must be TRUE or FALSE")}

  mat <- matrix(x, nrow = 1)

  pl <- derive_pl(pl, n, vector)
  n  <- derive_n(pl, n, vector)

  if (pl) {
    # Split strings into individual words and punctuation marks
    sing_split <- stringi::stri_split_regex(
      mat,
      "((?=[^[:alnum:]'\\-\\{])|(?<=[^[:alnum:]'\\-\\{]))(?![^\\{]*\\})",
      simplify = TRUE
    )
    sing_split <- t(sing_split)

    # Pluralize words that aren't wrapped in {braces}
    braced <- stringi::stri_detect_regex(
      sing_split, paste0("\\{|\\}", ifelse(replace_n, "|\\bn\\b", ""))
    )

    plu_split          <- sing_split
    plu_split[!braced] <- plu_ralize(
      sing_split[!braced], irregulars = irregulars
    )

    # Find where "a" or "an" have been removed
    removed <- which(plu_split == "" & sing_split != "")
    # Exclude ends of lines
    removed <- removed[removed %% nrow(sing_split) != 0]

    if (any(removed)) {
      # Capitalize word after removed "A" or "An"
      caps <- which(is_capitalized(sing_split))
      caps <- caps[caps %in% removed]

      after_caps <- vapply(
        caps,
        function(i) {
          idx <- which(
            grepl(
              "[[:alpha:]]",
              plu_split[seq(i, (i %/% nrow(plu_split) + 1) * nrow(plu_split))]
            )
          )
          idx[1] + i - 1L
        },
        integer(1)
      )

      plu_split[after_caps] <- capitalize(plu_split[after_caps])

      # Remove spaces after removed "a" or "an"
      after_removed <- removed + 1
      plu_split[after_removed][plu_split[after_removed] == " "] <- ""
    }

    mat[] <- apply(plu_split, 2, paste, collapse = "")
  }

  mat[] <- stringi::stri_replace_all_regex(
    mat,
    "\\{([^{}\\|]*?)\\|([^{}\\|]*?)\\|([^{}\\|]*?)\\}",
    switch(as.character(abs(n)), "1" = "$1", "2" = "$2", "$3")
  )

  mat[] <- stringi::stri_replace_all_regex(
    mat,
    "\\{([^{}\\|]*?)\\|([^{}\\|]*?)\\}",
    ifelse(abs(n) == 1, "$1", "$2")
  )

  if (replace_n) {
    n_fn  <- get_fun(n_fn)
    mat[] <- stringi::stri_replace_all_regex(mat, "\\bn\\b", n_fn(n, ...))
  }

  mat[] <- stringi::stri_replace_all_regex(mat, "\\{([^{}]*)\\}", "$1")

  x[] <- apply(mat, 2, paste, collapse = "")
  x
}

#' @rdname plu_ral
#' @export

ral <- plu_ral

derive_pl <- function(pl, n, vector) {
  if (!is.null(pl)) {
    if (length(pl) != 1) {rlang::abort("`pl` must be length one")}
    if (!is_t_or_f(pl))  {rlang::abort("`pl` must be TRUE or FALSE")}
    return(pl)
  }

  if (!is.null(n)) {
    if (!is.numeric(n)) {rlang::abort("`n` must be numeric")}
    return(abs(n) != 1)
  }

  abs(length(vector)) != 1
}

derive_n <- function(pl, n, vector) {
  if (!is.null(n)) {
    if (length(n) != 1)     {rlang::abort("`n` must be length one")}
    if (!is.numeric(n))     {rlang::abort("`n` must be numeric")}
    if (pl  && abs(n) == 1) {return(2)}
    if (!pl && abs(n) != 1) {return(1)}
    return(n)
  }

  if (pl  && abs(length(vector)) == 1) {return(2)}
  if (!pl && abs(length(vector)) != 1) {return(1)}
  length(vector)
}

is_t_or_f <- function(x) {is.logical(x) && !is.na(x)}
