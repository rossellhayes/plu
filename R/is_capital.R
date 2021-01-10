#' Capitalization
#'
#' `capitalize()` returns a character vector `x` with the first
#' alphabetic character replaced with a capital form (if one exists).
#'
#' `is_capital()` returns [`TRUE`] if all characters are capitals, [`FALSE`] if
#' all characters are lowercase, and [`NA`] if characters are mixed case or any
#' characters are caseless (e.g. numbers, punctuation marks, characters from a
#' unicase language like Arabic, Chinese or Hindi).
#'
#' `is_capitalized()` returns [`TRUE`] if the first alphabetic character in a
#' string is capital, [`FALSE`] if the first alphabetic character is lowercase,
#' and [`NA`] if there are no alphabetic characters or the first alphabetic
#' character is caseless (i.e. from a unicase language like Arabic, Chinese
#' or Hindi).
#'
#' @param x A character vector
#'
#' @return `capitalize()` returns a character vector of the same length as `x`.
#'
#'   `is_capital()` and `is_capitalized()` return a logical vector of the same
#'   length as `x`.
#'
#' @export
#'
#' @example examples/is_capital.R

capitalize <- function(x) {
  first_letter       <- stringi::stri_extract_first_regex(x, "[:alpha:]")
  capitalized_letter <- stringi::stri_trans_toupper(first_letter)

  stringi::stri_replace_first_regex(x, "[:alpha:]", capitalized_letter)
}

#' @rdname capitalize
#' @export

is_capital <- function(x) {
  if (!is.character(x)) {return(rep(NA, length(x)))}

  x     <- suppressWarnings(do.call(rbind, strsplit(x, "")))
  caps  <- stringi::stri_trans_toupper(x)
  lower <- stringi::stri_trans_tolower(x)

  result                <- matrix(nrow = nrow(x), ncol = ncol(x))
  result[]              <- x == caps
  result[caps == lower] <- NA

  apply(result, 1, lgl_collapse)
}

#' @rdname capitalize
#' @export

is_capitalized <- function(x) {
  first_letter <- stringi::stri_extract_first_regex(x, "[:alpha:]")
  is_capital(first_letter)
}

lgl_collapse <- function(x) {
  if (any(is.na(x)))  {return(NA)}
  if (all(x))         {return(TRUE)}
  if (all(!x))        {return(FALSE)}
  NA
}
