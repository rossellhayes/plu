# @staticimports pkg:stringstatic
#   str_replace

#' Capitalization
#'
#' `capitalize()` returns a character vector `x` with the first
#' alphabetic character replaced with a capital form (if one exists).
#'
#' `is_capital()` returns [`TRUE`] if all characters are capital, [`FALSE`] if
#' all characters are lowercase, and [`NA`] if characters are mixed case or any
#' characters are caseless (e.g. numbers, punctuation marks, characters from a
#' unicase language like Arabic, Chinese or Hindi).
#'
#' `is_capitalized()` returns [`TRUE`] if the first alphabetic character in a
#' string is capital, [`FALSE`] if the first alphabetic character is lowercase,
#' and [`NA`] if there are no alphabetic characters.
#'
#' @param x A character vector.
#' @param strict If strict is `TRUE`, `is_capital()` and `is_capitalized()`
#'   return `FALSE` instead of `NA` when characters are neither capital nor
#'   lowercase.
#'   Defualts to `FALSE`.
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
  str_replace(x, "^(.*?)(\\p{L})(.*)$", "\\1\\U\\2\\E\\3")
}

#' @rdname capitalize
#' @export

plu_capitalize <- capitalize

#' @rdname capitalize
#' @export

is_capital <- function(x, strict = FALSE) {
  strict_na <- ifelse(strict, FALSE, NA)

  if (!is.character(x)) {
    result      <- rep.int(strict_na, length(x))
    dim(result) <- dim(x)
    return(result)
  }

  split <- plu_split(x, "")
  chars <- unique(c(split))
  match <- match(split, chars)

  caps    <- split
  caps[]  <- toupper(chars)[match]
  lower   <- split
  lower[] <- tolower(chars)[match]

  result                <- split == caps
  dim(result)           <- dim(split)
  result[caps == lower] <- strict_na

  result <- lapply(
    seq_len(ncol(result)), function(i) {result[, i][split[, i] != ""]}
  )
  result <- vapply(
    result, ifelse(strict, lgl_collapse_strict, lgl_collapse), logical(1)
  )

  dim(result)     <- dim(x)
  result[x == ""] <- strict_na
  result
}

# @staticimports pkg:stringstatic
#   str_replace

#' @rdname capitalize
#' @export

is_capitalized <- function(x, strict = FALSE) {
  first_letter <- str_replace(x, "^.*?(\\p{L}).*$", "\\1")
  is_capital(first_letter, strict = strict)
}

lgl_collapse <- function(x) {
  if (any(is.na(x))) {return(NA)}
  if (all(x))   {return(TRUE)}
  if (all(!x))  {return(FALSE)}
  NA
}

lgl_collapse_strict <- function(x) {
  length(x) && !anyNA(x) && all(x)
}
