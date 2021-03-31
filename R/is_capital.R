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
  gsub("^(.*?)(\\p{L})(.*)$", "\\1\\U\\2\\E\\3", x, perl = TRUE)
}

#' @rdname capitalize
#' @export

plu_capitalize <- capitalize

#' @rdname capitalize
#' @export

is_capital <- function(x, strict = FALSE) {
  strict_na <- ifelse(strict, FALSE, NA)

  if (!is.character(x)) {return(rep.int(strict_na, length(x)))}

  split <- plu_split(x, "")
  chars <- unique(c(split))
  match <- match(split, chars)

  caps   <- split
  caps[] <- vapply(
    chars, function(x) {gsub("(.*)", "\\U\\1", x, perl = TRUE)}, character(1)
  )[match]

  lower   <- split
  lower[] <- vapply(
    chars, function(x) {gsub("(.*)", "\\L\\1", x, perl = TRUE)}, character(1)
  )[match]

  result                <- split
  mode(result)          <- "logical"
  result[]              <- split == caps
  result[caps == lower] <- strict_na

  result <- lapply(
    seq_len(ncol(result)), function(i) {result[, i][split[, i] != ""]}
  )
  result <- vapply(
    result, ifelse(strict, lgl_collapse_strict, lgl_collapse), logical(1)
  )

  out          <- x
  mode(out)    <- "logical"
  out          <- result
  out[x == ""] <- strict_na
  out
}

#' @rdname capitalize
#' @export

is_capitalized <- function(x, strict = FALSE) {
  first_letter <- gsub("^.*?(\\p{L}).*$", "\\1", x, perl = TRUE)
  is_capital(first_letter, strict = strict)
}

lgl_collapse <- function(x) {
  if (anyNA(x)) {return(NA)}
  if (all(x))   {return(TRUE)}
  if (all(!x))  {return(FALSE)}
  NA
}

lgl_collapse_strict <- function(x) {
  length(x) && !anyNA(x) && all(x)
}
