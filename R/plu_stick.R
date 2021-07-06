#' Collapse a vector into a natural language string
#'
#' @param x A [character] vector (or a vector coercible to character).
#' @param sep A [character] to place between list items. Defaults to `", "`
#' @param conj A [character] to place between the penultimate and last
#'     list items.
#'     Defaults to `" and "`.
#'     If [`NULL`], `sep` is used.
#' @param oxford A [logical] indicating whether to place `sep` before `conj`
#'     (x, y, and z) or not (x, y and z) in lists of length three or more.
#'     Defaults to `FALSE`.
#'     The default can be changed by setting `options(plu.oxford_comma)`.
#' @param syndeton \lifecycle{deprecated}
#' @param fn \lifecycle{deprecated}
#' @param ... \lifecycle{deprecated}
#'
#' @return A character vector of length 1.
#' @export
#'
#' @example examples/plu_stick.R

plu_stick <- function(
  x, sep = ", ", conj = " and ",
  oxford   = getOption("plu.oxford_comma", FALSE),
  syndeton = lifecycle::deprecated(),
  fn = lifecycle::deprecated(), ...
) {
  lifecycle::deprecate_warn(
    "0.2.2",
    paste0(sys.call()[1], "()"),
    details =
      "Please use `knitr::combine_words()` or `glue::glue_collapse()` instead."
  )

  if (!length(x)) {return(character(0))}

  sep  <- validate_sep(sep)
  conj <- validate_sep(conj)

  assert_length_1(oxford)
  assert_t_or_f(oxford)

  if (lifecycle::is_present(fn)) {
    lifecycle::deprecate_stop(
      "0.2.0",
      paste0(sys.call()[1], "(fn = )"),
      details = paste0(
        "Please apply a function to `x` before passing it to `",
        sys.call()[1], "()`."
      )
    )
  }

  if (lifecycle::is_present(syndeton)) {
    lifecycle::deprecate_stop(
      "0.2.0",
      paste0(sys.call()[1], "(syndeton = )"),
      details = "Please set `sep` and `conj` explicitly."
    )
  }

  phrase                       <- character(length(x) * 2 - 1)
  phrase[seq_along(x) * 2 - 1] <- x

  if (oxford && length(x) > 2 && !identical(sep, conj)) {
    phrase[length(x) * 2 - 2] <- plu_nge(paste0(sep, conj))
  } else {
    phrase[length(x) * 2 - 2] <- conj
  }

  phrase[phrase == "" & seq_along(phrase) %% 2 == 0] <- sep

  paste0(phrase, collapse = "")
}

#' @rdname plu_stick
#' @export

stick <- plu_stick

validate_sep <- function(sep) {
  if (is.null(sep)) {return("")}
  assert_length_1(sep, code(deparse(substitute(sep))))
  assert_type(sep, "character", code(deparse(substitute(sep))))
  sep
}
