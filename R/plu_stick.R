#' Collapse a vector into a natural language string
#'
#' \lifecycle{deprecated} This function has been deprecated in favor of
#' [and::and()], [knitr::combine_words()] or [glue::glue_collapse()].
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
  lifecycle::deprecate_stop(
    "0.2.2",
    paste0(sys.call()[1], "()"),
    details =
      "Please use `and::and()`, knitr::combine_words()` or `glue::glue_collapse()` instead."
  )
}

#' @rdname plu_stick
#' @export

stick <- plu_stick
