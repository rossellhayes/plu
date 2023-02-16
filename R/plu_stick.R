#' Deprecated functions
#'
#' \lifecycle{deprecated} This function has been deprecated in favor of
#' [and::and()], [knitr::combine_words()] or [glue::glue_collapse()].
#'
#' @param ... \lifecycle{deprecated}
#'
#' @return A [deprecation error][lifecycle::deprecate_stop()].
#' @export

plu_stick <- function(...) {
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
