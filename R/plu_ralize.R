#' Pluralize a word
#'
#' @param x A character vector of English words to be pluralized
#' @inheritParams plu_ral
#'
#' @return The character vector `x` pluralized
#'
#' @seealso [plu_ral()] to pluralize an English phrase based on a condition
#' @inheritSection plu_ral Irregular plurals
#'
#' @source Irregular plurals list adapted from the Automatically Generated
#' Inflection Database (AGID).
#'
#'   See [plu-package] for more details.
#'
#' @export
#'
#' @example examples/plu_ralize.R

plu_ralize <- function(
  x,
  irregulars = getOption(
    "plu.irregulars", c("moderate", "conservative", "liberal", "none")
  )
) {
  if (!length(x)) return(character(0))
  assert_type(x, "character")

  irregulars <- match.arg(
    irregulars, c("moderate", "conservative", "liberal", "none")
  )

  dict <- switch(
    irregulars,
    moderate     = moderate_list,
    conservative = conservative_list,
    liberal      = liberal_list,
    none         = data.frame(singular = character(0), plural = character(0))
  )

  result <- x[NA]
  result[!str_detect(x, "[A-Za-z0-9]$")] <- x[!str_detect(x, "[A-Za-z0-9]$")]

  irreg <- match(x[is.na(result)], dict$singular)
  result[is.na(result)] <- dict$plural[irreg]

  result[is.na(result)] <- paste0(
    str_replace_all(x[is.na(result)], c(
      "(?<=[^AaEeIiOoUu]|[Qq][Uu])y$" = "ie",
      "(?<=[JjSsXxZz]|[CScs][Hh])$" = "e"
    )),
    "s"
  )

  result
}

#' @rdname plu_ralize
#' @export

ralize <- plu_ralize
