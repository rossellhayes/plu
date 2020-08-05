#' Pluralize a word
#'
#' @param x A character vector of English words to be pluralized
#' @param irregulars What level of irregularity to use in pluralization.
#'     `"moderate"` uses the most common pluralization.
#'     `"conservative"` uses the most common irregular plural if one exists,
#'     even if a regular plural is more common.
#'     `"liberal"` uses a regular plural if it exists, even if an irregular
#'     plural is more common.
#'     `"none"` attempts to apply regular noun pluralization rules to all words.
#'     Defaults to `"moderate"`.
#'     The default can be changed by setting `options(plu.irregulars)`.
#'     See examples.
#'
#' @return The character vector `x` pluralized
#'
#' @seealso [plu::ral()] to pluralize an English phrase based on a condition
#'
#' @source Irregular plurals list adapted from [Automatically Generated
#' Inflection Database (AGID)](https://github.com/en-wl/wordlist/tree/master/agid)
#'
#'   See `system.file("COPYRIGHTS", package = "plu")` for more details.
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
  if (!length(x))       return(character(0))
  if (!is.character(x)) stop("`x` must be a character vector")

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

  todo <- rep(TRUE, length(x))

  irreg    <- todo & x %in% dict$singular
  x[irreg] <- dict$plural[match(x[irreg], dict$singular)]
  todo     <- todo & !irreg

  irreg_upper    <- todo & tosentence(x) == x & tolower(x) %in% dict$singular
  x[irreg_upper] <- tosentence(
    dict$plural[match(tolower(x[irreg_upper]), dict$singular)]
  )
  todo <- todo & !irreg_upper

  xy    <- todo & grepl("[^AaEeIiOoUu]y$|[Qq][Uu]y$", x)
  x[xy] <- gsub("y$", "ies", x[xy])
  todo  <- todo & !xy

  xs    <- todo & grepl("[JSXZjsxz]$|[CScs][Hh]$", x)
  x[xs] <- paste0(x[xs], "es")
  todo  <- todo & !xs

  x[todo] <- paste0(x[todo], "s")

  x
}

#' @rdname plu_ralize
#' @export

ralize <- plu_ralize
