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

  todo <- grepl("[A-Za-z0-9]$", x)

  irreg              <- match(x[todo], dict$singular)
  irreg_na           <- is.na(irreg)
  x[todo][!irreg_na] <- dict$plural[irreg[!irreg_na]]
  todo[todo]         <- irreg_na

  upper              <- tosentence(x[todo]) == x[todo]
  irreg              <- upper[NA]
  irreg[upper]       <- match(tolower(x[todo][upper]), dict$singular)
  irreg_na           <- is.na(irreg)
  x[todo][!irreg_na] <- tosentence(dict$plural[irreg[!irreg_na]])
  todo[todo]         <- irreg_na

  xy          <- grepl("([^AaEeIiOoUu]|[Qq][Uu])y$", x[todo])
  x[todo][xy] <- gsub("y$", "ies", x[todo][xy])
  todo[todo]  <- !xy

  xs          <- grepl("([JSXZjsxz]|[CScs][Hh])$", x[todo])
  x[todo][xs] <- paste0(x[todo][xs], "es")
  todo[todo]  <- !xs

  x[todo] <- paste0(x[todo], "s")

  x
}

#' @rdname plu_ralize
#' @export

ralize <- plu_ralize
