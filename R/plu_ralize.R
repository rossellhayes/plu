#' Pluralize a word
#'
#' @param word A character vector of English words to be pluralized
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
#' @return The character vector `word` pluralized
#'
#' @seealso [plu::ral()] to pluralize an English phrase based on a condition
#'
#' @source Irregular plurals list adapted from [Automatically Generated
#' Inflection Database (AGID)](https://github.com/en-wl/wordlist/tree/master/agid)
#'
#' Copyright 2000-2014 by Kevin Atkinson
#'
#' Permission to use, copy, modify, distribute and sell this database,
#' the associated scripts, the output created form the scripts and its
#' documentation for any purpose is hereby granted without fee,
#' provided that the above copyright notice appears in all copies and
#' that both that copyright notice and this permission notice appear in
#' supporting documentation. Kevin Atkinson makes no representations
#' about the suitability of this array for any purpose. It is provided
#' "as is" without express or implied warranty.
#'
#' @importFrom rlang %||%
#' @export
#'
#' @example examples/plu_ralize.R

plu_ralize <- function(
  word, irregulars = c("moderate", "conservative", "liberal", "none")
) {
  irregulars <- getOption("plu.irregulars") %||% match.arg(irregulars)

  dictionary <- switch(
    irregulars,
    moderate     = moderate_list,
    conservative = conservative_list,
    liberal      = liberal_list,
    none         = data.frame(singular = character(0), plural = character(0))
  )

  todo <- rep(TRUE, length(word))

  irreg <- todo & word %in% dictionary$singular
  word[irreg] <- dictionary$plural[match(word[irreg], dictionary$singular)]
  todo <- todo & !irreg

  irreg_upper <- todo & stringr::str_to_title(word) == word &
    tolower(word) %in% dictionary$singular
  word[irreg_upper] <- stringr::str_to_title(
    dictionary$plural[match(tolower(word[irreg_upper]), dictionary$singular)]
  )
  todo <- todo & !irreg_upper

  xy <- todo & grepl("y$", word) &
    !(grepl("[AEIOUaeiou]y$", word) & !grepl("[Qq][Uu]y$", word))
  word[xy] <- gsub("y$", "ies", word[xy])
  todo <- todo & !xy

  xs <- todo & grepl("([JSXZjsxz]|[CScs][Hh])$", word)
  word[xs] <- paste0(word[xs], "es")
  todo <- todo & !xs

  word[todo] <- paste0(word[todo], "s")

  word
}

#' @rdname plu_ralize
#' @export

ralize <- plu_ralize
