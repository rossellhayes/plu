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
#' @param syndeton \lifecycle{deprecated} Whether to place the conjunction
#'     before the `"last"` list items, between `"all"` list items, or
#'     between `"none"`.
#'     Defaults to `"last"`.
#'
#'     This argument is deprecated. You should set `sep` and `conj` explicitly
#'     instead of using `syndeton`.
#' @param fn \lifecycle{deprecated} A function to apply to all items in
#'     the list.
#' @param ... \lifecycle{deprecated} Additional arguments to `fn`.
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
  if (!length(x)) {return(character(0))}

  sep  <- validate_sep(sep)
  conj <- validate_sep(conj)

  if (length(oxford) != 1) {stop("`oxford` must be length one")}
  if (!is_t_or_f(oxford))  {stop("`oxford` must be TRUE or FALSE")}

  if (lifecycle::is_present(fn)) {
    lifecycle::deprecate_warn("1.2.0", paste0(sys.call()[1], "(fn = )"))

    x <- lapply(x, get_fun(fn), ...)
  }

  if (lifecycle::is_present(syndeton)) {
    lifecycle::deprecate_warn("1.2.0", paste0(sys.call()[1], "(syndeton = )"))

    if (!syndeton %in% c("last", "all", "none")) {
      stop('`syndeton` must be `NULL` or one of "last", "all", or "none".')
    }

    if (syndeton == "all")              {sep  <- conj}
    if (syndeton %in% c("all", "none")) {conj <- ""}
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

  if (length(sep) > 1)    {
    stop("`", deparse(substitute(sep)), "` must be length one")
  }

  if (!is.character(sep)) {
    stop("`", deparse(substitute(sep)), "` must be a character string")
  }

  sep
}
