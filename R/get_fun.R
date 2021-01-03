#' Find a function
#'
#' @param fn A function name, either a character string or an unquoted
#'   function name, with or without \link[base:ns-dblcolon]{colons}.
#' @param default If `fn` is [`NULL`], the `default` function is returned.
#'   Defaults to [identity()].
#'   If `default` is not the name of a function (e.g. [`FALSE`] or [`NULL`]),
#'   an error will be produced when `fn` is [`NULL`].
#'
#' @return A function
#' @export
#'
#' @example examples/get_fun.R

get_fun <- function(fn, default = identity) {
  fn_input <- substitute(fn, env = sys.frame(-1))

  if (deparse(fn_input) == "fn") {fn_input <- substitute(fn)}

  if (try_true(is.null(eval(fn_input)))) {
    fn       <- default
    fn_input <- default
  }

  if (try_true(is.character(eval(fn_input)))) {
    if (grepl("::", fn)) {
      package <- gsub("::.*$", "", fn)
      fn      <- gsub("^.*::", "", fn)
      fn      <- try(get(fn, envir = getNamespace(package)), silent = TRUE)
    } else {
      fn      <- try(get(fn), silent = TRUE)
    }
  }

  if (try_true(is.function(fn))) {return(fn)}

  stop("Couldn't find a function named ", deparse(fn_input), call. = FALSE)
}

try_true <- function(expr) {
  isTRUE(try(expr, silent = TRUE))
}
