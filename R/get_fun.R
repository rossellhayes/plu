#' Find a function
#'
#' @param fn A function name, either a character string or an unquoted
#'   function name, with or without \link[base:ns-dblcolon]{colons}.
#' @param default If `fn` is [`NULL`], the `default` function is returned.
#'   Defaults to [identity()].
#'
#' @return A function
#' @export
#'
#' @example examples/get_fun.R

get_fun <- function(fn, default = identity) {
  fn_input <- rlang::as_label(rlang::enexpr(fn))

  if (fn_input == "default") {
    fn_input <- deparse(substitute(default, env = sys.frame(-1)))
  } else if (fn_input == "n_fn") {
    fn_input <- deparse(substitute(n_fn,    env = sys.frame(-1)))
  }

  env <- rlang::current_env()

  if (try_true(is.null(fn))) {return(get_fun(default))}

  if (try_true(is.character(fn))) {
    fn_input <- fn

    if (grepl("::", fn)) {
      package  <- gsub("::.*$", "", fn)
      fn       <- gsub("^.*::", "", fn)
      env      <- getNamespace(package)
    }
  }

  tryCatch(
    suppressWarnings(rlang::as_function(fn, env = env)),
    error = function(e) {
      rlang::abort(
        paste0("Could not find the function `", fn_input, "`")
      )
    }
  )
}

try_true <- function(expr) {
  suppressWarnings(isTRUE(try(expr, silent = TRUE)))
}
