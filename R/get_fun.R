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
  if (try_true(is.function(fn))) {return(fn)}
  if (try_true(is.null(fn)))     {return(get_fun(default))}

  env <- parent.frame()

  if (try_true(is.character(fn))) {
    fn_input <- fn

    if (str_detect(fn_input, "::")) {
      fn       <- str_replace(fn_input, "^.*::", "")
      package  <- str_replace(fn_input, "::.*$", "")
      env      <- getNamespace(package)
    }
  } else {
    fn_input <- deparse(substitute(fn))
    fn       <- fn_input
  }

  if (fn_input == "default") {
    fn       <- deparse(substitute(default, env = env))
    fn_input <- fn
  } else if (fn_input == "n_fn") {
    fn       <- deparse(substitute(n_fn, env = env))
    fn_input <- fn
  }

  tryCatch(
    suppressWarnings(get(fn, envir = env, mode = "function")),
    error = function(e) {error("Could not find the function ", code(fn_input))}
  )
}

try_true <- function(expr) {
  suppressWarnings(isTRUE(try(expr, silent = TRUE)))
}
