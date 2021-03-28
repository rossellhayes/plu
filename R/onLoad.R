.onLoad <- function(libname, pkgname) {
  backports::import(pkgname, c("anyNA", "isTRUE", "isFALSE"))
}
