tosentence <- function(x) {
  gsub("^([a-z])", "\\U\\1", x, perl = TRUE)
}
