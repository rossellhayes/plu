tosentence <- function(x) {
  gsub("^(.*?)(\\p{L})(.*)$", "\\1\\U\\2\\L\\3", x, perl = TRUE)
}
