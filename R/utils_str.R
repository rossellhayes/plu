str_to_sentence <- function(x) {
  gsub("^(.*?)(\\p{L})(.*)$", "\\1\\U\\2\\E\\3", x, perl = TRUE)
}

