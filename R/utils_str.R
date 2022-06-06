str_detect <- function(string, pattern) {
  grepl(pattern = paste0("(?s)", pattern), x = string, perl = TRUE)
}

str_replace_all <- function(string, pattern, replacement) {
  if (!is.null(names(pattern))) {
    for (i in seq_along(pattern)) {
      string <- gsub(
        pattern = names(pattern)[[i]],
        replacement = pattern[[i]],
        x = string,
        perl = TRUE
      )
    }

    return(string)
  }

  gsub(pattern = pattern, replacement = replacement, x = string, perl = TRUE)
}

str_to_sentence <- function(string) {
  sub("^(.*?)(\\p{L})(.*)$", "\\1\\U\\2\\E\\3", string, perl = TRUE)
}
