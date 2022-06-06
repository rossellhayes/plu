plu_ralize <- function(x) {
  x <- as.character(x)
  x <- sub("(?<=[^AaEeIiOoUu]|[Qq][Uu])y$", "ie", x, perl = TRUE)
  x <- sub("(?<=[JjSsXxZz]|[CScs][Hh])$", "e", x, perl = TRUE)
  x <- sub("(?<=\\p{L}|\\p{N})$", "s", x, perl = TRUE)
  x
}
