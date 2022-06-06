plu_nge <- function(x, ends = FALSE) {
  x <- str_replace_all(x, " +", " ")

  if (ends) {
    x <- str_replace(x, "^ +", "")
    x <- str_replace(x, " +$", "")
  }

  x
}
